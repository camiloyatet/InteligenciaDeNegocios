### Librerias y Funciones. ----
rm(list = ls(all=T))
source("~/CamiloYate/Funciones.r")
pqts <- c("data.table", "tidyverse", "tictoc","lubridate", "readxl", "RODBC", "caret", "agricolae")
Loadpkg(pqts)

fecha_foco = as.Date("2019-10-31")

tic("Preparacion de ina Informacion")
### Bases de datos iniciales ----

Regionales <- read_excel("BD/Regionales.xlsx")
PLUS <- read_excel("BD/PLU.xlsx") 

hojas <- excel_sheets("BD/Originales/TipologiaPrecios.xlsx")

Listados <- do.call("bind_rows",
                  lapply(hojas, FUN = function(hoja) {
                    read_excel("BD/Originales/TipologiaPrecios.xlsx", sheet = hoja, .name_repair = Limpiar.Cadenas) %>% 
                      mutate(Regional=hoja)
                    }
                  )
                  ) %>% 
  select(prod_id=plucolsubsidio, Tipologia=tipologiacolsubsidio) 

Activos <- Listados %>% select(prod_id) %>% distinct()
tipologia <- Listados %>% group_by(prod_id) %>% filter(row_number()==1)

### Cargue de Datos Transaccionales ----

tic("Cargue Transacciones")
carpeta <- "../1. Aplicacion de Consumo/Datos/POS"
temp <- paste0(carpeta, "/", list.files(carpeta, pattern = "+.txt", recursive = F)); temp
prim <-1

Union <- do.call("bind_rows",
                 lapply(temp, FUN = function(file) {
                   fread(file, select = c(1:19), na.strings = c("")) %>% 
                     filter(!is.na(Id_Cliente), !is.na(prod_id)) %>% 
                     mutate(prod_id=as.numeric(prod_id),
                            Fecha=as.Date(as.character(Fecha), "%Y%m%d"),
                            Venta_Bruta=as.numeric(gsub("\\D","",Venta_Bruta)),
                            Venta_Neta=as.numeric(gsub("\\D","",Venta_Neta)),
                            Cantidad_Productos=as.numeric(gsub("\\D","",Cantidad_Productos))
                     ) %>% 
                     inner_join(Activos, by="prod_id") %>% 
                     left_join(Regionales, by=c("Id_Sucursal"="CenLog")) %>% 
                     group_by(Fecha, prod_id, prod_nombre, Regional) %>% 
                     summarise(VentaBruta=sum(Venta_Bruta, na.rm = T),
                               VentaNeta=sum(Venta_Neta, na.rm = T),
                               Cantidades=sum(Cantidad_Productos, na.rm = T)) %>% 
                     ungroup() %>% 
                     mutate(PrecioUnitario_Bruta=VentaBruta/Cantidades,
                            PrecioUnitario_Neta=VentaNeta/Cantidades,
                            DiaSem=as.factor(weekdays(Fecha)))
                 }
                 )
)
toc()

### Calculo RFM por Producto ----
tic("Calculo RFM")
RFMPRods<-Union %>% 
  select(prod_id, Fecha, VentaBruta) %>%
  mutate(Recencia=as.numeric(difftime(fecha_foco,Fecha,units="days"))) %>%
  group_by(prod_id) %>%
  summarise(Recencia = min(Recencia),
            Frecuencia=n_distinct(Fecha),
            Monto=sum(VentaBruta, na.rm = T)/Frecuencia
  ) %>%
  mutate(R_Score=Hmisc::cut2(Recencia, g = 5),
         F_Score=Hmisc::cut2(Frecuencia, g = 5),
         M_Score=Hmisc::cut2(Monto, g = 5)) 

levels(RFMPRods$R_Score)<-seq(5,1, by = -1)
levels(RFMPRods$F_Score)<-seq(1,5)
levels(RFMPRods$M_Score)<-seq(1,5)
toc()
### Listados de Medicamentos ----
tic("Listado de medicamento")
Vademecum <- tipologia %>% 
  left_join(PLUS, by = "prod_id") %>% 
  arrange(prod_id) %>% 
  group_by(prod_id) %>% 
  filter(row_number()==1) %>% 
  select(prod_id, prod_nombre=prod_nombre, Tipologia, Patologia=`Patología/Linea`, categoria=Categoría)
toc()

### Clusters ----
tic("Calculo de Clusters")
aux1 <- RFMPRods %>% 
  filter(!is.na(Recencia)) %>% 
  left_join(tipologia, by = "prod_id") %>% 
  select(prod_id:Monto, Tipologia)

aux2 <- as.data.frame(predict(dummyVars(~ ., data = aux1), newdata = aux1)) %>% 
  mutate_all(as.numeric) %>% 
  na.omit()

set.seed(31415)
pca <- prcomp(aux2[-1],  scale = T, center = T)
km_clusters <- kmeans(pca$x,6)
aux2$Cluster=as.factor(km_clusters$cluster)

Clusters <- aux2 %>% 
  mutate(Cluster=case_when(Cluster==1 ~ "E",
                            Cluster==2 ~ "B",
                            Cluster==3 ~ "D",
                            Cluster==4 ~ "AA",
                            Cluster==5 ~ "A",
                            Cluster==6 ~ "C"
                           )
         ) %>% 
  select(prod_id, Cluster)

rm(aux1, aux2, pca, km_clusters)
toc()
### Costos ----

tic("Cargue de bases de Costos")

hojas <- excel_sheets("BD/Originales/PRECIOS POR REGIONAL OCTUBRE 2019.xlsx")

costos <- do.call("bind_rows",
                   lapply(hojas, FUN = function(hoja) {
                     read_excel("BD/Originales/PRECIOS POR REGIONAL OCTUBRE 2019.xlsx", sheet = hoja) %>% 
                       mutate(Regional=hoja)
                     }
                     )
                   ) %>% 
  filter(Regional!="CADENA") %>% 
  mutate(COSTO=ifelse(is.na(COSTO), PRECIO ,COSTO)) %>% 
  group_by(prod_id=PLU, Regional) %>% 
  summarise(COST=round(median(COSTO),0))
  
toc()

### Numero de Semana ----
tic("Consecutivos Semanas")
Fecha= seq(min(Union$Fecha, na.rm = T),max(Union$Fecha, na.rm = T), by = '1 week')
WEEK_NO = seq(1:length(Fecha))
semanas = data.frame(Fecha, WEEK_NO)
rm(Fecha, WEEK_NO)
toc()
### Consolidacion ----

tic("Consolidacion Base")
data <- Union %>% 
  mutate(Promocion=ifelse(((VentaBruta-VentaNeta)/VentaBruta)>0.1, 1,0),
         Fecha=floor_date(Fecha, "weeks", week_start = 7)) %>% 
  group_by(prod_id, Regional, Fecha) %>% 
  summarise(UNITS=sum(Cantidades,na.rm=T),
            PRICE=mean(PrecioUnitario_Bruta,na.rm=T),
            BASE_PRICE=mean(PrecioUnitario_Neta,na.rm=T),
            FEATURE=max(Promocion)) %>% 
  ungroup() %>% group_by(prod_id, Regional) %>% 
  mutate(aux_units=Hmisc::cut2(UNITS, g = 4),
         units_factor=as.factor(as.integer(as.factor(aux_units))),
         DISPLAY=0,
         aux_precio=max(BASE_PRICE, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(RFMPRods, by = "prod_id") %>% 
  left_join(Vademecum, by = "prod_id") %>% 
  left_join(Clusters, by = "prod_id") %>% 
  left_join(costos, by = c("prod_id", "Regional")) %>% 
  left_join(semanas, by = "Fecha") %>% 
  group_by(Fecha, categoria) %>% 
  mutate(Tmp1=sum(PRICE*UNITS)) %>% 
  ungroup() %>% group_by(Fecha) %>% 
  mutate(PCT_CAT=Tmp1/sum(PRICE*UNITS)) %>% 
  mutate(PCT_CAT=ifelse(is.nan(PCT_CAT), 0, PCT_CAT)) %>% 
  ungroup() %>% group_by(Fecha, Patologia) %>% 
  mutate(Tmp1=sum(PRICE*UNITS)) %>% 
  ungroup() %>% group_by(Fecha) %>% 
  mutate(PCT_PAT=Tmp1/sum(PRICE*UNITS)) %>% 
  mutate(PCT_PAT=ifelse(is.nan(PCT_PAT), 0, PCT_PAT)) %>% 
  ungroup() %>% group_by(Fecha, Tipologia) %>% 
  mutate(Tmp1=sum(PRICE*UNITS)) %>% 
  ungroup() %>% group_by(Fecha) %>% 
  mutate(PCT_TIP=Tmp1/sum(PRICE*UNITS)) %>% 
  mutate(PCT_TIP=ifelse(is.nan(PCT_TIP), 0, PCT_TIP)) %>%
  ungroup() %>% 
  mutate(COST=ifelse(COST>aux_precio, aux_precio*0.85, COST)) %>% 
  select(prod_id, prod_nombre , Recencia, Frecuencia, Monto, R_Score, F_Score, M_Score, categoria, Patologia, Tipologia, Cluster, Regional, Fecha, BASE_PRICE, 
         PRICE, UNITS, FEATURE, COST, WEEK_NO, units_factor, DISPLAY, PCT_CAT, PCT_PAT, PCT_TIP)
toc()
### Datos de Precios externos ----
tic("Datos Competencia")
# Comerciales
archivo <- "BD/Originales/SHOPPING COMPLETO 2018-2019.xlsx"
lista_hojas <- excel_sheets(archivo)

#Centro:
col_nombres <- c('Mes','prod_id','prod_nombre','agrupacion','proveedor','PrecioOlimpica','ObservacionOlimpica','PrecioRebaja','ObservacionRebaja','PrecioCafam',
                 'ObservacionCafam','PrecioCruzVerde','ObservacionCruzVerde','PrecioFarmatodo','ObservacionesFarmatodo',
                 'PrecioAlemana','ObservacionesAlemana')
col_tipos <- c('date','numeric','text','text','text','numeric','skip','numeric','skip','numeric','skip','numeric','skip','numeric','skip','numeric','skip')

centro <- read_excel(archivo, sheet = lista_hojas[1],  col_names = col_nombres, col_types = col_tipos, skip = 1) %>% 
  mutate(Regional="Centro")

#Antioquia
col_nombres <- c('Mes','prod_id','prod_nombre','agrupacion','proveedor','PrecioRebaja','ObservacionRebaja','PrecioPasteur','Observacion','PrecioCafam',
                 'ObservacionCafam','PrecioAlemana','ObservacionesAlemana','PrecioOlimpica','ObservacionOlimpica','PrecioJunin','ObservacionJunin')
col_tipos <- c('date','numeric','text','text','text','numeric','skip','numeric','skip','numeric','skip','numeric','skip','numeric','skip','numeric','skip')

antioquia <- read_excel(archivo, sheet = lista_hojas[2],  col_names = col_nombres, col_types = col_tipos, skip = 1) %>% 
  mutate(Regional="Antioquia")

#Costa
col_nombres <- c('Mes','prod_id','prod_nombre','agrupacion','proveedor','PrecioInglesa','ObservacionInglesa','PrecioFarmavida','ObservacionFarmavida',
                 'PrecioOlimpica','ObservacionOlimpica','PrecioBotica','ObservacionBotica','PrecioRebaja','ObservacionRebaja','PrecioPasteur','Observacion')
col_tipos <- c('date','numeric','text','text','text','numeric','skip','numeric','skip','numeric','skip','numeric','skip','numeric','skip','numeric','skip')

costa <- read_excel(archivo, sheet = lista_hojas[3],  col_names = col_nombres, col_types = col_tipos, skip = 1) %>% 
  mutate(Regional="Costa")

#Valle
col_nombres <- c('Mes','prod_id','prod_nombre','agrupacion','proveedor','PrecioMultidrogas','ObservacionMD','PrecioComfandi','ObservacionCOM','PrecioCruzVerde',
                 'ObservacionCruzVerde','PrecioRebaja','ObservacionRebaja','PrecioComfenalco','Observacion','PrecioSanJorge','ObservacionSanJorge')
col_tipos <- c('date','numeric','text','text','text','numeric','skip','numeric','skip','numeric','skip','numeric','skip','numeric','skip','numeric','skip')

valle <- read_excel(archivo, sheet = lista_hojas[4],  col_names = col_nombres, col_types = col_tipos, skip = 1) %>% 
  mutate(Regional="Valle")

#Eje Cafetero
col_nombres <- c('Mes','prod_id','prod_nombre','agrupacion','proveedor','PrecioOlimpica','ObservacionOlimpica','PrecioPasteur','Observacion','PrecioCafam',
                 'ObservacionCafam','PrecioRebaja','ObservacionRebaja','PrecioMultidrogas','ObservacionMD','PrecioProfamiliar','ObservacionMultifamiliar')
col_tipos <- c('date','numeric','text','text','text','numeric','skip','numeric','skip','numeric','skip','numeric','skip','numeric','skip','numeric','skip')

eje <- read_excel(archivo, sheet = lista_hojas[5],  col_names = col_nombres, col_types = col_tipos, skip = 1) %>% 
  mutate(Regional="Eje Cafetero")

Precios_Externos <- bind_rows(centro, antioquia, costa, valle, eje)

# Regulados

archivo <- "BD/Originales/SHOPPING REGULADOS MAYO 2019.xlsx"
lista_hojas <- excel_sheets(archivo)

col_nombres <- c('prod_id','prod_nombre','proveedor','agrupacion','Tipologia','Regulacion','PreReguladoRegulado','Costo','PreReguladoColsubsidio','Diferencial',
                 'PreReguladoPasteur','PreReguladoFarmatodo','PreReguladoCafam','PreReguladoAlemana','PreReguladoRebaja','PreReguladoLocatel','PreReguladoCruzVerde',
                 'PromedioRegulado','Observacion','Validacion','PreReguladoSugerido','Margen','DifPreRegulado','Revisar')
col_tipos <- c('numeric','skip','skip','skip','text','skip','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric',
               'numeric','numeric','skip','skip','numeric','numeric','skip','skip')

regulados <- read_excel(archivo, sheet = lista_hojas[1],  col_names = col_nombres, col_types = col_tipos, skip = 1)

# Depuración

Precios <- Precios_Externos %>% 
  left_join(regulados, by = c("prod_id")) %>% 
  mutate(Periodo=year(Mes)*100+month(Mes))

rm(centro, antioquia, costa, valle, eje, col_nombres, col_tipos, archivo, Precios_Externos, regulados, lista_hojas)
toc()
### Exportacion ----
tic("Exportacion")
saveRDS(data, "DashoardElasticidades/data/DataDemanda.rds")
saveRDS(Precios, "DashoardElasticidades/data/Precios.rds")
toc()
### Eliminacion ----
rm(list = ls(all=T))
toc()
