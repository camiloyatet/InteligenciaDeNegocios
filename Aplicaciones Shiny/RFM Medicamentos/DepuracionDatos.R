rm(list = ls(all=T))
source("~/CamiloYate/Funciones.r")
### Carga e instalacion de paquetes ====
pkgs <- c("data.table", "dplyr", "RODBC", "stringr", "lubridate", "tictoc", "readxl")
Loadpkg(pkgs)

fecha_ref=as.Date("2019-06-30") # Modificar Fecha.
miss<-c("NULL","","NA","09-999999999999999", "30-99999999", "#NA", "N/A", "09-0","NO REGISTRA", "999999999999999", "99999999")

#### Autorizacion HD ----
tic("Cargue de bases de Habeas Data")
con <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//BOGAK08beimrodc/BI/Contacto/Fuentes/Autorizacion.accdb")
habeas_data <- sqlQuery(con , paste0("select id_persona from tb_autorizaciones where autorizacion='SI'")) %>% 
    mutate(NumIdPersona=as.numeric(gsub("\\D", "", id_persona)),
           Autorizado=1) %>% 
    select(-id_persona) %>% 
    distinct()
odbcCloseAll() 
toc()

#### HIjos de los Clientes - Datos por contruir, carga lenta ----

# tic("Cargue de bases de Edad de Hijos")
# 
# # Hijos
# con <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//BOGAK08beimrodc/BI/Persona/Grupo_familiar.accdb")
# hijos <- sqlQuery(con , paste0("select id_persona_afiliado as id_persona, id_persona_familiar from TB_hijo")) %>% 
#     mutate(NumIdPersona=as.numeric(gsub("\\D", "", id_persona))) %>% 
#     distinct()
# odbcCloseAll() 
# 
# # Edades
# 
# # Union
# toc()

#### Cupo de Credito ----
tic("Cargue de bases de Cupo")
con <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//BOGAK08beimrodc/BI/Tabla_Conversion/Tabla_conversion.accdb")
estados_credito <- sqlQuery(con , paste0("select * from Tb_Estado_Credito"))
odbcCloseAll() 

con <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//BOGAK08beimrodc/BI/Filial/Cupo_Credito.accdb")
CupoCredito <- sqlQuery(con , paste0("select id_persona, codigo_estado_credito from cupo_credito where año=",year(fecha_ref), " and mes=",month(fecha_ref))) %>% 
    left_join(estados_credito, by="codigo_estado_credito") %>% 
    mutate(NumIdPersona=as.numeric(gsub("\\D", "", id_persona))) %>% 
    select(NumIdPersona, EstadoCredito=estado_credito)
odbcCloseAll() 
rm(estados_credito, con)
toc()

#### Informacion de Personas ----
tic("Cargue de bases de persona")
persona <- readRDS("D:/Analitica/BaseConsolidada/Persona.rds") %>%
    ungroup() %>%
    select(NumIdPersona, FechaNacimiento, Genero, Segmento, CategoriaAfiliacion) %>%
    mutate(Edad=Calcular.Edad(FechaNacimiento, fecha_ref),
           NumIdPersona=as.numeric(NumIdPersona)) %>%
    select(NumIdPersona, Genero, Edad, Segmento, CategoriaAfiliacion) %>%
    arrange(NumIdPersona, desc(CategoriaAfiliacion)) %>%
    group_by(NumIdPersona) %>%
    filter(row_number()==1)
length(unique(persona$NumIdPersona))==dim(persona)[1]
toc()

#### Listado de Medicamentos ----
tic("Cargue Listado de Medicamentos")
listado<-read_excel("Datos/PLU.xlsx") %>% 
    arrange(prod_id) %>% 
    group_by(prod_id) %>%
    filter(row_number()==1) %>% 
    select(prod_id, categoria_list=`Categoría`, patologia_list=`Patología/Linea`)
toc()
    
#### Transacciones ----
tic("Cargue Transacciones")
carpeta <- "Datos/POS"
temp <- paste0(carpeta, "/", list.files(carpeta, pattern = "+.txt", recursive = F)); temp
prim<-length(temp)-5; temp[prim]
# prim=1

MED<-fread(temp[prim], na.strings = miss)[ ,1:19] %>% 
    filter(!is.na(Id_Cliente), !is.na(prod_id)) %>% 
    mutate(Fecha=as.Date(as.character(Fecha), "%Y%m%d"),
           Venta_Bruta=as.numeric(gsub("\\D","",Venta_Bruta)),
           Venta_Neta=as.numeric(gsub("\\D","",Venta_Neta)),
           Cantidad_Productos=as.numeric(gsub("\\D","",Cantidad_Productos)),
           PertencetuClub=as.character(PertencetuClub),
           Id_Cliente=ifelse(grepl("-", Id_Cliente),Id_Cliente, paste0("09-",Id_Cliente)),
           NumIdPersona=as.numeric(gsub(",",".",str_split(Id_Cliente, "-", simplify = T)[,2]))
           ) %>% 
    select(-c(Afiliado, Segmento, PertencetuClub)) %>% 
    filter(!is.na(NumIdPersona)) %>% 
    select(-c(Id_Cliente, Ciudad, Departamento, prod_proveedor_nit, cod_EAN))

stop <- length(temp) - 1

for (i in prim+1:stop){
    tmp<-fread(temp[i], na.strings = miss)[ ,1:19] %>% 
        filter(!is.na(Id_Cliente), !is.na(Fecha)) %>% 
        mutate(Fecha=as.Date(as.character(Fecha), "%Y%m%d"),
            Venta_Bruta=as.numeric(gsub("\\D","",Venta_Bruta)),
            Venta_Neta=as.numeric(gsub("\\D","",Venta_Neta)),
            Cantidad_Productos=as.numeric(gsub("\\D","",Cantidad_Productos)),
            Id_Cliente=ifelse(grepl("-", Id_Cliente),Id_Cliente, paste0("09-",Id_Cliente)),
            PertencetuClub=as.character(PertencetuClub),
            NumIdPersona=as.numeric(gsub(",",".", str_split(Id_Cliente, "-", simplify = T)[,2]))) %>% 
        select(-c(Afiliado, Segmento, PertencetuClub))%>% 
        filter(!is.na(NumIdPersona)) %>% 
        select(-c(Id_Cliente, Ciudad, Departamento, prod_proveedor_nit, cod_EAN))
    
    names(tmp)<-names(MED)
    MED<-rbind(MED,tmp)
    rm(tmp)
}

rm(temp)
table(year(MED$Fecha)*100+month(MED$Fecha))
toc()

fwrite(MED, "Transacciones6Meses.csv")

#### Union Total ----
tic("Union Final")
BD <- MED %>% 
    left_join(persona, by="NumIdPersona") %>% 
    left_join(habeas_data, by="NumIdPersona") %>% 
    left_join(listado, by="prod_id") %>% 
    left_join(CupoCredito, by="NumIdPersona") %>% 
    mutate(Autorizado=ifelse(is.na(Autorizado), 0,1),
           Patologia=ifelse(patologia_list %in% c("#N/A","|",NA), Patologia, patologia_list),
           Categoria=ifelse(categoria_list %in% c("#N/A","|",NA), Categoria, categoria_list),
           Patologia=ifelse(Patologia %in% c("#N/A","|",NA, "0"), "SIN PATOLOGIA", Patologia),
           Categoria=ifelse(Categoria %in% c("#N/A","|",NA, "0"), "SIN CATEGORIA", Categoria),
           Patologia=ifelse(grepl("-", Patologia),substr(Patologia, 5, length(Patologia) ),Patologia),
           Categoria=ifelse(grepl("-", Categoria),substr(Categoria, 5, length(Categoria) ),Categoria),
           EstadoCredito=ifelse(is.na(EstadoCredito), "SIN TMS", as.character(EstadoCredito)),
           Afiliado=ifelse(is.na(CategoriaAfiliacion), "No", "Si")
    ) %>% 
    mutate_at(c("Genero","Segmento","CategoriaAfiliacion"), list(~ifelse(is.na(.), "NO AFILIADO", .))) %>% 
    select(-patologia_list, -categoria_list)

table(year(BD$Fecha)*100+month(BD$Fecha))
saveRDS(BD, file = "DashboardTransacciones/data/BD.rds")

fwrite( MED %>% select(prod_id, prod_nombre) %>% distinct() %>% anti_join(listado %>% select(prod_id), by = "prod_id"), "PLUS_sin_Coincidencia.csv")
toc()

### Eliminacion de tablas auxiliares----
rm(list = ls(all=T))

