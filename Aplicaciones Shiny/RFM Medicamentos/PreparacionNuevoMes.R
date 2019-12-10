rm(list = ls(all=T))
source("C:/Users/hernyatt/Documents/CamiloYate/Funciones.r")
### Carga e instalacion de paquetes ====
pkgs <- c("data.table", "dplyr", "stringr", "tidyr", "readxl")
Loadpkg(pkgs)

#### Listado Patologia y Categoria ----

Listado <- readRDS("DashboardTransacciones/data/BD.rds") %>% 
    select(prod_id, Patologia, Categoria) %>% 
    distinct() %>%  
    mutate(Patologia=ifelse(Patologia=="SIN PATOLOGIA",NA,Patologia)) %>% 
    arrange(prod_id, Patologia) %>% 
    group_by(prod_id) %>% 
    fill(Patologia) %>% 
    arrange(prod_id) %>% 
    filter(row_number()==1) %>% 
    ungroup() %>% 
    mutate(prod_id=as.numeric(prod_id),
           Patologia=ifelse(is.na(Patologia),"SIN PATOLOGIA", Patologia))

#### Cargue y escritura de Datos ----

#=TEXTO(I2,"yyyymmdd")

archivo= "C:/Users/hernyatt/Desktop/26_Informes Compras DrogueriasJunio2019.xlsx"
col_tipos <- c("text","text", "text", "text", "text", "text","text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text")

data1 <- read_excel(archivo, sheet = 1, col_types = col_tipos)
data2 <- read_excel(archivo, sheet = 2, col_types = col_tipos)

data <- bind_rows(data1, data2) %>% filter(!is.na(Id_Cliente)) %>% 
    left_join(Listado, by=c("prod_id"="prod_id"))
# data <- data[,-18]

names(data)<-c("Id_Cliente" ,"Afiliado","Segmento","PertencetuClub","Id_Sucursal" ,"nombre_Sucursal","Ciudad","Departamento","Fecha","Venta_Neta","Venta_Bruta",
                   "Cantidad_Productos","prod_id","prod_nombre","prod_proveedor_nit","prod_proveedor_nombre","cod_EAN","Patologia","Categoria" )

data %>% group_by(Id_Cliente) %>% summarise(n=n()) %>%  arrange(desc(n)) %>% head()

mes <- substr(data$Fecha[1],1,6);mes
fwrite(data, paste0("./Datos/POS/_",mes,".txt"))

## Llimpieza de ws----
rm(list=ls(all=T))

