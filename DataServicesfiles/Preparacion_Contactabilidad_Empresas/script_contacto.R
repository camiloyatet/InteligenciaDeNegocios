

# Librerias ---------------------------------------------------------------
library(dplyr)
library(DT)
library(tidyr)
library(data.table)
library(DBI)
library(odbc)
library(tictoc)

tic()

# parámetros de la función -------------------------------------------------

month      = "12"
year       = "2019"
month.year = T

#prueba <- fread("Y://HabeasData/ContactabilidadEmpresas/ContactactabilidadEmpresas-092019.txt")
#colnames(prueba)

# Conexión access ---------------------------------------------------------

contactabilidad_path <- "//Bogak08beimrodc/bi/Contacto/Cliente_contacto.accdb"  ##ruta
empresa_path         <- "//Bogak08beimrodc/bi/Empresa/Empresa.accdb"
#empresa_path.alt     <- "C:/Users/jauncasc/Desktop/Empresa.accdb"



conect_con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",contactabilidad_path,";"))
dbListTables(conect_con)

conect_emp <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",empresa_path,";"))
dbListTables(conect_emp)

# tablas construcción contactabilidad -------------------------------------

# Contactabilidad
Movil_Cliente         <- odbc::dbGetQuery(conect_con, "SELECT * FROM Movil_Cliente")
Direccion_Cliente     <- odbc::dbGetQuery(conect_con, "SELECT * FROM Direccion_Cliente")
Mail_Cliente          <- odbc::dbGetQuery(conect_con, "SELECT * FROM Mail_Cliente")
Sitio_Web_Cliente     <- odbc::dbGetQuery(conect_con, "SELECT * FROM Sitio_Web_Cliente")
Telefono_Fijo_Cliente <- odbc::dbGetQuery(conect_con, "SELECT * FROM Telefono_Fijo_Cliente")

# Razón social
razonsocial <- odbc::dbGetQuery(conect_emp, "SELECT id_empresa,codigo_tipo_documento,
                                      numero_documento,numero_documento_sin_digito,razon_social 
                                FROM Cliente")

# # Listado de archivos txt -------------------------------------------------
# 
# ues.path         <- list.files(path = getwd(),pattern = ".txt")
# new.names        <- gsub(x = ues.path,pattern = ".txt",replacement = "")
# 
# # Carga de las bases de datos txt -----------------------------------------
# 
# for(i in 1:length(ues.path)){
#   
#   assign(new.names[i], fread(paste0(getwd(),"/",ues.path[i]),sep = ";",dec = ".")%>% 
#            data.frame()) 
# }     

# Marca fecha ultima modificacion por cada archivo ------------------------------------------
Direccion_Cliente <- Direccion_Cliente %>% 
  rename(Fecha.de.la.última.modificación.Dir.Cliente=`Fecha de la última modifi`)

Mail_Cliente <- Mail_Cliente %>% 
  rename(Fecha.de.la.última.modificación.mail.Cliente=`Fecha de la última modificación`)

Movil_Cliente <- Movil_Cliente %>% 
  rename(Fecha.de.la.última.modificación.movil.Cliente=`Fecha de la última modificación`)

Sitio_Web_Cliente <- Sitio_Web_Cliente %>% 
  rename(Fecha.de.la.última.modificación.web.Cliente=`Fecha de la última modificación`)

Telefono_Fijo_Cliente <- Telefono_Fijo_Cliente %>% 
  rename(Fecha.de.la.última.modificación.tel.fijo.Cliente=`Fecha de la última modificación`)

# Union de las bases ------------------------------------------------------
empresas_contactabilidad <- Direccion_Cliente %>% 
  left_join(select(razonsocial,id_empresa,razon_social),by = c("id_empresa")) %>%
  left_join(Mail_Cliente,by = c("id_empresa")) %>%
  left_join(Movil_Cliente,by = c("id_empresa")) %>% 
  left_join(Sitio_Web_Cliente,by = c("id_empresa")) %>% 
  left_join(Telefono_Fijo_Cliente,by = c("id_empresa"))


# correccion formato de los telefonos -------------------------------------

# empresas_contactabilidad <- empresas_contactabilidad %>% 
#   mutate(telefono.movil=substr(telefono.movil,1,nchar(telefono.movil)-3)) %>% 
#   mutate(Teléfono=substr(Teléfono,1,nchar(Teléfono)-3))

# CORRECION DE ENCABEZADOS ------------------------------------------------
colnames(empresas_contactabilidad)<- tolower(iconv(colnames(empresas_contactabilidad),to="ASCII//TRANSLIT"))

# Variable mes y año (Base Incremental) -----------------------------------

if(month.year != T){
  
  empresas_contactabilidad<- empresas_contactabilidad %>% 
    mutate(mes=ifelse(length(month(Sys.time()) - 1)!=2,
                      paste0("0",month(Sys.time()) - 1),
                      month(Sys.time()) - 1)) %>% 
    mutate(año=year(Sys.time()))
  
}else{
  
  empresas_contactabilidad <- empresas_contactabilidad %>% 
    mutate(mes=month) %>% 
    mutate(año=year)
  
}

# Corrección formato de fechas --------------------------------------------

empresas_contactabilidad <- empresas_contactabilidad %>% mutate(fecha.de.la.ultima.modificacion.dir.cliente=as.character(as.Date(fecha.de.la.ultima.modificacion.dir.cliente,
                                                                                                                                 tryFormats = c('%Y-%m-%d',
                                                                                                                                                "%d/%m/%Y %H:%M",
                                                                                                                                                "%Y/%m/%d",
                                                                                                                                                "%d/%m/%Y %H:%M",
                                                                                                                                                "%d/%m/%Y %H:%M:%S"
                                                                                                                                 )))) %>% 
  
  mutate(fecha.de.la.ultima.modificacion.mail.cliente=as.character(as.Date(fecha.de.la.ultima.modificacion.mail.cliente,
                                                                           tryFormats = c('%Y-%m-%d',
                                                                                          "%d/%m/%Y %H:%M",
                                                                                          "%Y/%m/%d",
                                                                                          "%d/%m/%Y %H:%M",
                                                                                          "%d/%m/%Y %H:%M:%S"
                                                                           )))) %>% 
  
  mutate(fecha.de.la.ultima.modificacion.movil.cliente=as.character(as.Date(fecha.de.la.ultima.modificacion.movil.cliente,
                                                                            tryFormats = c('%Y-%m-%d',
                                                                                           "%d/%m/%Y %H:%M",
                                                                                           "%Y/%m/%d",
                                                                                           "%d/%m/%Y %H:%M",
                                                                                           "%d/%m/%Y %H:%M:%S"
                                                                            )))) %>% 
  
  mutate(fecha.de.la.ultima.modificacion.web.cliente=as.character(as.Date(fecha.de.la.ultima.modificacion.web.cliente,
                                                                          tryFormats = c('%Y-%m-%d',
                                                                                         "%d/%m/%Y %H:%M",
                                                                                         "%Y/%m/%d",
                                                                                         "%d/%m/%Y %H:%M",
                                                                                         "%d/%m/%Y %H:%M:%S"
                                                                          )))) %>% 
  
  mutate(fecha.de.la.ultima.modificacion.tel.fijo.cliente=as.character(as.Date(fecha.de.la.ultima.modificacion.tel.fijo.cliente,
                                                                               tryFormats = c('%Y-%m-%d',
                                                                                              "%d/%m/%Y %H:%M",
                                                                                              "%Y/%m/%d",
                                                                                              "%d/%m/%Y %H:%M",
                                                                                              "%d/%m/%Y %H:%M:%S"
                                                                               ))))




# Salvado de las base ----------------------------------------------------

# ContactactabilidadEmpresas-<MES><AÑO>

Nombre = "ContactactabilidadEmpresas"

if(length(month(Sys.time()) - 1)!=2){
  
  Mes = paste0("0",month(Sys.time()) - 1)
}else{
  Mes = month(Sys.time()) - 1
}

Año = year(Sys.time())

 
# Escritura del archivo

if(month.year != T){
  
  fwrite(empresas_contactabilidad,paste0(Nombre,"-",Mes,Año,".txt"),sep = ";")
  
}else{
  
  fwrite(empresas_contactabilidad,paste0(Nombre,"-",month,year,".txt"),sep = ";")
}

toc()

dbDisconnect(conect_emp)
dbDisconnect(conect_con)
