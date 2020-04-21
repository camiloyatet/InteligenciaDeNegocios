
# Librerias ---------------------------------------------------------------
library(dplyr)
library(DT)
library(tidyr)
library(data.table)
library(DBI)
library(odbc)
library(tictoc)
library(stringi)
library(RODBC)
library(fuzzyjoin)
library(stringr)

# parámetros de la función -------------------------------------------------
tic()

month      = "03"
year       = "2020"
month.year = T

# definición de paths ---------------------------------------------------------
dir_infra    <- "//BOGAk08Beimrodc/BI/Contacto/Infraestructura/Infraestructura.accdb"

# formalización de conexión access ----------------------------------------
ic      <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",dir_infra))
# Infraestructura colsubsidio ---------------------------------------------

index   <- RODBC::sqlTables(ic)$TABLE_NAME %>% grep(pattern = "COLSUBISIDIO_INFRAESTRUCTURA") 
tabla   <- RODBC::sqlTables(ic)$TABLE_NAME[index]

infraestructura_cols <- sqlQuery(ic, 
                                 paste("SELECT * FROM",tabla),
                                 stringsAsFactors = FALSE,as.is=T) %>% 
  distinct() %>% 
  rename_all(tolower)


# Variable mes y año (Base Incremental) -----------------------------------

if(month.year != T){
  
  infraestructura_cols<- infraestructura_cols %>% 
    mutate(mes=ifelse(length(month(Sys.time()) - 1)!=2,
                      paste0("0",month(Sys.time()) - 1),
                      month(Sys.time()) - 1)) %>% 
    mutate(año=year(Sys.time()))
  
}else{
  
  infraestructura_cols <- infraestructura_cols %>% 
    mutate(mes=month) %>% 
    mutate(año=year)
  
}

# Salvado de las base ----------------------------------------------------

# ContactactabilidadEmpresas-<MES><AÑO>

Nombre = "InfraestructuraColsubsidio-"

if(length(month(Sys.time()) - 1)!=2){
  
  Mes = paste0("0",month(Sys.time()) - 1)
}else{
  Mes = month(Sys.time()) - 1
}

Año = year(Sys.time())

# Escritura del archivo

if(month.year != T){
  
  fwrite(infraestructura_cols,paste0("Y://HabeasData/InfraestructuraColsubsidio/",Nombre,Mes,Año,".txt"),sep=";")
  
}else{
  
  fwrite(infraestructura_cols,paste0("Y://HabeasData/InfraestructuraColsubsidio/",Nombre,month,year,".txt"),sep=";")
}

toc()


