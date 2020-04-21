
# Librerias ---------------------------------------------------------------

library(dplyr)
library(DT)
library(tidyr)
library(data.table)
library(DBI)
library(odbc)
library(stringi)
library(RODBC)
library(stringr)

# definición de paths ---------------------------------------------------------
dir          <- "//BOGAk08Beimrodc/BI/Filial"
exclude      <- "Famisanar_"
listfiles    <- list.files(dir,pattern = "Famisanar")
listfiles    <- listfiles[str_detect(listfiles, exclude, negate = TRUE)]
paths_accdb  <- paste0(dir,"/",listfiles)

# formalización de conexión access ----------------------------------------

fm       <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",paths_accdb))
index    <- RODBC::sqlTables(fm)$TABLE_NAME %>% grep(pattern = "Famisanar") 
tabla    <- RODBC::sqlTables(fm)$TABLE_NAME[index]

consulta <- sqlQuery(fm,paste("SELECT * FROM",tabla),
                     stringsAsFactors = FALSE,
                     as.is=T)


# Parámetros función ------------------------------------------------------

meses = sprintf("%02d", 1:12)
#meses = sprintf("%02d", 1)

años  = c("2018","2019","2020") 

filtrado <- function(df,month,year){
  df %>% 
    mutate(mes=ifelse(nchar(mes)==1,paste0("0",mes),mes)) %>% 
    mutate(año=as.character(año)) %>% 
    filter(mes==month) %>%  
    filter(año==year) %>% 
    fwrite(.,paste0("Y://HabeasData/Famisanar/","Famisanar-",month,year,".txt"),sep=";")
}

for(i in 1: length(años)){
  for(j in 1:length(meses)){
    
    filtrado(consulta,month=meses[j],year=años[i])
    
  }
}


odbcCloseAll()

