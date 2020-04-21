

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

month      = "09"
year       = "2019"
month.year = T

# Lectura de archivo .txt -------------------------------------------------

proteccion.df <- fread("C:/Users/jauncasc/Desktop/Proteccion.txt")

# Conexión access ---------------------------------------------------------

prot_path        <- "//Bogak08beimrodc/bi/Filial/Proteccion.accdb"  ##ruta
prot_path_alt    <- "C:/Users/jauncasc/Desktop/Proteccion.accdb"


conect_prot <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",prot_path,";"))
dbListTables(conect_prot)

conect_prot_alt <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",prot_path_alt ,";"))
dbListTables(conect_prot_alt)

# tablas construcción protección -------------------------------------

proteccion.df <- odbc::dbGetQuery(conect_prot_alt, "SELECT * FROM Proteccion")

# construcción de variables -TipDocumento- -NumeroDocumento- -ESTADO OBL- -ESTADO VOL-
# -ESTADO CES-

proteccion.df <- proteccion.df %>% mutate(TipDocumento=gsub("[^a-zA-Z]", "", id_persona)) %>% 
  mutate(NumeroDocumento=gsub("[^0-9.]", "",id_persona)) %>%
  rename(`ESTADO OBL`= pension_obligatoria) %>% rename(`ESTADO VOL`=pension_voluntaria) %>% 
  rename(`ESTADO CES`= cesantia) %>% select(-Proteccion) %>% 
  rename(IDPERSONA=id_persona)

# Construcción de marcas  -------------------------------------------------

proteccion.df <- proteccion.df %>% 
  mutate(`ESTADO OBL`=ifelse(`ESTADO OBL`==1,"ACT","")) %>% 
  mutate(`ESTADO VOL`=ifelse(`ESTADO VOL`==1,"ACT","")) %>% 
  mutate(`ESTADO CES`=ifelse(`ESTADO CES`==1,"ACT",""))

# Creación variable año ---------------------------------------------------

proteccion.df <- proteccion.df %>% mutate(año=2017)

# Variable mes y año (Base Incremental) -----------------------------------

proteccion.df <- proteccion.df  %>% 
  mutate(mes=ifelse(nchar(mes)!=2,
                    paste0("0",mes),
                    mes))

# Orden del data set
colnames(proteccion.df)
proteccion.df <- proteccion.df %>% select(IDPERSONA,TipDocumento,NumeroDocumento,
                                          `ESTADO OBL`,`ESTADO CES`,`ESTADO VOL`,mes,año)
# Salvado de las base ----------------------------------------------------


ext_df_proteccion <- function(data,m,y){
  
  Nombre = "InsumoProteccion"
  data %>% filter(mes==m) -> result
  fwrite(result,paste0(Nombre,"-",m,y,".txt"),sep = ";")
  
}


vect.mes <- levels(as.factor(proteccion.df$mes))
for(i in vect.mes){
ext_df_proteccion(proteccion.df,i,"2017")
}


  