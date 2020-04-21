
# Librerias ---------------------------------------------------------------
library(dplyr)
library(DT)
library(tidyr)
library(data.table)
library(DBI)
library(odbc)
library(tictoc)
library(stringi)


# definición de paths ---------------------------------------------------------

geoemp_path          = "//Bogak08beimrodc/bi/Contacto/Cliente_contacto.accdb"
geopersona_path      = "//Bogak08beimrodc/bi/Contacto/Fuentes/Direccion.accdb"
geopersona_path_alt  = "C:/Users/jauncasc/Desktop/Direccion.accdb"
seg_empresa          = "//Bogak08beimrodc/bi/Empresa/Empresa.accdb"
tablaconversion      = "//Bogak08beimrodc/bi/Tabla_Conversion/Tabla_conversion.accdb"

# formalización de conexión access ----------------------------------------

conect_geoemp   <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",geoemp_path,";"))
dbListTables(conect_geoemp)

conect_geoper   <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",geopersona_path_alt,";"))
dbListTables(conect_geoper)

conect_seg_emp  <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",seg_empresa,";"))
dbListTables(conect_seg_emp)

conect_conver   <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",tablaconversion,";"))
dbListTables(conect_conver)


# Query's sobre las conexiones --------------------------------------------

#Georreferenciación empresa
geoempresa.df   <- odbc::dbGetQuery(conect_geoemp, "SELECT * FROM Direccion_Cliente")

#Georreferenciación persona
geopersona.df   <- odbc::dbGetQuery(conect_geoper, "SELECT * FROM tb_direccion_de_residencia")

# Inclusión de la razón social
razonsocial.df  <- odbc::dbGetQuery(conect_seg_emp,"SELECT id_empresa, 
                                    razon_social FROM Cliente")

# Inclusión de la segmentación empresarial
segmento.df     <- odbc::dbGetQuery(conect_seg_emp,"SELECT id_empresa, 
                                    piramide_1,
                                    piramide_2 FROM Segmento")

# Inclusión nombre de la localidad
localidad.df     <- odbc::dbGetQuery(conect_conver,"SELECT codigo_localidad, 
                                    localidad
                                    FROM Tb_localidad")

# Inclusión de nombre del municipio
municipio.df     <- odbc::dbGetQuery(conect_conver,"SELECT codigo_poblado, 
                                    nombre_municipio
                                    FROM Tb_Division_Politica_Dane")

# Adición campos de segmentación ------------------------------------------

# Pegado de campos 
geoempresa.df <- geoempresa.df %>% left_join(razonsocial.df,by = c("id_empresa")) %>% 
  left_join(segmento.df,by = c("id_empresa")) %>% data.frame() %>% 
  left_join(.,localidad.df,by = c("Cod_Localidad"="codigo_localidad")) %>% 
  left_join(.,municipio.df,by = c("Cod_Poblado"="codigo_poblado"))

# Selección final de campos -----------------------------------------------

geoempresa.df <- geoempresa.df %>% select(id_empresa,
                                          razon_social,
                                          piramide_1,
                                          piramide_2,
                                          Direccion,
                                          Cod_Poblado,
                                          Cod_Localidad,
                                          Barrio,
                                          CX,
                                          CY,
                                          nombre_municipio,
                                          localidad,
                                          Departamento
)

# renombre de campos -----------------------------------------------

colnames(geoempresa.df) <- c("id_empresa", 
                             "razon_social",
                             "piramide_1",
                             "piramide_2",
                             "direccion",
                             "cod_poblado",
                             "cod.localidad",
                             "barrio",
                             "CX",
                             "CY",
                             "nom_mun",
                             "localidad",
                             "nom_dep") 


# Escritura archivos - pre app --------------------------------------------

geoempresa.df <- geoempresa.df %>% 
  mutate(piramide_2= stri_trans_general(piramide_2,id="latin-ascii")) %>% 
  mutate(piramide_2=ifelse(piramide_2=="3.2 VIP Est�ndar","3.2 VIP Estandar",piramide_2)) %>% 
  mutate(piramide_2=ifelse(piramide_2=="4.1 Est�ndar","4.1 Estandar",piramide_2)) 


GeoNoAfil       <- geoempresa.df %>% filter(piramide_2=="No afiliada") %>% data.frame()
GeoEmpresa_afil <- geoempresa.df %>% filter(piramide_2!="No afiliada") %>% data.frame()

GeoEmpresa_afil %>% fwrite(.,file = "GeoEmpresa_afil.csv",sep = ";",row.names = FALSE)
GeoNoAfil       %>% fwrite(.,file = "GeoNoAfil.csv",sep = ";",row.names = FALSE)

# selección de variables geopersonas --------------------------------------

geopersona.df  %>% select(id_persona,
                          direccion_residencia,
                          nivel_socio_economico_residente,
                          barrio,
                          #codigo_localidad,
                          localidad,
                          CX,
                          CY,
                          codigo_poblado) %>% 
  rename(direccion=direccion_residencia,
         codigo_localidad=localidad) %>%  
  fwrite(.,file = "GeoPersona.csv",sep = ";")


# Esto es unicamente para el app empresas afiliadas y no afiliadas -------------------

path_app = "D:/ONEDRIVE/OneDrive - Caja Colombiana de Subsidio Familiar - Colsubsidio/Colsubsidio(Final)/Afiliadas_noAfiliadas_shiny_segmentoempresarial (beta)/data/"

GeoEmpresa_afil %>% rename(IDEMP = id_empresa,
                           `RAZON SOCIAL` = razon_social,
                           `Piramide 1` = piramide_1,
                           `Piramide 2` = piramide_2
) %>% select(IDEMP,
             `RAZON SOCIAL`,
             `Piramide 1`,
             `Piramide 2`,
             CX,
             CY) %>% 
  saveRDS(paste0(path_app,"emp_afiliadas.rds"))


GeoNoAfil %>% rename(Empresa= razon_social,
                           Piramide.1 = piramide_1,
                           Piramide.2 = piramide_2
) %>% select(id_empresa,
             Empresa,
             CX,
             CY,
             direccion,
             Piramide.1,
             Piramide.2) %>% 
  saveRDS(paste0(path_app,"no_afiliadas.rds"))


# Cierre de conexión ------------------------------------------------------

dbDisconnect(conect_geoper)
dbDisconnect(conect_geoemp)
