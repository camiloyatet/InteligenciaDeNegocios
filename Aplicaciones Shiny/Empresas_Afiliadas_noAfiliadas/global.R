library("dplyr")
library("DT")
library("shinydashboard")
library("scales")
library("tidyr")
library("leaflet")
library("leaflet.extras")
library("shiny")
library("geosphere")
library("rlang")
library("tibble")


#Empresas no afiliadas
no_afiliadas           <- readRDS("./data/no_afiliadas.rds") %>% 
        as.data.frame()


#Empresas afiliadas
emp_afiliadas          <- readRDS("./data/emp_afiliadas.rds") %>% as.data.frame()

# Matriz toda la infraestructura ------------------------------------------

#Infraestructura 
infraestructuras        <- readRDS("./data/infraestructura.rds") %>% 
  mutate(TIPO=iconv(TIPO,from = "UTF-8",to="ASCII//TRANSLIT")) %>% 
  mutate(TIPO=ifelse(TIPO=="RECREACIEN","RECREACION",TIPO))


# Carga base de convenios
convenios              <- readRDS("./data/convenios.rds") %>% rename(CX=cx,CY=xy) %>% 
        mutate(RSCOD=paste0(`NOMBRE DEL COMERCIO`," - ",COD))
  table(infraestructuras$TIPO)

