## Librerias -----
library("tidyverse")
library("DT")
library("shinydashboard")
library("shinyWidgets")
library("plotly")
library("scales")

## Cargue de Datos -----
data <- readRDS("data/data.rds") %>% 
  mutate(Afiliado=ifelse(is.na(id_persona), F, T))

fec_min <- readRDS("data/fec_min.rds")
fec_max <- readRDS("data/fec_max.rds")


test <- data %>% filter(NumIdPersona==52251145) %>% 
  select(NumIdPersona, Nombre, Periodo, UES, Producto, Valor)
