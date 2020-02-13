library("tidyverse")
library("DT")
library("shinydashboard")
library("scales")
library("leaflet.extras")

# Cargue de Datos ----
Infraestructura <- readRDS("data/Infraestructura.rds")
listado <- readRDS("data/listado.rds")
afiliadas <- readRDS("data/afiliadas.rds")
personas <- readRDS("data/personas.rds")
noafiliadas <- readRDS("data/NoAfiliadas.rds")
vecinos <- readRDS("data/Vecinas.rds")

## Iconos ----
Empleo <- makeIcon(
  iconUrl = "Colsubsidio1.png",
  iconWidth = 20, iconHeight = 35
)

AyB <- makeIcon(
  iconUrl = "AlimentosBebidas1.png",
  iconWidth = 20, iconHeight = 35
)

CS <- makeIcon(
  iconUrl = "Colsubsidio1.png",
  iconWidth = 20, iconHeight = 35
)

Credito <- makeIcon(
  iconUrl =  "CreditoSeguros1.png",
  iconWidth = 20, iconHeight = 35
)

Cultura <- makeIcon(
  iconUrl =  "Cultura1.png" ,
  iconWidth = 20, iconHeight = 35
)

Educacion <- makeIcon(
  iconUrl =  "Cultura1.png" ,
  iconWidth = 20, iconHeight = 35
)

Super <- makeIcon(
  iconUrl =  "Supermercado1.png",
  iconWidth = 20, iconHeight = 35
)

RyT <- makeIcon(
  iconUrl =  "Turismo1.png",
  iconWidth = 20, iconHeight = 35
)

Salud <- makeIcon(
  iconUrl =  "Salud1.png" ,
  iconWidth = 20, iconHeight = 35
)

Vivienda <- makeIcon(
  iconUrl =  "Vivienda1.png",
  iconWidth = 20, iconHeight = 35
)
