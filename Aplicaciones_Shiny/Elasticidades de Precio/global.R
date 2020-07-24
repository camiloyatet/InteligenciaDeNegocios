# Librerias ----
library("tidyverse")
library("shinydashboard")
library("shinyjs")
library("leaflet")
library("plotly")
library("DT")
library("dygraphs")
library("xts")
library("caret")
library("FactoMineR")
library("factoextra")
library("data.table")
library("ggplot2")
library("prophet")
library("lme4")
library("Matrix")
library("scales")
library("agricolae")
library("rhandsontable")
library("V8")

# Cargar Datos ----
data <- readRDS("data/DataDemanda.rds") %>% ungroup()  %>% filter(!is.na(R_Score), !is.na(Regional))
Precios <- readRDS("data/Precios.rds")
Resultados <- readRDS("data/Resultados.rds")

# Definiciones
prods <- unique(data$prod_id)
regis <- c(unique(data$Regional), "TODAS")
cate <- c(unique(data$categoria), "TODAS")
pato <- c(unique(data$Patologia), "TODAS")
tipo <- c(unique(data$Tipologia), "TODAS")
fecha_ref <- min(data$Fecha)

# Llamar Funciones
source("functions/function.R")


