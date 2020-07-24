library("dplyr")

BD <- readRDS("./data/BD.rds") 
    #filter(Fecha >= "2019-01-01")

suc <- readRDS("./data/suc.rds")

Merge_All <- function(x, y){
    df <- merge(x, y, by= "Score", all=TRUE)
    return(df)
}

### Carga e instalacion de paquetes ====

library("tidyverse")
library("shiny")
library("shinydashboard")
library("leaflet")
library("ggplot2")
library("Hmisc") 
library("scales") 
library("plotly") 
library("DT") 
library("factoextra") 
library("data.table")

### Extraccion de niveles de variables ----

fec_min <- min(BD$Fecha, na.rm = T) ; fec_max <- max(BD$Fecha, na.rm = T)
cate <- c(unique(BD$Categoria), "TODAS")
pato <- c(unique(BD$Patologia), "TODAS")
prov <- c(unique(BD$prod_proveedor_nombre), "TODOS")
afil <- list("Afiliado"="Si", "No Afiliado"="No", "Todos"="Todos")
cred <- list("Activo"="ACTIVO", "Inactivo"="INACTIVO", "Sin TMS"="SIN TMS", "Todas"="TODAS")
cata <- list("Categoria A"="A", "Categoria B"="B", "Categoria C"="C", "No Afiliado"="NO AFILIADO", "Todas"="TODAS")
eda_min <- min(BD$Edad, na.rm = T) ; eda_max <- max(BD$Edad, na.rm = T)
gene <- list("Masculino"="M", "Femenino"="F", "Todos"="Todos")
segm <- list("Basico"="BÁSICO", "Medio"="MEDIO", "Joven"="JOVEN", "Alto"="ALTO", "No Afiliado"="NO AFILIADO", "Todos"="TODOS")
scor <- c("Todos", "1","2","3","4","5")

