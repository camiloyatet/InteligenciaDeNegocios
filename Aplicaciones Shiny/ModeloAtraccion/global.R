options(scipen = 99999)

library(shinydashboard)
library(tidyverse)
library(data.table)
library(DT)
library(shinydashboardPlus)
library(shinythemes)
library(shinyjs)
library(dashboardthemes)
library(scales)
library(caret)
library(gbm)

### Cargue de datos -----
smlmv <- 828116 #

modelo <- readRDS("data/Modelo.rds")
CIIU <- readRDS("data/CIIU.rds") #
Salario <- readRDS("data/Salario.rds") #
Relacion <- readRDS("data/Relacion.rds")
Sectores <- readRDS("data/Sectores.rds")




