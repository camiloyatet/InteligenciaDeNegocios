# Cargamos librerias
library(shiny); library(ggplot2)
library(dplyr); library(plotly)
library(shinydashboard);library(DT)
library(shinyjs); library(reshape)
library(data.table); library(tidyr)
library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes)
library(dashboardthemes)
library(scales); library(shinycustomloader)
library(readxl); library(writexl)

# listado poblacion con meses cotizados
poblacion <- readRDS("Data/df_union_13042020.rds")
str(poblacion)

# Base Consolidada mensual
consolidada <- readRDS("Data/consolidada_edit_FEB2020.rds")
str(consolidada)

# Subsidios entregados
subsidios_entregados <- readRDS("Data/subsidios_entregados.rds")
str(subsidios_entregados)

# Subsidios postulados
subsidios_postulados <- readRDS("Data/subsidios_postulados.rds")
str(subsidios_postulados)

# # Test personas
# test_personas <- read_excel("Data/test_personas.xlsx") %>% 
#   data.frame() 