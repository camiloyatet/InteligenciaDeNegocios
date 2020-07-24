# Cargar datos y librerias

library(shiny); library(ggplot2)
library(dplyr); library(plotly)
library(shinydashboard);library(DT)
library(shinyjs); library(reshape)
library(data.table); library(tidyr)
library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes)
library(dashboardthemes)
library(scales); library(shinycustomloader)
library(readxl); library(writexl);
library(shinyWidgets)

# Cargamos datos
df_informe_transacciones <- readRDS("Data/df_informe_transaciones.rds")
str(df_informe_transacciones)

tb_mes <- read_excel("Data/tb_mes.xlsx") %>% 
  mutate(mes = as.character(mes))

# Actualizamos input
name_convenio <- readRDS("Data/name_convenio.rds")
name_eps <- readRDS("Data/name_eps.rds")

# name_convenio <- c(unique(base_informe_rds$DENOMINACION_CONVENIO),"Todos")
# name_eps <- c(unique(base_informe_rds$NOMBRE_EPS),"Todos")
# saveRDS(name_convenio, "App/Data/name_convenio.rds")
# saveRDS(name_eps, "App/Data/name_eps.rds")
