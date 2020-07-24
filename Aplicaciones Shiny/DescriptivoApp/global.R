library(DT);library(shiny);library(Hmisc);library(scales);library(plotly);
require(gridBase);library(tidyverse);library(data.table);library(shinydashboard);
library(shinydashboardPlus);library(shinyWidgets);library(scales);library(bit64)
# ------------------------------------------ Datos -----------------------------------------------
BaseDatos <- read_rds("data/Base.rds") %>% 
  filter(marca_afiliado_unico)
BaseDatos <- BaseDatos %>% mutate(NumIdPersona = as.character(NumIdPersona))
#other <- BaseDatos %>% select(id_persona)

#BaseDatos <- read_rds("data/rds5000.rds") %>% 
#  filter(marca_afiliado_unico)
#other <- BaseDatos %>% select(id_persona)