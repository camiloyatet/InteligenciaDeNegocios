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

data <- readRDS("data/data.rds")

