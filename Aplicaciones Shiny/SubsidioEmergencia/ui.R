library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shinycssloaders)

dashboardPage(dashboardHeader(title = "Subsidio Emergencia"),
              dashboardSidebar(
                sidebarMenu(disable = TRUE,br(),
                            tags$img(src = "logo.png", height=40, width=200, align="center"),
                            tags$hr(),
                            shinyjs::hidden(menuItem("INSTRUCCIONES", tabName = "dummy")),
                            tags$hr(),
                            menuItem("CONSULTA", tabName = "consulta", icon = icon("area-chart")),
                            br()
                            )),
              dashboardBody(
                shinyDashboardThemes(theme = "blue_gradient"),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                tabItems(
                  tabItem("dummy",
                          fluidRow(
                            column(1),
                            column(10,
                                   h1("Descripción la aplicación"),
                                   br(),
                                   h3(
                                   "La presente aplicación es una herramienta de consulta que permite el seguimiento del 
                                   estado de las solicitudes por subsidio de emergencia."
                                   ),
                                   br(),
                                   h3(
                                   "En la siguiente pestaña 'Consulta' puede subir un listado de Cédulas en un archivo excel.
                                   Una vez realice el cargue de la información debe dar click en Consultar para ejecutar la busqueda."
                                   ),
                                   br(),
                                   h3( 
                                     "Fecha de actualización: 16 de abril de 2020."
                                   )
                            ),
                            column(1)
                          )
                  ),
                  ### Consulta ====
                  tabItem(tabName = "consulta",
                            fluidRow(
                              column(1),
                              column(3,
                                     h3("Cargar información"),
                                     fileInput("file1", h5("Seleccione archivo (excel)"),
                                               buttonLabel = "Cargar",
                                               placeholder = "Sin archivo seleccionado",
                                               multiple = F,
                                               accept = c(".xlsx"))
                              ),
                              column(2,
                                     br(),br(),br(),br(),
                                     actionButton("go", label = "Consultar")
                              ),
                              column(1),
                              column(5,
                                     h3("Resumen"),
                                     valueBoxOutput("num_informes",width = 9)
                                     # ,valueBoxOutput("num_per_tiempo",width = 3),
                                     # valueBoxOutput("num_per_sub_pagados",width = 3),
                                     # valueBoxOutput("num_per_sub_colsubsidio",width = 3),
                                     # valueBoxOutput("num_per_sub_compensar", width = 3),
                                     # valueBoxOutput("num_per_sub_cafam", width = 3),
                                     # valueBoxOutput("num_per_afil_colsubsidio", width = 3)
                                     )
                              ),
                            fluidRow(
                              column(4,
                                     box(title = "Tiempo cotizado", status = "primary", solidHeader = F, width = 12,
                                         DT::dataTableOutput("datos_cotiza"))
                              ),
                              column(4,
                                     box(title = "Solicitud de Subsidio", status = "primary", solidHeader = F, width = 12,
                                         DT::dataTableOutput("datos_subsidio"))
                              ),
                              column(4,
                                     box(title = "Afiliados actuales (Corte Febrero)", status = "primary", solidHeader = F, width = 12,
                                         DT::dataTableOutput("datos_afil"))
                              )
                            ),
                            downloadButton("downloadData", "Descargar Base")
                          )
                  )
                )
              )

