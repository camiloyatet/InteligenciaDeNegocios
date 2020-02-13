sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(disable = TRUE, br(),
                                        HTML('<center><img src="logo.png" width="300" height="60"></center>'),
                                        br(),br(),
                                        menuItem("Instrucciones", tabName = "ins", icon = icon("chalkboard-teacher")),
                                        menuItem("Empresas Foco", tabName = "emp", icon = icon("building")),
                                        radioButtons("estrategia", label = h5("Estrategia Comercial"), choices = c('Atracción','Fidelización','Retención'), inline = F)
                                        )
                            )

body <- dashboardBody(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tabItems(
    tabItem("ins",
            h3("Instrucciones"),
            h5("La siguinte animacon muestra el funcionamiento de la herramienta"),
            HTML('<center><img src="gif.gif"></center>'),
            ),
    tabItem("emp",
            conditionalPanel(condition = "input.estrategia == 'Fidelización' || input.estrategia == 'Retención'", #Codigo JS
                             selectInput("EmpresaAfiliada", h5("Seleccione la Empresa"), choices = c(''), width = "400px"),
                             fluidRow(column(6, h3("Mapa"), leafletOutput('mapa_afiliados', height = "600px")),
                                      column(3, 
                                             valueBoxOutput("po_box_NIT", width = "100%"),
                                             valueBoxOutput("po_box_Pir1", width = "100%"),
                                             valueBoxOutput("po_box_Pir2", width = "100%"),
                                             valueBoxOutput("po_box_CIIU1", width = "100%"),
                                             valueBoxOutput("po_box_CIIU2", width = "100%"),
                                             valueBoxOutput("po_box_CIIU3", width = "100%")),
                                      column(3,
                                             valueBoxOutput("po_box_Aporte", width = "100%"),
                                             valueBoxOutput("po_box_Remanente", width = "100%"),
                                             valueBoxOutput("po_box_Empleados", width = "100%"),
                                             valueBoxOutput("po_box_CATA", width = "100%"),
                                             valueBoxOutput("po_box_CATB", width = "100%"),
                                             valueBoxOutput("po_box_CATC", width = "100%")
                                             )
                                      )
                             ),
            conditionalPanel(condition = "input.estrategia == 'Atracción'",
                             # dataTableOutput('test'),
                             selectInput("EmpresaNoAfiliada", h5("Seleccione la Empresa"), choices = c(''), width = "400px"),
                             fluidRow(column(6, h3("Mapa"), leafletOutput('mapa_no_afiliados', height = "600px")),
                                      column(3, 
                                             valueBoxOutput("po_box_NIT_na", width = "100%"),
                                             valueBoxOutput("po_box_Pir1_na", width = "100%"),
                                             valueBoxOutput("po_box_Pir2_na", width = "100%"),
                                             valueBoxOutput("po_box_CIIU1_na", width = "100%"),
                                             valueBoxOutput("po_box_CIIU2_na", width = "100%"),
                                             valueBoxOutput("po_box_CIIU3_na", width = "100%")),
                                      column(3,
                                             valueBoxOutput("po_box_Aporte_na", width = "100%"),
                                             valueBoxOutput("po_box_Remanente_na", width = "100%"),
                                             valueBoxOutput("po_box_Empleados_na", width = "100%"))
                                      ),
                             br(),
                             h3("Empresas Afiliadas Similares"),
                             br(),
                             fluidRow(
                               column(4, valueBoxOutput("po_box_Vecino1", width = "100%")),
                               column(4, valueBoxOutput("po_box_Vecino2", width = "100%")),
                               column(4, valueBoxOutput("po_box_Vecino3", width = "100%"))
                               )
                             )
            )
    )
)

dashboardPage(
  dashboardHeader(title = "Informacion Empresas Foco", titleWidth = 300),
  sidebar,
  body,
  tags$head(tags$style(HTML('
                            .skin-blue .main-header .logo {
                            background-color: #3c8dbc;
                            }
                            .skin-blue .main-header .logo:hover {
                            background-color: #3c8dbc;
                            }
                            ')))
  )

