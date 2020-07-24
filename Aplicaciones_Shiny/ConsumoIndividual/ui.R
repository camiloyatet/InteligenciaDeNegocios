sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(disable = TRUE, br(),
                                        tags$img(src = "logo.png", height=55, width=270, align="center"),
                                        br(),br(),
                                        menuItem("Instrucciones", tabName = "ins", icon = icon("chalkboard-teacher")),
                                        menuItem("Consumo Individual", tabName = "res", icon = icon("users")),
                                        br(),
                                        selectInput("Unidad", h5("Seleccione la unidad de interes"), choices=c(unique(data$UES), "Todas"), selected = "Recreacion y Turismo"),
                                        selectInput("Producto", h5("Seleccione el producto de interes"), choices=""),
                                        br(),
                                        actionButton("go", "Aplicar", icon = icon("refresh")),
                                        br()
                            )
)

body <- dashboardBody(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tabItems(
    #### Instrucciones----
    tabItem("ins",
            h3("Instrucciones"),
            h5("La siguiente animacion muestra el funcionamiento paso a pas de la herramienta"),
            HTML('<center><img src="manual.gif",height="1000" width="1500"></center>')
            ),
    ### Estadisticas ----
    tabItem("res",
            h3("Distribucion de Personas segun Producto"),
            selectInput("Medida", h5("Seleccione la Variable de Analisis"), 
                        choices=list("Monto"="Monto", "Numero de Cientes"="Personas", "Numero de Registros"="Frecuencia"), 
                        selected = "Todas", width = "600px"),
            fluidRow(
              valueBoxOutput("po_box_Total", width = 4),
              valueBoxOutput("po_box_afiliados", width = 4),
              valueBoxOutput("po_box_NoAfiliados", width = 4)
              ),
            br(),
            downloadButton("Descargar1", "Descargar"),
            br(),br(),
            switchInput(inputId = "TipoMedida",onStatus = "primary", offStatus = "info", onLabel = "Valor", offLabel = "Porcentaje", 
                        width = "400px", value = T),
            plotlyOutput('AfiliadosAbsoluto'),
            h3("Informacion de Afiliados"),
            box(status = 'primary', solidHeader = F, width = 12, collapsible = T, collapsed = F,
                plotlyOutput('Piramide', height = "600px"),
                br(),
                fluidRow(
                  column(6,plotlyOutput('Categoria', height = "600px")),
                  br(),
                  column(6,plotlyOutput('Segmento', height = "600px"))
                  ),
                br(),
                fluidRow(
                  column(6,plotlyOutput('PiramideEmp', height = "600px")),
                  br(),
                  column(6,plotlyOutput('CIIU', height = "600px"))
                  )
                )
            # dataTableOutput('test')
            )
    ### ----
    )
  )

dashboardPage(
  dashboardHeader(title = "Analisis de Consumo", titleWidth = 300),
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

