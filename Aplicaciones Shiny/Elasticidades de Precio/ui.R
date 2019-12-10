### Header ----
header <- dashboardHeader(title = "Optimizacion de Precios", titleWidth = 300)

### Sidebar ----
sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(id="sbmenu",
                                        useShinyjs(),
                                        br(),
                                        tags$img(src = "logo.png", height=40, width=300, align="center"),
                                        br(),br(), 
                                        shinyjs::hidden(menuSubItem("dummy", tabName = "dummy")),
                                        menuItem("Analisis RFM", tabName = "rfm", icon = icon("pills")),
                                        menuItem("Segmentacion de Productos", tabName = "segm", icon = icon("object-ungroup")),
                                        menuItem("Comparacion de Precios", tabName = "precio", icon = icon("hand-holding-usd")),
                                        menuItem(text = "Optimización de Precio", tabName = "optim", icon = icon("money")),
                                        menuItem(text = "Resultados del Modelo", tabName = "resul", icon = icon("poll")),
                                        menuItem(text = "Modificacion de Precios por Carga", tabName = "mod_mas", icon = icon("file-upload")),
                                        menuItem(text = "Modificacion de Precios Individual", tabName = "modif", icon = icon("dollar")),
                                        br(),br(),
                                        HTML('<center><img src="ues.png" width="150" height="40"></center>')
                                        )
                            )

### Body ----
body <- dashboardBody(
    ### Opciones CSS ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css")
        ),
    # tags$style(type="text/css",
    #            ".shiny-output-error { visibility: hidden; }",
    #            ".shiny-output-error:before { visibility: hidden; }"),
    ### Pestañas ----
    tabItems(
        ### Dummy ----
        tabItem("dummy",
                fluidRow(
                    column(1),
                    column(10,
                           h1("Analisis Elasticidades de Precios"),
                           br(),br(),
                           h3("La presente aplicacion tiene por objetivo ser una herramienta de consulta de las posibles variaciones de cantidades respecto a los precios de los productos de las Droguerias Colsubsidio.")
                    ),
                    column(1)
                )
        ),
        ### Analisis RFM ----
        tabItem("rfm",
                box(title = "Distribución de Productos por Scores RFM",
                    status = "primary",
                    solidHeader = FALSE,
                    width = 12,
                    plotOutput("ResumenRFM",height = "800px")
                ),
                br(),
                box(title = "Valores de los Scores RFM",
                    status = "primary",
                    solidHeader = FALSE,
                    width = 12,
                    fluidRow(
                        column(4,h4("Frecuencia"),
                               tableOutput('TablaFrecuencia')
                               ),
                        column(4,h4("Recencia"),
                               tableOutput('TablaRecencia')
                               ),
                        column(4,h4("Monto"),
                               tableOutput('TablaMonto')
                               )
                        )
                    )
                ),
        ### Segmentacion ----
        tabItem("segm",
                h2("Segmentación de Productos"),
                br(),
                box(title = "Comparacion de Scores entre Clusters",
                    status = "primary",
                    solidHeader = FALSE,
                    width = 12,
                    plotOutput("CompR", height = 300),
                    br(),
                    plotOutput("CompF", height = 300),
                    br(),
                    plotOutput("CompM", height = 300)
                    ),
                br(),
                box(title = "Clusters en las dos Primeras Dimensiones",
                    status = "primary",
                    solidHeader = FALSE,
                    width = 12,
                    plotOutput("biplot", height = "800px")
                    ),
                br()
                ),
        ### Comparacion de Precios ----
        tabItem("precio",
                h2("Análisis de Precios"),
                br(),
                box(title = NULL,
                    status = "primary",
                    solidHeader = FALSE,
                    width = 12,
                    column(3, selectInput(inputId = "po_input_product1", label = "Seleccione el PLU del producto", choices = prods, selected = 1210773)),
                    column(3, selectInput(inputId = "po_input_region1", label = "Seleccione la Regional",choices= regis))
                    ),
                box(title = NULL,
                    status = "primary",
                    solidHeader = FALSE,
                    width = 12,
                    fluidRow(
                        valueBoxOutput("po_box_description_1", width = 2),
                        valueBoxOutput("po_box_category_1", width = 2),
                        valueBoxOutput("po_box_patology_1", width = 2),
                        valueBoxOutput("po_box_tipology_1", width = 2),
                        valueBoxOutput("po_box_cluster_1", width = 2),
                        valueBoxOutput("po_box_rfm_1", width = 2)
                        )
                    ),
                box(title = "Comparacion de Precio Historico ",
                    status = "primary",
                    solidHeader = FALSE,
                    width = 12,
                    plotlyOutput("Comparacion_Precios", height = "600px")
                    )
                ),
        ### Optimizacion ----
        tabItem(tabName = "optim",
                box(title = NULL,
                    status = "primary",
                    solidHeader = FALSE,
                    width = 12,
                    column(3, selectInput(inputId = "po_input_product", label = "Seleccione el PLU del producto", choices = prods, selected = 189)),
                    column(3, selectInput(inputId = "po_input_region", label = "Seleccione la Regional",choices= regis, selected = "CENTRO")),
                    column(3, uiOutput("po_render_price")),
                    column(3, checkboxInput(inputId = "po_input_letter", label = "Promocion"))
                ),
                box(title = NULL,
                    status = "primary",
                    solidHeader = FALSE,
                    width = 12,
                    fluidRow(
                        valueBoxOutput("po_box_description", width = 2),
                        valueBoxOutput("po_box_category", width = 2),
                        valueBoxOutput("po_box_patology", width = 2),
                        valueBoxOutput("po_box_tipology", width = 2),
                        valueBoxOutput("po_box_cluster", width = 2),
                        valueBoxOutput("po_box_rfm", width = 2)
                        ),
                    fluidRow(
                        valueBoxOutput("po_box_predicted_demand_level", width = 6),
                        valueBoxOutput("po_box_elasticity", width = 6)
                        ),
                    fluidRow(
                        valueBoxOutput("po_box_current_price", width = 2),
                        valueBoxOutput("po_box_predicted_demand", width = 2),
                        valueBoxOutput("po_box_current_profit", width = 2),
                        valueBoxOutput("po_box_current_cost", width = 2),
                        valueBoxOutput("po_box_current_cost_ttl", width = 2),
                        valueBoxOutput("po_box_current_profit_pct", width = 2)
                        ),
                    fluidRow(
                        valueBoxOutput("po_box_optimum_price", width = 2),
                        valueBoxOutput("po_box_optimum_demand", width = 2),
                        valueBoxOutput("po_box_optimum_profit", width = 3),
                        valueBoxOutput("po_box_optimum_cost_ttl", width = 3),
                        valueBoxOutput("po_box_optimum_profit_pct", width = 2)
                        ),
                    br(),
                    downloadButton("DescargaTransacciones", "Descargar")
                    ),
                fluidRow(
                    box(title = "Datos Historicos",
                        status = "primary",
                        solidHeader = FALSE, 
                        width = 12,
                        box(title = "Precio",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            dygraphOutput("price_history")
                            ),
                        box(title = "Demanda",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            verticalLayout( 
                                dygraphOutput("demand_history", height = 300),
                                dygraphOutput("demand_levels", height = 100)
                                )
                            )
                        )
                    ),
                fluidRow(
                    box(title = "Ventas vs Precio",
                        status = "primary",
                        solidHeader = FALSE, 
                        width = 6,
                        plotlyOutput("po_profit_plot")
                        ),
                    box(title = "Demanda vs Precio",
                        status = "primary",
                        solidHeader = FALSE, 
                        width = 6,
                        plotlyOutput("po_demand_vs_price_plot")
                        )
                    ),
                fluidRow(
                  column(6,
                         box(title = "Prueba de Tukey",
                             status = "primary",
                             solidHeader = FALSE, 
                             width = "100%",
                             plotlyOutput("po_tukey")
                             )
                         ),
                  column(6,
                         fluidRow(
                           box(title = "Diferencia Significativa",
                               status = "primary",
                               solidHeader = FALSE, 
                               width = "100%",
                               valueBoxOutput("tukeyDif", width = "100%")
                               )
                           ),
                         fluidRow(
                           box(title = "Diferencia Promedio",
                               status = "primary",
                               solidHeader = FALSE, 
                               width = "100%",
                               valueBoxOutput("tukeyDifUnidades", width = "100%")
                               )
                           )
                         )
                  )
                ), 
        ### Resultados del Modelo ----
        tabItem(tabName = "resul",
                fluidRow(
                  column(3,
                         box(title = "Regional",
                             status = "primary",
                             solidHeader = FALSE,
                             plotlyOutput("BarrasRegional"),
                             width = "100%")),
                  column(3,
                         box(title = "Categoria",
                             status = "primary",
                             solidHeader = FALSE,
                             plotlyOutput("BarrasCategoria"),
                             width = "100%")),
                  column(3,
                         box(title = "Tipologia",
                             status = "primary",
                             solidHeader = FALSE,
                             plotlyOutput("BarrasTipologia"),
                             width = "100%")),
                  column(3,
                         box(title = "Cluster",
                             status = "primary",
                             solidHeader = FALSE,
                             plotlyOutput("BarrasCluster"),
                             width = "100%"))
                  ),
                extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('plotly_click-plot', 'null'); }"),
                actionButton("reset", "Reset click"),
                fluidRow(
                  dataTableOutput("test"),
                  dataTableOutput("DetalleResultados"),
                  downloadButton("DescargaResultados", "Descargar")
                  )
                ),
        ### Modificacones de Precios ----
        tabItem(
          tabName = "mod_mas",
          tabsetPanel(
            tabPanel("Cargue de Archivo de Precios",
                     h2("Cargue de Informacion"),
                     br(),
                     fluidRow(
                       column(5,
                              box(width = "100%", title= "Cargue del archivo a de PLU y precio", status="info",
                                  fileInput("archivo", label= h6("Cargue la base de datos de precios (csv)"), multiple = FALSE, placeholder="Explore el archivo", buttonLabel="Buscar",
                                            accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")
                                            )
                                  )
                              ),
                       column(7, 
                              box(width = "100%", 
                                  title= "Resultado de la Carga", 
                                  collapsible=F, 
                                  collapsed = F, 
                                  status="info",
                                  fluidRow(
                                    infoBoxOutput("vb_Registros", width = 6),
                                    infoBoxOutput("vb_coincidentes", width = 6))
                                  )
                              )
                       ),
                     br(),
                     h3("Calificación de los datos"),
                     box(title = NULL,
                         status = "info",
                         solidHeader = FALSE,
                         width = 12,
                         fluidRow(
                           column(3, h4("No Coincidentes"), dataTableOutput("NoCoincidentes"),downloadButton("DescargaTransacciones1", "Descargar")),
                           column(9, h4("Coincidentes"), dataTableOutput("Coincidentes"),downloadButton("DescargaTransacciones2", "Descargar"))
                           )
                         )
                     ),
            tabPanel("Resultados de Modificacion",
                     box(title = NULL,
                         status = "primary",
                         solidHeader = FALSE,
                         width = 12,
                         fluidRow(
                           valueBoxOutput("po_margen_total_1", width = 12))
                         ),
                     fluidRow(
                       column(4,
                              box(title = "Regional",
                                  status = "primary",
                                  solidHeader = FALSE,
                                  plotlyOutput("BarrasRegional_1_1"),
                                  width = "100%")),
                       column(4,
                              box(title = "Tipologia",
                                  status = "primary",
                                  solidHeader = FALSE,
                                  plotlyOutput("BarrasTipologia_1_1"),
                                  width = "100%")),
                       column(4,
                              box(title = "Cluster",
                                  status = "primary",
                                  solidHeader = FALSE,
                                  plotlyOutput("BarrasCluster_1_1"),
                                  width = "100%"))
                       ),
                     fluidRow(
                       column(6,
                              box(title = "Categoria",
                                  status = "primary",
                                  solidHeader = FALSE,
                                  plotlyOutput("BarrasCategoria_1_1"),
                                  width = "100%")),
                       column(6,
                              box(title = "Patologia",
                                  status = "primary",
                                  solidHeader = FALSE,
                                  plotlyOutput("BarrasPatologia_1_1"),
                                  width = "100%"))
                       )
                     )
            )
          ),
        ### Modificacones de Precios ----
        tabItem(
          tabName = "modif",
          tabsetPanel(
            tabPanel("Modificacion de Precios",
                     h3("Modifique el precio de los productos segun convenga"),
                     br(),
                     actionButton("runButton","Actualizar Precios"),
                     br(),br(),
                     box(title = NULL,
                         status = "primary",
                         solidHeader = FALSE,
                         width = 12,
                         column(3, selectInput(inputId = "ta_categoria", label = "Seleccione la categoria de producto", choices = cate, selected = "ETICOS")),
                         column(3, selectInput(inputId = "ta_cluster", label = "Seleccione el Cluster del producto",choices= c('AA','A', 'B','C','D','E')
                                               , selected = "AA")),
                         column(3, selectInput(inputId = "ta_diferencia", label = "Seleccione si el producto tiene sensibilidad a promocion",
                                               choices= c('Diferente','Indiferente'), selected = "Diferente"))
                         ),
                     box(title = NULL,
                         status = "primary",
                         solidHeader = T,
                         width = 12,
                         collapsible = T,
                         collapsed = F,
                         rHandsontableOutput('Precios'))),
            tabPanel("Resultados de Modificacion",
                     box(title = NULL,
                         status = "primary",
                         solidHeader = FALSE,
                         width = 12,
                         fluidRow(
                           valueBoxOutput("po_margen_total", width = 12))
                     ),
                     fluidRow(
                       column(4,
                              box(title = "Regional",
                                  status = "primary",
                                  solidHeader = FALSE,
                                  plotlyOutput("BarrasRegional_1"),
                                  width = "100%")),
                       column(4,
                              box(title = "Tipologia",
                                  status = "primary",
                                  solidHeader = FALSE,
                                  plotlyOutput("BarrasTipologia_1"),
                                  width = "100%")),
                       column(4,
                              box(title = "Cluster",
                                  status = "primary",
                                  solidHeader = FALSE,
                                  plotlyOutput("BarrasCluster_1"),
                                  width = "100%"))
                     ),
                     fluidRow(
                       column(6,
                              box(title = "Categoria",
                                  status = "primary",
                                  solidHeader = FALSE,
                                  plotlyOutput("BarrasCategoria_1"),
                                  width = "100%")),
                       column(6,
                              box(title = "Patologia",
                                  status = "primary",
                                  solidHeader = FALSE,
                                  plotlyOutput("BarrasPatologia_1"),
                                  width = "100%"))
                       )
                     )
            )
          # rHandsontableOutput('Precios'),
          # tags$style(type="text/css", "#table1 th {font-weight:bold;}")
        )
        ### ----
        )
    )


### Definicion ----
dashboardPage(
  skin = "blue",
  header,
  sidebar,
  body
)