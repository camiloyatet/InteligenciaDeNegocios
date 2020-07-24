### Header ----
header <- dashboardHeader(title = "Transacciones Droguerias", titleWidth = 300)

### Sidebar ----
sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(shinyjs::useShinyjs(),
                                        disable = TRUE, br(),
                                        tags$img(src = "logo.png", height=40, width=300, align="center"),
                                        br(),
                                        tags$hr(),
                                        shinyjs::hidden(menuSubItem("dummy", tabName = "dummy")),
                                        menuItem("Generalidades", tabName = "general", icon = icon("receipt")),
                                        menuItem("Analisis RFM", tabName = "rfm", icon = icon("user-check")),
                                        menuItem("Tops Dinamicos", tabName = "tops", icon = icon("sort-amount-down")),
                                        menuItem("Filtros", icon = icon("filter"),
                                                 dateRangeInput("Fecha", h6("Periodo de Análisis"),start=fec_min,end=fec_max,
                                                                format ="dd/mm/yyyy",separator = "a", language = "es", weekstart = 1,
                                                                min=fec_min, max=fec_max),
                                                 selectizeInput("Categoria", h6("Categoría de Producto"),choices = cate, selected="TODAS", multiple = T),
                                                 selectizeInput("Patologia", h6("Patología de Producto"),choices = pato, selected="TODAS", multiple = T),
                                                 selectizeInput("Habeas", h6("Autorización de Contacto"), choices = list("Si"=1, "No"=0, "Todos"="Todos") , selected="Todos", multiple = F),
                                                 fileInput("plus", label= h6("Cargue el Listado de Productos (csv)"), multiple = FALSE, placeholder="Explore el archivo", buttonLabel="Buscar",
                                                           accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
                                                 selectizeInput("Proveedor", h6("Proveedor"),choices = prov, selected="TODOS", multiple = T),
                                                 tags$hr(style="border-color: gainsboro;"),
                                                 selectizeInput("Afiliado", h6("Cliente Afiliado"),choices = afil, selected="Todos"), ###
                                                 selectizeInput("Cupo", h6("Cliente con TMS"),choices = cred, selected="TODAS"),
                                                 selectizeInput("CategoriaAfil", h6("Categoría de Afiliacion"),choices = cata, selected="TODAS"),
                                                 sliderInput("Edad", h6("Edad (Afiliado)"), min = eda_min, max=eda_max, value = c(eda_min,eda_max)),
                                                 selectizeInput("Genero", h6("Genero (Afiliado)"),choices = gene, selected="Todos"),
                                                 selectizeInput("Segmento", h6("Segmento Poblacional"), choices = segm , selected="TODOS", multiple = T),
                                                 tags$hr(style="border-color: gainsboro;"),
                                                 selectizeInput("Recencia", h6("Recencia"), choices=scor, multiple=T, selected="Todos"),
                                                 selectizeInput("Frecuencia", h6("Frecuencia"), choices=scor, multiple=T, selected="Todos"),
                                                 selectizeInput("Monto", h6("Monto"), "", choices=scor, multiple=T, selected="Todos"),
                                                 br(),
                                                 actionButton("go", "Aplicar Filtros", icon = icon("refresh"))
                                                 ),
                                        tags$hr(),
                                        HTML('<center><img src="ues.png" width="150" height="40"></center>')
                                        )
                            )

### Body ----
body <- dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    tabItems(
        ### Dummy ----
        tabItem("dummy",
                fluidRow(
                    column(1),
                    column(10,
                           h1("Analisis transaccional Droguerias"),
                           br(),br(),
                           h3("La presente aplicacion tiene por objetivo ser una herramienta de consulta de las transacciones de las Droguerias Colsubsidio. Seleccione en la barra lateral los filtros a aplicar.")
                    ),
                    column(1)
                    )
                ),
        ### Descriptivos generales ----
        tabItem("general",
                h3("Conteos Generales"),
                fluidRow(
                    column(2, valueBoxOutput("Transacciones", width = "100%"),
                           downloadButton("DescargaTransacciones", "Descargar")),
                    column(2, valueBoxOutput("Clientes", width = "100%"),
                           downloadButton("DescargaClientes", "Descargar")),
                    column(2, valueBoxOutput("Productos", width = "100%"),
                           downloadButton("DescargaProductos", "Descargar")),
                    column(2, valueBoxOutput("VentaBruta", width = "100%")),
                    column(2, valueBoxOutput("VentaNeta", width = "100%")),
                    column(2, valueBoxOutput("TicketPromedio", width = "100%"))
                           ),
              h3("Magnitudes por Sucursal"),
                fluidRow(
                    column(4,
                           selectizeInput("VariableCol", h4("Variable de Color"),
                                          choices = list("Clientes"="Clientes",
                                                         "Venta Bruta"="Venta_Bruta",
                                                         "Venta Neta"="Venta_Neta"), selected="Clientes")
                    ),
                    column(4,
                           selectizeInput("VariableRad", h4("Variable de Radio"),
                                          choices = list("Clientes"="Clientes",
                                                         "Venta Bruta"="Venta_Bruta",
                                                         "Venta Neta"="Venta_Neta"), selected="Venta_Bruta")
                    ),
                    column(4,
                           sliderInput("Tamano", label = h4("Radio"), min = 1, max = 20, step =1,value = 5)
                    )
                ),
                br(),
                leafletOutput("MapaPunto"),
                br()
                ),
        ### Analisis RFM ----
        tabItem("rfm",
                h4("Distribución de Pacientes por Scores RFM"),
                plotOutput("ResumenRFM",height = "600px"),
                br(),
                h4("Valores Scores RFM"),
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
                ),
        ### Tops Dinamicos ----
        tabItem("tops",
                fluidRow(
                    column(4,
                           selectInput("VariableTop", label = h4("Seleccione la Variable del TOP"),
                                       choices = list("Patología"="Patologia",
                                                      "Categoría"="Categoria",
                                                      "Sucursal"="nombre_Sucursal",
                                                      "Producto" = "prod_nombre",
                                                      "Proveedor"="prod_proveedor_nombre",
                                                      "Jerarquía 1"="jerarquia1",
                                                      "Jerarquía 2"="jerarquia2",
                                                      "Principio Activo"="principioconcatenado",
                                                      "Paciente"="Id_Cliente"))
                           ),
                    column(3,
                           numericInput("top", label=h4("Número de filas a Mostrar"),
                                        10, min = 1, max = 100)
                           ),
                    column(5,
                           selectInput("VariableOrden", label=h4("Seleccione la Variable de Orden"),
                                       choices=list(
                                           "Pacientes"="Pacientes",
                                           "Venta Bruta"="VentaBruta",
                                           "Venta Neta"="VentaNeta",
                                           "Frecuencia de Compra"="Frec",
                                           "Unidades"="Cantidad",
                                           "Uso Promedio"="UsoPromedio",
                                           "Venta Bruta Promedio"="VentaBPromedio",
                                           "Venta Neta Promedio"="VentaNPromedio"))
                           )
                    ),
                dataTableOutput("TopDInamico")
                )
        ### ----
        )
    )

### Definicion ----
dashboardPage(
    header,
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
