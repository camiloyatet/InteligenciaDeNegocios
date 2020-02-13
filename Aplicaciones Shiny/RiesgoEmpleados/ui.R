### Header ----
header <- dashboardHeader(title = "Score de Riesgo Empleados", titleWidth = 300)

### Sidebar ----
sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(id="sbmenu",
                                        useShinyjs(),
                                        shinyjs::hidden(menuSubItem("dummy", tabName = "dummy")),
                                        br(),
                                        HTML('<center><img src="logo.png" width="200" height="40"></center>'),
                                        br(),
                                        menuItem("Descargas", tabName = "desc", icon = icon("download")),
                                        menuItem("Calificacion Masiva", tabName = "masiva", icon = icon("database")),
                                        menuItem("Calificacion Individual", tabName = "individual", icon = icon("user-check")),
                                        tags$hr(style="border-color: gainsboro;"),
                                        HTML('<center><img src="logo1.png" width="30" height="30"></center>'),
                                        tags$hr(style="border-color: gainsboro;")
                            )
)

### Body ----
body <- dashboardBody(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  ### Pestañas ----
  tabItems(
    ### Dummy ----
    tabItem("dummy",
            fluidRow(
              column(1),
              column(10,
                     h1("Estimacion de Riesgo de Empleados Colsubsidio"),
                     br(),br(),
                     h3("La presente aplicacion tiene por objetivo ser una herramienta de consulta para estimar el riesgo individual de cada empleado colsubsido a cometer una falta considerada como riesgosa.")
              ),
              column(1)
            )
    ),
    ### Consulta Masiva  ----
    tabItem(tabName = "desc",
            h2("Descargas de Listados"),
            br(),
            fluidRow(
              column(3, infoBoxOutput("vb_RiesgoMB", width = "100%"), downloadButton("DescargaRMB", "Descargar")),
              column(3, infoBoxOutput("vb_RiesgoB", width = "100%"), downloadButton("DescargaRB", "Descargar")),
              column(3, infoBoxOutput("vb_RiesgoM", width = "100%"), downloadButton("DescargaRM", "Descargar")),
              column(3, infoBoxOutput("vb_RiesgoA", width = "100%"), downloadButton("DescargaRA", "Descargar"))
            ),
            br(),br(),
            downloadButton("DescargaTotal", "Descargar Todos")
    ),
    ### Consulta Masiva  ----
    tabItem(tabName = "masiva",
            h2("Cargue de Informacion"),
            br(),
            fluidRow(
              column(5,
                     box(width = "100%", title= "Cargue del archivo a analizar", status="info",
                         fileInput("archivo", label= h6("Cargue la base de datos a analizar (csv)"), multiple = FALSE, placeholder="Explore el archivo", buttonLabel="Buscar",
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
                  column(9, h4("Coincidentes"), dataTableOutput("Calificados"),downloadButton("DescargaTransacciones2", "Descargar"))
                  )
                )
            ),
    ### Consulta Individual ----
    tabItem(tabName = "individual",
            h2("Cargue de Informacion"),
            dataTableOutput('test1'),
            fluidRow(
              column(2, textInput("Identificaicon", label = h5("Digite El número de identificaicon del Empleado")))
            ),
            box(title = NULL,
                status = "info",
                solidHeader = FALSE,
                width = 12,
                fluidRow(
                  infoBoxOutput("vb_Id", width = 3),
                  infoBoxOutput("vb_Nombre", width = 3),
                  infoBoxOutput("vb_Contrato", width = 3),
                  infoBoxOutput("vb_Ubicacion", width = 3)
                )
            ),
            box(title = NULL,
                status = "info",
                solidHeader = FALSE,
                width = 12,
                fluidRow(
                  infoBoxOutput("vb_Division", width = 3),
                  infoBoxOutput("vb_Subdivision", width = 3),
                  infoBoxOutput("vb_Area", width = 3),
                  infoBoxOutput("vb_Cargo", width = 3)
                  )
                ),
            box(title = NULL,
                status = "info",
                solidHeader = FALSE,
                width = 12,
                fluidRow(
                  infoBoxOutput("vb_Score", width = 6),
                  infoBoxOutput("vb_Riesgo", width = 6)
                  )
                )
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