### Header ----
header <- dashboardHeader(title = "Estimacion de Aportes (Atraccion)", titleWidth = 300)

### Sidebar ----
sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(id="sbmenu",
                                        useShinyjs(),
                                        shinyjs::hidden(menuSubItem("dummy", tabName = "dummy")),
                                        br(),
                                        HTML('<center><img src="logo.png" width="200" height="40"></center>'),
                                        br(),
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
            br(),
            fluidRow(
              tags$iframe(src = 'Dummy.html', 
                          width = '100%', height = '800px', 
                          frameborder = 0, scrolling = 'auto'
              )
            )
    ),
    
    ### Consulta Masiva  ----
    tabItem(tabName = "masiva",
            h2("Cargue de Informacion"),
            br(),
            fluidRow(
              column(4,
                     box(width = "100%", title= "Cargue del archivo a analizar", status="info",
                       fileInput("archivo", label= h6("Cargue la base de datos a analizar (csv)"), multiple = FALSE, placeholder="Explore el archivo", buttonLabel="Buscar",
                                 accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")
                                 )
                       )
                     ),
              column(8, 
                     box(width = "100%", 
                         title= "Resultado de la Carga", 
                         collapsible=F, 
                         collapsed = F, 
                         status="info",
                         fluidRow(
                           infoBoxOutput("vb_Registros", width = 4),
                           infoBoxOutput("vb_coincidentes", width = 4),
                           infoBoxOutput("vb_nocoincidentes", width = 4))
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
                  column(4, h4("No Coincidentes"), dataTableOutput("NoCoincidentes"),downloadButton("DescargaTransacciones1", "Descargar")),
                  column(8, h4("Coincidentes"), dataTableOutput("Calificados"),downloadButton("DescargaTransacciones2", "Descargar"))
                  )
                )
            ),
    ### Consulta Individual ----
    tabItem(tabName = "individual",
            h2("Cargue de Informacion"),
            dataTableOutput('test1'),
            fluidRow(
              column(2, textInput("idempresa", label = h5("Digite la Identificacion de la Empresa"))),
              column(2, textInput("RazonSocial", label = h5("Digite la Razón Social  de la Empresa"))),
              column(2, numericInput("Empleados", label = h5("Digite El número de Empleados de la Empresa"), value = 1, min = 1)),
              column(2, textInput("CIIU", label = h5("Digite código CIIU de la Empresa")))
            ),
            box(title = NULL,
                status = "info",
                solidHeader = FALSE,
                width = 12,
                fluidRow(
                  infoBoxOutput("vb_NIT", width = 3),
                  infoBoxOutput("vb_RazonSocial", width = 3),
                  infoBoxOutput("vb_Empleados", width = 3),
                  infoBoxOutput("vb_CIIU", width = 3)
                  )
                ),
            box(title = NULL,
                status = "info",
                solidHeader = FALSE,
                width = 12,
                fluidRow(
                  infoBoxOutput("vb_Actividad", width = 3),
                  infoBoxOutput("vb_Sector", width = 3),
                  infoBoxOutput("vb_Grupo", width = 3),
                  infoBoxOutput("vb_Descripcion", width = 3)
                  )
                ),
            box(title = NULL,
                status = "info",
                solidHeader = FALSE,
                width = 12,
                fluidRow(
                  infoBoxOutput("vb_Piramide1", width = 3),
                  infoBoxOutput("vb_Piramide2", width = 3),
                  infoBoxOutput("vb_Aporte", width = 3),
                  infoBoxOutput("vb_Remanante", width = 3)
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