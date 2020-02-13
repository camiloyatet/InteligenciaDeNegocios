### Header ----
header <- dashboardHeader(title = "Estimacion de Aportes", titleWidth = 300)

### Sidebar ----
sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(id="sbmenu",
                                        useShinyjs(),
                                        shinyjs::hidden(menuSubItem("dummy", tabName = "dummy")),
                                        br(),
                                        HTML('<center><img src="logo.png" width="200" height="40"></center>'),
                                        br(),
                                        menuItem("Estimacion de Aportes", tabName = "esti", icon = icon("database")),
                                        br(),
                                        selectInput("Actividad", h6("Seleccione la Actividad economica"), choices = unique(data$actividadeconomica)),
                                        selectInput("Sector", h6("Seleccione el sector economico"), choices = ""),
                                        selectInput("Tipo", h6("Seleccione el el tipo de empresa"), choices = ""),
                                        actionButton("go", "Aplicar Filtros", icon = icon("refresh")),
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
             column(12,
                    h1("Aplicacion de estimacion de Aportes a partir de la Guia Salarial"),
                    br(),br(),
                    h3("La presente aplicacion tiene por objetivo ser una herramienta de estimacionde los aportes que podria generar una empresa a partir del numero de trabajadores en diferentes posiciones"),
                    br(),
                    # tags$iframe(style="height:600px; width:100%", src="guia.pdf"),
                    br(),br(),br(),
                    h3("Para calcular el aporte estimado de una empresa: enla barra lateral de click en la pesatana de 'estimacion de aportes'. En la barra lateral podra seleccionar la actividad, sector economico y el tamano de la empresa"),
                    br(),
                    h3("Diligencie el numero de empleados en cada cargo, luego de click en el boton de calcular"),
                    br(),
                    h3("Se muestra una guia a continuacion"),
                    HTML('<center><img src="guia.gif"></center>'),
                    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                    h5("* Los acentos y caracteres especiales son intencionalmente omitidos")
              )
            )
    ),
    
    ### Consulta Masiva  ----
    tabItem(tabName = "esti",
            h2("Cargue de Informacion"),
            br(),
            box(title = NULL,
                status = "primary",
                solidHeader = F,
                width = 12,
                column(6, h3("Ingrese el numero de empleados por cargo"), rHandsontableOutput('tabla'),
                       br(),
                       actionButton("runButton","Calcular")),
                column(6, h3("Estimacion de Aporte"), 
                       valueBoxOutput("po_num_empleados", width = "100%"),
                       valueBoxOutput("po_aporte", width = "100%"),
                       # valueBoxOutput("po_remanente", width = "100%"),
                       valueBoxOutput("po_piramide1", width = "100%"),
                       valueBoxOutput("po_piramide2", width = "100%")
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