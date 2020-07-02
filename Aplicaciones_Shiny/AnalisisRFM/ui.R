# -------------------------------------------------------- CONSTANTES --------------------------------------------------------
botonActualizar <- actionButton("btnActualizar", "Aplicar", class = "btn-primary")
botonDownload <- downloadButton("downloadData", "Descargar calificación")
Agrup1 <- textOutput("NombreAgrupador1")

# -------------------------------------------------------- HEADER -----------------------------------------------------------
header <- dashboardHeader(title = "Afiliados", titleWidth = 250)

# -------------------------------------------------------- SIDEBAR -----------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(disable = TRUE, br(),
              tags$img(src = "Logo.png", height=40, width=200, align="center"),
              menuItem("Informacion", tabName = "tab_base_datos", icon = icon("database")),
              menuItem("Analisis RFM", tabName = "tab_analisis", icon = icon("chart-pie")),
              menuItem("Filtros", tabName = "tab_filtro", icon = icon("sliders-h"),
                       radioButtons("radio", label = h5("Calculo de M_Score"),
                                    choices = list("Monto total" = 1, "Promedio (Monto/Transacciones)" = 2), 
                                    selected = 1),
                       pickerInput("Agrupador1Pick",h5("Agrupador 1"),choices="",multiple =T,
                                   options = list(
                                     `actions-box` = TRUE,
                                     `selected-text-format` = "count > 2"
                                   )
                       ),
                       pickerInput("Agrupador2Pick",h5("Agrupador 2"),choices="",multiple =T,
                                   options = list(
                                     `actions-box` = TRUE,
                                     `selected-text-format` = "count > 2"
                                   )
                       ),
                       botonActualizar),
              menuItem("Manual", tabName = "manual", icon = icon("book"))
  )
)

# -------------------------------------------------------- BODY --------------------------------------------------------------
body <- dashboardBody(
  tabItems(
    
    # -------------------------------------------------------- ANALISIS ------------------------------------------------------
    tabItem("tab_analisis",
            # -------------------- DATABOX -----------------------------
            fluidRow(
              column(3,infoBoxOutput("numCLientes", width = 12)),
              column(3, infoBoxOutput("numCLientesAlto", width = 12)),
              column(3, infoBoxOutput("numCLientesMedio", width = 12)),
              column(3, infoBoxOutput("numCLientesBajo", width = 12))
            ),
            # -------------------- RFM -----------------------------
            fluidRow(
              box(title = "Analisis RFM" , width = 12, solidHeader=TRUE,status="primary",
                  column(9,
                         plotOutput("ResumenRFM",height = "680px"),
                         botonDownload
                  ),
                  column(3,
                         verticalLayout(
                           h4("Frecuencia (Transacciones)"),
                           box(status="primary",width = 20,tableOutput('TablaFrecuencia')),
                           h4("Recencia (Dias)"),
                           box(status="primary",width = 20,tableOutput('TablaRecencia')),
                           h4("Monto"),
                           box(status="primary",width = 20,tableOutput('TablaMonto'))
                         )
                  )
              )
            ),
            # -------------------- output plot -----------------------------
            fluidRow(
              column(3,box(title = "Distribución de clientes por calificación",width = 40,
                           solidHeader=TRUE,
                           status="primary",
                           loadingState(),
                           plotlyOutput("tortaDistribucionCalificacion", height = 400))),
              column(3,box(title = "Consumo promedio segun calificación" ,
                           width = 40,
                           solidHeader=TRUE,
                           status="primary",
                           loadingState(),
                           plotlyOutput("barTicketPromedio",height = 400))),
              column(6,box(title = paste0("Ingresos por calificación"),
                           width = 40,
                           solidHeader=TRUE,
                           status="primary",
                           loadingState(),
                           plotlyOutput("HBarChar", height = 400))
              )
            ),
            # -------------------- Tree Map -----------------------------
            box(title = "Ingresos por agrupador 1",
                width = 40,
                solidHeader=TRUE,
                status="primary",
                plotOutput("threemapAgrupador1",height="600px")),
            box(title = "Ingresos por agrupador 2",
                width = 40,
                solidHeader=TRUE,
                status="primary",
                plotOutput("threemapAgrupador2",height="600px"))
    ),
    # -------------------------------------------------------- CARGUE ------------------------------------------------------
    tabItem("tab_base_datos",
            h4("Informacion para RFM"),
            fileInput("file1", "Base de datos", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
            fluidRow(
              column(5,h4("Estructura"),tableOutput("contents")),
              column(5,h4("Agrupadores"),tableOutput("contentsAgrupador"))
            )
    ),
    tabItem("manual",
            fluidRow(
              column(1),
              column(10,
                     h1("Ficha de consumo"),
                     br(),
                     h3("La presente aplicacion, le permitira realizar analisis RFM, utilizar para segmentar grupos clientes , se nutre del comportamiento historico transaccional. Tendra la opcion de descargar la base de datos calificada segun los Score generados en el analisis. Importante: Tener en cuenta que el formato de la fecha sea correcto."),
                     br(),
                     div(img(src = "Manual.gif", width = 1200)),

                     br()
              ),
              column(1)
            )
    )
  )
)

# -------------------------------------------------------- DASHBOARD --------------------------------------------------------------
dashboardPage(skin = "blue",
              header,
              sidebar,
              body,
              tags$head(
                tags$style(
                  HTML('
                        .skin-blue .main-header .logo {
                                              background-color: #000000;}
                        .skin-blue .main-header .navbar {
                                              background-color: #000000;}
                        #final_text {
                        text-align: center;}
                        
                        div.box-header {
                        text-align: center;
                        background-color: #000000;}
                                                        
                        .box.box-solid.box-primary>.box-header {
                        color:#FFFFFF;
                        background:#000000}'
                  )
                )
              )
)
