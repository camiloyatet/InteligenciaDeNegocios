# -------------------------------------------------------- CONSTANTES --------------------------------------------------------

# -------------------------------------------------------- HEADER -----------------------------------------------------------
header <- dashboardHeaderPlus(
    title = tagList(img(src = "35pxAmarillo.png"),span(class = "logo-min","Colsubsidio")),
    enable_rightsidebar = F
)

# -------------------------------------------------------- SIDEBAR -----------------------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(disable = TRUE,br(),
                menuItem("Informacion", tabName = "tab_base_datos", icon = icon("database")),
                menuItem("Desciptivo", tabName = "tabDescriptivo", icon = icon("chart-pie")),
                menuItem("Filtro", tabName = "tabDescriptivo", icon = icon("filter"), 
                         sliderTextInput(
                             inputId = "Id101",
                             label = "Rango de edad:", 
                             choices = seq(from = 0,
                                           to = 90,
                                           by = 5),
                             selected = c(0,90),
                             from_min = 0, 
                             from_max = 90,
                             grid = T
                         ),
                         actionButton(
                             inputId = "Aplicar",
                             label = "Aplicar",
                             icon = icon("filter")
                         )
                ),
                menuItem("Manual", tabName = "tab_manual", icon = icon("book"))
    )
)

# -------------------------------------------------------- BODY --------------------------------------------------------------
body <- dashboardBody(
    tabItems(
        tabItem("tab_base_datos",
                fluidRow(
                    column(6,
                           box(width = 12,height = 280,
                               h4('Importe el archivo CSV"'),
                               prettyRadioButtons(
                                   inputId = "radio",
                                   label = "Indique el tipo de llave", 
                                   choices = c("id_Persona" = 1, "Numero Documento" = 2),
                                   icon = icon("check"),
                                   bigger = TRUE,
                                   status = "info",
                                   animation = "jelly",
                                   inline = T
                               ),
                               fileInput("file1", "Base de datos", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                               actionButton(
                                   inputId = "success",
                                   label = "Cruzar informacion",
                                   icon = icon("exchange-alt")
                               ),
                               downloadButton('downloadData', 'Download')
                           )
                    )
                ),
                fluidRow(
                    column(3,uiOutput("TotalRegistros")),
                    column(3,uiOutput("TotalRegistrosUnicos"))
                ),
                fluidRow(
                    column(3,tableOutput('datatTablecedula')),
                    column(3,tableOutput('datatTablecedula2'))
                )
        ),
        tabItem("tabDescriptivo",
                fluidRow(
                    column(3,infoBoxOutput("totalAfiliados", width = 12)),
                    column(3,infoBoxOutput("totalAfiliadosCategoriaA", width = 12)),
                    column(3,infoBoxOutput("totalAfiliadosCategoriaB", width = 12)),
                    column(3,infoBoxOutput("totalAfiliadosCategoriaC", width = 12))
                ),
                boxPlus(
                    closable = F, 
                    width = NULL,
                    enable_label = F,
                    label_text = 1,
                    label_status = "danger",
                    status = "danger", 
                    solidHeader = F, 
                    collapsible = F,loadingState(),
                    plotlyOutput('Piramide', height = "600px")
                ),
                boxPlus(width = 12,
                        fluidRow(
                            column(4,loadingState(), plotlyOutput('PiramideCategoriaA', height = "300")),
                            column(4,loadingState(), plotlyOutput('PiramideCategoriaB', height = "300")),
                            column(4,loadingState(), plotlyOutput('PiramideCategoriaC', height = "300"))
                        ),
                        br(),
                        fluidRow(
                            column(3,loadingState(), plotlyOutput('PiramideSegmentoBasico', height = "300")),
                            column(3,loadingState(), plotlyOutput('PiramideSegmentoMedio', height = "300")),
                            column(3,loadingState(), plotlyOutput('PiramideSegmentoJoven', height = "300")),
                            column(3,loadingState(), plotlyOutput('PiramideSegmentoAlto', height = "300"))
                        )    
                ),
                fluidRow(
                    column(3,infoBoxOutput("SegmentoPoblacionalBasico", width = 12)),
                    column(3,infoBoxOutput("SegmentoPoblacionalJoven", width = 12)),
                    column(3,infoBoxOutput("SegmentoPoblacionalMedio", width = 12)),
                    column(3,infoBoxOutput("SegmentoPoblacionalAlto", width = 12))
                ),
                boxPlus(width = 12,
                        fluidRow(
                            column(4,plotlyOutput("PieSegmento")),
                            column(4,plotlyOutput("PieCategoria")),
                            column(4,plotlyOutput("PieGenero"))
                        )    
                ),

                boxPlus(
                    title = "INFORMACION SOCIO DEMOGRAFICA - HORIZONTAL BAR",
                    closable = F, 
                    width = 12,
                    enable_label = F,
                    label_text = 1,
                    label_status = "danger",
                    status = "danger", 
                    solidHeader = F, 
                    collapsible = F,
                    plotlyOutput("HorizontalBarCategoria",height = 300),
                    plotlyOutput("HorizontalBarCategoria2",height = 300)
                ),
                boxPlus(
                    title = "INFORMACION",
                    closable = F, 
                    width = 12,
                    enable_label = F,
                    label_text = 1,
                    label_status = "danger",
                    status = "danger", 
                    solidHeader = F, 
                    collapsible = F,
                    fluidRow(
                        column(6,plotlyOutput("plotGrupoFamiliar")),
                        column(6,plotlyOutput("plotEstratoPersona"))
                    ),                    
                    fluidRow(
                        column(6,plotlyOutput("plotPiramide1")),
                        column(6,plotlyOutput("plotPiramide2"))
                    )
                ),
                
                

                boxPlus(
                    title = "DATOS GENERALES",
                    closable = F, 
                    width = 12,
                    enable_label = F,
                    label_text = 1,
                    label_status = "danger",
                    status = "danger", 
                    solidHeader = F, 
                    collapsible = F,
                    fluidRow(
                        column(3,h4("Distribución Piramide"),tableOutput('PiramideTable')),
                        column(3,h4("Distribución Segmento/Categoria"),tableOutput('Categoria')),
                        column(3,h4("Distribución Grupo familiar"),tableOutput('SegmentoGrupoFamiliar')),
                        column(3,h4("Distribución Estrato"),tableOutput('EstratoPersona'))
                    )
                )
        )
    )
)

# -------------------------------------------------------- DASHBOARD --------------------------------------------------------------
dashboardPagePlus( 
    header,
    sidebar,
    body,
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    )
)
