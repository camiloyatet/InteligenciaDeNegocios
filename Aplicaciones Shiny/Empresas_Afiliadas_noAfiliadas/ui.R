sidebar <- dashboardSidebar(width="300px",
                            sidebarMenu(id="menu1",disable = TRUE, br(),
                                        tags$img(src = "logo.png", height=60, width=300, align="center"),
                                        shiny::br(),
                                            shiny::br(),
                                        
                                        menuItem("MANUAL", tabName = "tab_manual", icon = icon("question-circle")),
                                        
                                        menuItem("EMPRESAS AFILIADAS", tabName = "AFILIADAS", icon = icon("fas fa-building"),
                                                 menuSubItem("Georreferenciación",tabName ="total",icon = icon("far fa-map"))
                                                 #menuSubItem("Supermercados",tabName ="sup",icon = icon("fas fa-shopping-cart")),
                                                 #menuSubItem("Club",tabName ="clu",icon = icon("fas fa-table-tennis")),
                                                 #menuSubItem("Salud",tabName ="sal",icon =icon("fas fa-user-md")),
                                                 #menuSubItem("Vivienda",tabName ="viv",icon = icon("fas fa-home")),
                                                 #menuSubItem("Drogueria",tabName ="dro" ,icon = icon("fas fa-capsules")),
                                                 #menuSubItem("EducaciÃ³n",tabName ="edu",icon = icon("fas fa-graduation-cap")),
                                                 #menuSubItem("Centro de servicios",tabName ="csc",icon = icon("fas fa-handshake" )),
                                                 #menuSubItem("RecreaciÃ³n",tabName ="rec",icon = icon("fas fa-theater-masks"))
                                                 
                                        ),
                                        
                                        menuItem("EMPRESAS NO AFILIADAS", tabName = "NOAFILIADAS", icon = icon("fas fa-industry"),
                                                 menuSubItem("Georreferenciación",tabName ="total2",icon = icon("far fa-map"))
                                        #          #menuSubItem("Supermercados",tabName ="sup2" ,icon = icon("fas fa-shopping-cart")),
                                        #          #menuSubItem("Club",tabName ="clu2",icon = icon("fas fa-table-tennis")),
                                        #          #menuSubItem("Salud",tabName ="sal2",icon =icon("fas fa-user-md")),
                                        #          #menuSubItem("Vivienda",tabName ="viv2",icon = icon("fas fa-home")),
                                        #          #menuSubItem("Drogueria",tabName ="dro2",icon = icon("fas fa-capsules")),
                                        #          #menuSubItem("EducaciÃ³n",tabName ="edu2",icon = icon("fas fa-graduation-cap")),
                                        #          #menuSubItem("Centro de servicios",tabName ="csc2",icon = icon("fas fa-handshake")),
                                        #          #menuSubItem("RecreaciÃ³n",tabName ="rec2",icon = icon("fas fa-theater-masks"))
                                        ),
                                        shiny::br(),
                                        
                                        conditionalPanel(
                                                condition = "input.menu1 == 'total'",
                                                sliderInput(inputId="distancia", label="Distancia en (KMS)", value=1, min = 0, max = 5, step = 0.1),
                                                #numericInput(inputId="empresa", label="Numero de empresas", value=150, min = 1, max = 93450, step = 1),
                                                textInput(inputId="razonsocial", label="Ingrese Empresa", value= "ACCION BPO S.A.S.", width = '400px')
                                                #selectizeInput(inputId="razonsocial",label="Ingrese Empresa",choices=emp_afiliadas$`RAZON SOCIAL`,size=2,
                                                #              selected= "ACCION BPO S.A.S.")
                                        ),
                                        conditionalPanel(
                                                condition="input.menu1 == 'total2'",
                                                sliderInput(inputId="distancia2", label="Distancia en (KMS)", value=1, min = 0, max = 5, step = 0.1),
                                                #numericInput(inputId="empresa2", label="Numero de empresas", value=150, min = 1, max = 30034, step = 1),
                                                textInput(inputId="razonsocial2", label="Ingrese Empresa", value= "WEATHERFORD COLOMBIA LIMITED", width = '400px')
                                        )
                                        
                                        
                                        
                                        
                                        
                            )
)

body <- dashboardBody(
        tabItems(
                #tabItem(tabName = "MAP",
                #h2("Filtros"),
                #br(),
                #h4("El presente documento permite consultar el siguiente producto mas probable para cada cliente contenido en la base de datos"),
                #br(),
                #h4("Seleccione las caracteristicas de la poblacion deseadas y luego de clic sobre el boton 'aplicar filtros'"),
                #br(),
                #br()
                #fluidRow(
                #infoBox(
                #"Orders", uiOutput("orderNum2"), "Subtitle", icon = icon("credit-card")
                #))
                tab_manual <- tabItem(
                        tabName = "tab_manual",
                        fluidRow(
                                column(1),
                                column(10,
                                       h1("CONSULTA DE INFRAESTRUCTURA Y CONVENIOS TMS POR EMPRESA"),
                                       br(),
                                       h3("La presente aplicación permite consultar las empresas afiliadas y no afiliadas a la caja de compensación Colsubsidio frente a su infraestructura y convenios TMS"),
                                       div(img(src = "gifafiliados_noafiliados.gif", width = 1100)),
                                       h3("PASOS"),
                                       br(),
                                       h3("1. Selecciona la opción a empresa afiliada o no afiliada"),
                                       h3("2. Selecciona la distancia a la cual quiere evaluar"),
                                       h3("3. Ingresa en NIT de la empresa"),
                                       h3("4. Puede evaluar los resultados a través de mapas uno para infraestructura Colsubsidio y uno para Convenios tms"),
                                       h3("5. Puede evaluar los resultados a través de tablas destinadas a la infraestructura Colsubsidio y los Convenios tms"),
                                       br()
                                ),
                                column(1)
                        )
                ),
                
                #),
                tabItem(tabName = "total2",
                        shiny::br(),
                        shiny::h2("EMPRESAS NO AFILIADAS"),
                        shiny::br(),
                        shiny::br(),
                        #h4("A continuacion se presentan las diez primeras opcines de producto mas probables para cada cliente. 
                        #\n Es posible descargar el listado completo en formato csv"),
                        shiny::br(),
                        shiny::br(),
                        
                        fluidRow(shinydashboard::box(title = "EMPRESAS NO AFILIADAS VS INFRAESTRUCTURA",
                                     collapsible = T,
                                     width="1300px",
                                     height="460px",
                                     solidHeader = TRUE,
                                     status = "primary",
                                     footer = "Infraestructura más próxima a la empresa",
                                     leafletOutput("map.no",width = "1000px")),
                                 align = "center"),
                        
                        shiny::br(),
                        shiny::br(),
                        shiny::br(),
                        fluidRow(shinydashboard::box(title = "EMPRESAS NO AFILIADAS VS CONVENIOS",
                                     collapsible = T,
                                     width="1300px",
                                     height="460px",
                                     solidHeader = TRUE,
                                     status = "danger",
                                     footer = "Centros de servicio mas proximos a la empresa",
                                     leafletOutput("map.convenio.no_afi",width = "1000px")),
                                 align = "center"
                        ),
                        
                        shiny::br(),
                        shiny::br(),
                        shiny::br(),
                        fluidRow(
                                shinydashboard::box(title ="EMPRESAS NO AFILIADAS FRENTE A LA INFRAESTRUCTURA DE COLSUBSIDIO",
                                    status = "primary",
                                    width="1300px",
                                    height="300px",
                                    solidHeader = TRUE,
                                    DT::dataTableOutput('tablano'),collapsible = T),
                                align = "center"),
                        
                        shiny::br(),
                        shiny::br(),
                        
                        fluidRow(shinydashboard::box(title = "EMPRESAS NO AFILIADAS FRENTE A LA INFRAESTRUCTURA DE COLSUBSIDIO \n ORDEN DE CERCANIA",
                                     status = "primary",
                                     width="1300px",
                                     height="800px",
                                     solidHeader = TRUE,
                                     DT::dataTableOutput('csc.no'),collapsible = T),
                                 align = "center"),

                        fluidRow(
                                shinydashboard::box(title ="EMPRESAS NO AFILIADAS FRENTE A LOS CONVENIOS DE COLSUBSIDIO",
                                    status = "danger",
                                    width="1300px",
                                    height="300px",
                                    solidHeader = TRUE,
                                    DT::dataTableOutput('tabla3.no'),collapsible = T),
                                align = "center"),
                        
                        shiny::br(),
                        shiny::br(),

                        fluidRow(shinydashboard::box(title = "EMPRESAS NO AFILIADAS FRENTE A LOS CONVENIOS DE COLSUBSIDIO \n ORDEN DE CERCANIA",
                                     status = "danger",
                                     width="1300px",
                                     height="720px",
                                     solidHeader = TRUE,
                                     DT::dataTableOutput('tabla4.no'),collapsible = T),
                                 align = "center")
                        
                        
                ),
        
                
                # tabItem(tabName = "csc2",
                #         fluidRow(
                #                 shiny::br(),
                #                 shiny::br(),
                #                 column(12, DT::dataTableOutput('piramide1.no',width =800)),
                #                 align = "center"),      
                #         
                #         fluidRow(
                #                 shiny::br(),
                #                 shiny::br(),
                #                 column(12, DT::dataTableOutput('piramide2.no',width = 800)),
                #                 align = "center")
                # ),
                
                tabItem(tabName = "total",
                        shiny::br(),
                        shiny::h2("EMPRESAS AFILIADAS"),
                        #shiny::span(textOutput("message"), style="color:red"),
                        shiny::br(),
                        shiny::br(),
                        #h4("A continuacion se presentan las diez primeras opcines de producto mas probables para cada cliente. 
                        #           \n Es posible descargar el listado completo en formato csv"),
                        shiny::br(),
                        shiny::br(),
                        
                        fluidRow(shinydashboard::box(title = "EMPRESAS AFILIADAS VS INFRAESTRUCTURA",
                                     collapsible = T,
                                     width="1300px",
                                     height="460px",
                                     solidHeader = TRUE,
                                     status = "primary",
                                     footer = "Infraestructura más próxima a la empresa",
                                     leafletOutput("map",width = "1000px")),
                                 align = "center"),
                        shiny::br(),
                        shiny::br(),
                        shiny::br(),
                        shiny::br(),
                        shiny::br(),
                        shiny::br(),
                        
                        fluidRow(shinydashboard::box(title = "EMPRESAS AFILIADAS VS CONVENIOS",
                                     collapsible = T,
                                     width="1300px",
                                     height="460px",
                                     solidHeader = TRUE,
                                     status = "danger",
                                     footer = "Convenios más próximos a la empresa",
                                     leafletOutput("map.convenio.afi",width = "1000px")),
                                 align = "center"
                        ),
                        shiny::br(),
                        shiny::br(),
                        shiny::br(),
                        fluidRow(
                                shinydashboard::box(title ="EMPRESAS AFILIADAS FRENTE A LA INFRAESTRUCTURA DE COLSUBSIDIO",
                                    status = "primary",
                                    width="1300px",
                                    height="300px",
                                    solidHeader = TRUE,
                                    DT::dataTableOutput('tabla'),collapsible = T),
                                align = "center"),
                        shiny::br(),
                        shiny::br(),
                        
                        fluidRow(shinydashboard::box(title = "EMPRESAS AFILIADAS FRENTE A LA INFRAESTRUCTURA DE COLSUBSIDIO \n ORDEN DE CERCANIA",
                                     status = "primary",
                                     width="1300px",
                                     height="540px",      
                                     solidHeader = TRUE,
                                     DT::dataTableOutput('csc'),collapsible = T),
                                 align = "center"),
                        
                        fluidRow(
                                shinydashboard::box(title ="EMPRESAS AFILIADAS FRENTE A LOS CONVENIOS DE COLSUBSIDIO",
                                    status = "danger",
                                    width="1300px",
                                    height="300px",
                                    solidHeader = TRUE,
                                    DT::dataTableOutput('tabla4'),collapsible = T),
                                align = "center"),
                        
                        fluidRow(shinydashboard::box(title = "EMPRESAS AFILIADAS FRENTE A LOS CONVENIOS DE COLSUBSIDIO \n ORDEN DE CERCANIA",
                                     status = "danger",
                                     width="1300px",
                                     height="720px",      
                                     solidHeader = TRUE,
                                     DT::dataTableOutput('tabla3'),collapsible = T),
                                 align = "center") #,
                        #fluidRow(
                                #shinydashboard::box(title ="PRUEBA",
                                    #status = "danger",
                                    #width="1300px",
                                    #height="300px",
                                    #solidHeader = TRUE,
                                    #DT::dataTableOutput('prueba'),collapsible = T),
                                #align = "center"),
                        
                        #fluidRow( verbatimTextOutput("placeholder", placeholder = TRUE))
                        
                        
                )#,
                
                # tabItem(tabName = "csc",
                #         fluidRow(
                #                 shiny::br(),
                #                 shiny::br(),
                #                 column(12, DT::dataTableOutput('piramide1',width =800)),
                #                 align = "center"),      
                #         
                #         fluidRow(
                #                 shiny::br(),
                #                 shiny::br(),
                #                 column(12, DT::dataTableOutput('piramide2',width = 800)),
                #                 align = "center")
                # )
                
                
                
        )
)

dashboardPage(
        dashboardHeader(title = "EMPRESAS AFILIADAS Y NO AFILIADAS FRENTE A LA INFRAESTRUCTURA Y CONVENIOS", titleWidth = 1500),
        sidebar,
        body,
        tags$head(tags$style(HTML('
                           .skin-blue .main-header .logo {
                           background-color: #3c8dbc;
                           }
                           .skin-blue .main-header .logo:hover {
                           background-color: #3c8dbc;
                           }
                           
                           .box.box-solid.box-primary{
                           border-bottom-color:#3c8dbc;
                           border-left-color:#3c8dbc;
                           border-right-color:#3c8dbc;
                           border-top-color:#3c8dbc;
                           background:#FFFFFF
                           
                           ')))
)
