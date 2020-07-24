
#Menu consumo----
menu_consumo<- function(ind_act, ind_anu, emp_act, emp_anu){
  box(
    width = 12,
    height = 75,
    style = "text-align: center",
    fluidPage( tabsetPanel(
      tabPanel("Consumo individual", ind_anu),
      tabPanel("Consumo empresarial", emp_anu)
    )
    )
  )
}
#Información-----

box_informacion_empresa_credito<-
  fluidRow(
    column(4,infoBoxOutput("razon_social_credito",width = 12)),
    column(4,infoBoxOutput("nit_empresarial_credito",width = 12)),
    column(4,infoBoxOutput("estado_empresa_credito",width = 12))
  )
box_informacion_empresa_educacion<-
  fluidRow(
    column(4,infoBoxOutput("razon_social_educacion",width = 12)),
    column(4,infoBoxOutput("nit_empresarial_educacion",width = 12)),
    column(4,infoBoxOutput("estado_empresa_educacion",width = 12))
  )
box_informacion_empresa_mercadeo<-
  fluidRow(
    column(4,infoBoxOutput("razon_social_mercadeo",width = 12)),
    column(4,infoBoxOutput("nit_empresarial_mercadeo",width = 12)),
    column(4,infoBoxOutput("estado_empresa_mercadeo",width = 12))
  )
box_informacion_empresa_recreacion<-
  fluidRow(
    column(4,infoBoxOutput("razon_social_recreacion",width = 12)),
    column(4,infoBoxOutput("nit_empresarial_recreacion",width = 12)),
    column(4,infoBoxOutput("estado_empresa_recreacion",width = 12))
  )
box_informacion_empresa_salud<-
  fluidRow(
    column(4,infoBoxOutput("razon_social_salud",width = 12)),
    column(4,infoBoxOutput("nit_empresarial_salud",width = 12)),
    column(4,infoBoxOutput("estado_empresa_salud",width = 12))
  )
box_informacion_empresa_vivienda<-
  fluidRow(
    column(4,infoBoxOutput("razon_social_vivienda",width = 12)),
    column(4,infoBoxOutput("nit_empresarial_vivienda",width = 12)),
    column(4,infoBoxOutput("estado_empresa_vivienda",width = 12))
  )
#Menu caja de unidades de negocios----

#Texto de input----
input_numero_documento <- textInput("numero_documento", label = h4("Nit de la empresa"), value = "NIT8600073361", width = NULL, placeholder = NULL)

#Contenido de Help (Manual)

tab_manual <- tabItem(
  tabName = "tab_manual",
  fluidRow(
    column(1),
    column(10,
           h1("Ficha de consumo"),
           br(),
           h3("La presente aplicación permite consultar los consumos empresarial e individual realizados por una empresa específica, tales están segmentados por UES."),
           div(img(src = "example1.gif", width = 1100)),
           h3("1. Ingresar el tipo de documento de la empresa junto con su NIT, tal como lo muestra el ejemplo.Si lo requiere, puede limitar el lapso de tiempo por meses en la barra inferior."),
           h3("2. Seleccionar el botón de búsqueda para cargar la información de la empresa ingresada."),
           h3("3. La información se actualiza según la unidad seleccionada dentro del recuadro naranja."),
           h3("4. Al interior del óvalo verde, podrá indicar el tipo de consumo que deseé"),
           br()
    ),
    column(1)
  )
)
#Credito----
#tab_credito_individual_anual----
tab_credito_individual<-tabItem(
  tabName = "credito_individual_anual",
  br(),
  # h2("Credito individual"),
  br(),
  box(height = 340, width=12,
      DT::dataTableOutput("individual_credito", height = 300)
  ),
  bsModal("detalle_individual_credito", "","tabBut", size = "large",
          br(),
          box(width = 12,height = 80,
              h3(textOutput(as.character("detalle_credito_servicio"))),background = "aqua"
          ),
          br(),
          DT::dataTableOutput("individual_credito_descripcion", height = 450),
          br()
  )
)
# tab_credito_empresarial_anual----
tab_credito_empresarial<-tabItem(
  tabName = "credito_individual_anual",
  br(),
  h2("Credito empresarial anual"),
  br(),
  box(height = 340, width=12,
      DT::dataTableOutput("empresarial_credito", height = 300)
  ),
  bsModal("detalle_empresarial_credito", "Detalle producto","tabBut", size = "large",
          h3(textOutput(as.character("detalle_credito_servicio_empresarial"))),
          br(),
          DT::dataTableOutput("empresarial_credito_descripcion", height = 450),
          br()
  )
  
)
# menu_consumo_credito----
menu_consumo_credito<-menu_consumo(
  ind_anu = tab_credito_individual,
  emp_anu = tab_credito_empresarial
)

tab_credito <- tabItem(
  tabName = "credito_principal",
  box_informacion_empresa_credito,
  menu_consumo_credito
)
#Educación----
# tab_educacion_individual_anual----
tab_educacion_individual<-tabItem(
  tabName = "educacion_individual_anual",
  br(),
  # h2("Educacion individual"),
  br(),
  box(height = 340, width=12,
      DT::dataTableOutput("individual_educacion", height = 300)
  ),
  bsModal("detalle_individual_educacion", "Detalle producto","tabBut", size = "large",
          box(width = 12,height = 80,
              h3(textOutput(as.character("detalle_educacion_servicio"))),background = "aqua"),
          br(),
          DT::dataTableOutput("individual_educacion_descripcion", height = 450),
          br()
  )
)
# tab_educacion_empresarial_anual----
tab_educacion_empresarial<-tabItem(
  tabName = "educacion_individual_anual",
  br(),
  br(),
  box(height = 340, width=12,
      DT::dataTableOutput("empresarial_educacion", height = 300)
  ),
  bsModal("detalle_empresarial_educacion", "Detalle producto","tabBut", size = "large",
          box(width = 12,height = 80,
              h3(id="test",textOutput(as.character("detalle_educacion_servicio_empresarial"))),background = "aqua"),
          br(),
          DT::dataTableOutput("empresarial_educacion_descripcion", height = 450),
          br()
  )
)
# menu_consumo_educacion----
menu_consumo_educacion<-menu_consumo(
  ind_anu = tab_educacion_individual,
  emp_anu = tab_educacion_empresarial
)

tab_educacion <- tabItem(
  tabName = "educacion_principal",
  box_informacion_empresa_educacion,
  menu_consumo_educacion
)
#Mercadeo social----
# tab_mercadeo_individual_anual----
tab_mercadeo_individual<-tabItem(
  tabName = "mercadeo_individual_anual",
  br(),
  # h2("Mercadeo individual"),
  br(),
  box(height = 340, width=12,
      DT::dataTableOutput("individual_mercadeo", height = 300)
  ),
  bsModal("detalle_individual_mercadeo", "Detalle producto","tabBut", size = "large",
          h3(textOutput(as.character("detalle_mercadeo_servicio"))),
          br(),
          DT::dataTableOutput("individual_mercadeo_descripcion", height = 450),
          br()
  )
)
# tab_mercadeo_empresarial_anual----
tab_mercadeo_empresarial<-tabItem(
  tabName = "mercadeo_individual_anual",
  br(),
  # h2("Mercadeo empresarial anual"),
  br(),
  box(height = 340, width=12,
      DT::dataTableOutput("empresarial_mercadeo", height = 300)
  ),
  bsModal("detalle_empresarial_mercadeo", "Detalle producto","tabBut", size = "large",
          h3(textOutput(as.character("detalle_mercadeo_servicio_empresarial"))),
          br(),
          DT::dataTableOutput("empresarial_mercadeo_descripcion", height = 450),
          br()
  )
)
# menu_consumo_mercadeo----
menu_consumo_mercadeo<-menu_consumo(
  ind_anu = tab_mercadeo_individual,
  emp_anu = tab_mercadeo_empresarial
)
tab_mercadeo <- tabItem(
  tabName = "mercadeo_principal",
  box_informacion_empresa_mercadeo,
  menu_consumo_mercadeo
)
#Recreacion y turismo----
# tab_ryt_individual_anual----
tab_ryt_individual<-tabItem(
  tabName = "RyT_individual_anual",
  br(),
  # h2("Recreación individual"),
  br(),
  box(height = 340, width=12,
      DT::dataTableOutput("individual_recreacion", height = 300)
  ),
  bsModal("detalle_individual_recreacion", "Detalle producto","tabBut", size = "large",
          h3(textOutput(as.character("detalle_recreacion_servicio"))),
          br(),
          DT::dataTableOutput("individual_recreacion_descripcion", height = 450),
          br()
  )
)
# tab_ryt_empresarial_anual----
tab_ryt_empresarial<-tabItem(
  tabName = "RyT_empresarial_anual",
  br(),
  # h2("Recreación empresarial"),
  br(),
  box(height = 340, width=12,
      DT::dataTableOutput("empresarial_recreacion", height = 300)
  ),
  bsModal("detalle_empresarial_recreacion", "Detalle producto","tabBut", size = "large",
          h3(textOutput(as.character("detalle_recreacion_servicio_empresarial"))),
          br(),
          DT::dataTableOutput("empresarial_recreacion_descripcion", height = 450),
          br()
  )
)
# menu_consumo_ryt----
menu_consumo_ryt<-menu_consumo(
  ind_anu = tab_ryt_individual,
  emp_anu = tab_ryt_empresarial
)
tab_recreacion <- tabItem(
  tabName = "recreacion_principal",
  box_informacion_empresa_recreacion,
  menu_consumo_ryt
)
#Salud----
# tab_salud_individual_anual----
tab_salud_individual<-tabItem(
  tabName = "salud_individual_anual",
  br(),
  # h2("Salud individual"),
  br(),
  box(height = 340, width=12,
      DT::dataTableOutput("individual_salud", height = 300)
  ),
  bsModal("detalle_individual_salud", "Detalle producto","tabBut", size = "large",
          h3(textOutput(as.character("detalle_salud_servicio"))),
          br(),
          DT::dataTableOutput("individual_salud_descripcion", height = 450),
          br()
  )
)
# tab_salud_empresarial_anual----
tab_salud_empresarial<-tabItem(
  tabName = "salud_empresarial_anual",
  br(),
  # h2("Salud empresarial"),
  br(),
  box(height = 340, width=12,
      DT::dataTableOutput("empresarial_salud", height = 300)
  ),
  bsModal("detalle_empresarial_salud", "Detalle producto","tabBut", size = "large",
          h3(textOutput(as.character("detalle_salud_servicio_empresarial"))),
          br(),
          DT::dataTableOutput("empresarial_salud_descripcion", height = 450),
          br()
  )
)
# menu_consumo_salud----
menu_consumo_salud<-menu_consumo(
  ind_anu = tab_salud_individual,
  emp_anu = tab_salud_empresarial
)
tab_salud <- tabItem(
  tabName = "salud_principal",
  box_informacion_empresa_salud,
  menu_consumo_salud
)
#Vivienda----
# tab_vivienda_individual_anual----
tab_vivienda_individual<-tabItem(
  tabName = "vivienda_individual",
  br(),
  # h2("Vivienda individual"),
  br(),
  box(height = 340, width=12,
      DT::dataTableOutput("individual_vivienda", height = 300)
  ),
  bsModal("detalle_individual_vivienda", "Detalle producto","tabBut", size = "large",
          h3(textOutput(as.character("detalle_vivienda_servicio"))),
          br(),
          DT::dataTableOutput("individual_vivienda_descripcion", height = 450),
          br()
  )
  
)
# tab_vivienda_empresarial_anual----
tab_vivienda_empresarial<-tabItem(
  tabName = "vivienda_individual_anual"
  
)
# menu_consumo_vivienda----
menu_consumo_vivienda<-menu_consumo(
  ind_anu = tab_vivienda_individual,
  emp_anu = tab_vivienda_empresarial
)
tab_vivienda <- tabItem(
  tabName = "vivienda_principal",
  box_informacion_empresa_vivienda,
  menu_consumo_vivienda
)


#Menu caja de consumo navbarPage ----
#Une todos los item del menu----
cuerpo_ues <- tabItems(
  tab_manual,
  tab_credito,
  tab_educacion,
  tab_mercadeo,
  tab_recreacion,
  tab_salud,
  tab_vivienda
)
#Partes del DashBoard----
#Cabecera


header <- dashboardHeaderPlus(
  title = tagList(img(src = "35pxAmarillo.png"),span(class = "logo-min","Ficha Consumo")),
  enable_rightsidebar = F
)


#Sidebar
Sidebar<-dashboardSidebar(
  hr(),
  h3("\tBUSCAR"),
  input_numero_documento,
  dateRangeInput('dateRange',
                 label = h4('Periodo'),
                 start = as.Date("2016/01/01"),
                 end = as.Date("2020/12/31"),
                 separator = " A ",
                 format = "dd/mm/yyyy",
                 min = as.Date("2016/01/01"),
                 max = as.Date("2021/01/01")
  ),
  actionButton("Run", "", icon=icon("fas fa-search"), class="btn btn-default pull-right"),
  br(),
  br(),
  hr(),
  h3("UES"),
  sidebarMenu(
    menuItem("Credito", tabName = "credito_principal",icon = icon("far fa-credit-card")),
    menuItem("Educacion", tabName = "educacion_principal",icon = icon("far fa-school")),
    menuItem("Mercadeo", tabName = "mercadeo_principal", icon = icon("fas fa-poll")),
    menuItem("Recreacion", tabName = "recreacion_principal",icon = icon("fas fa-swimmer")),
    menuItem("Salud", tabName = "salud_principal", icon = icon("fas fa-hospital")),
    menuItem("Vivienda", tabName = "vivienda_principal", icon = icon("fas fa-city"))
  ),
  br(),
  h3("MANUAL"),
  sidebarMenu(
    menuItem("Manual", tabName = "tab_manual", icon = icon("question-circle"))
  )
  #caja_help
)
#Cuerpo
body<- dashboardBody(
  cuerpo_ues
)
# Define las partes del DashBoard----
dashboardPage(header,Sidebar,body,
              tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "Estilo.css")
              )
              )
