library(shiny); library(shinydashboard); library(shiny)

dashboardPage(skin = "blue",
              dashboardHeader(title = "Gestion información medicamentos", titleWidth = 500),
              dashboardSidebar(
                sidebarMenu(disable = TRUE,
                            br(),br(),
                            tags$img(src = "logo.png", height=40, width=200, align="center"),
                            br(),br(),
                            pickerInput(inputId = "xeps", label = "Multiple", choices = name_eps, multiple = TRUE),
                            actionButton("go", label = "Aplicar consulta")
                )
              ),
              dashboardBody(
                shinyDashboardThemes(theme = "grey_dark"),
                column(8,
                  fluidRow(
                    box(solidHeader = FALSE, width = 12, height = 550, status = "primary", title = "Volumen Pedidos",
                        withLoader(plotlyOutput("plot_volumen_pedidos", height = 400), type = "html", loader = "loader1")
                        ),
                    box(solidHeader = FALSE, width = 12, height = 550, status = "primary", title = "Top Transacciones por Departamento",
                        withLoader(plotlyOutput("plot_tr_departamento"), type = "html", loader = "loader1")
                        ),
                    box(solidHeader = FALSE, width = 12, height = 550, status = "primary", title = "Top Transacciones Vulnerables",
                        withLoader(plotlyOutput("plot_tr_vulnerables_si"), type = "html", loader = "loader1")
                    ),
                    box(solidHeader = FALSE, width = 12, height = 550, status = "primary", title = "Top Transacciones no Vulnerables",
                        withLoader(plotlyOutput("plot_tr_vulnerables_no"), type = "html", loader = "loader1")
                    ),
                    box(solidHeader = FALSE, width = 12, height = 550, status = "primary", title = "Top Transacciones Atendidos Vulnerables",
                        withLoader(plotlyOutput("plot_tr_atendidos_si_vulnerables_si"), type = "html", loader = "loader1")
                    ),
                    box(solidHeader = FALSE, width = 12, height = 550, status = "primary", title = "Top Transacciones Atendidos no Vulnerables",
                        withLoader(plotlyOutput("plot_tr_atendidos_si_vulnerables_no"), type = "html", loader = "loader1")
                    )
                  )
                  ),
                column(4,
                       box(solidHeader = FALSE, width = 12, height = 550, status = "primary", title = "Resumen descriptivo",
                           valueBoxOutput("info_transacciones",width = 12),
                           valueBoxOutput("info_vulnerables_si",width = 12),
                           valueBoxOutput("info_vulnerables_no",width = 12),
                           valueBoxOutput("info_atendidos",width = 12),
                           valueBoxOutput("info_atendidos_si_vulnerables_si",width = 12),
                           valueBoxOutput("info_atendidos_si_vulnerables_no",width = 12)
                           )
                       )
                )
              )


