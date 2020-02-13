shinyServer(function(input, output, session) {
  values <- reactiveValues()
  addClass(selector = "body", class = "sidebar-collapse")
  
  #### Reactividad de Inputs ----
  
  data_prod1 <- reactive({
    data %>% filter(actividadeconomica == input$Actividad)
  })
  
  outVar1 <- reactive({
    prods <- unique(data_prod1()$sectoreconomica)
  })
  
  observe({
    updateSelectInput(session, "Sector", choices = outVar1())
  })
  
  data_prod2 <- reactive({
    data %>% 
      filter(actividadeconomica == input$Actividad,
             sectoreconomica == input$Sector)
  })
  
  outVar2 <- reactive({
    prods <- unique(data_prod2()$TipoEmpresa)
  })
  
  observe({
    updateSelectInput(session, "Tipo", choices = outVar2())
  })
  
  

  #### Datos Reactivos ----
  data_f <- eventReactive(input$go, {
    
    data %>% 
      filter(
        actividadeconomica == input$Actividad,
        sectoreconomica == input$Sector,
        TipoEmpresa == input$Tipo)
    }, ignoreNULL = F)

  #### Datos Ingresados por Usuario ----
  output$tabla <- renderRHandsontable({
    
    aux <- data_f() %>% 
      mutate(Empleados=0) %>% 
      select(nivel, cargos, Empleados)
    
    rhandsontable(aux, rowHeaders=NULL, colHeaders = c("Nivel del Cargo", "Cargo", "Numero de Empleados")) %>% 
      hot_table(highlightCol = T, highlightRow = T) %>% 
      hot_col(col=c(1,2), readOnly = T) %>% 
      hot_col(col=3, readOnly = F, format = "0")
  })
  
  #### Datos Ingresados ----
  observeEvent(input$runButton, {
    values$data <-  hot_to_r(input$tabla)
  })
  
  data_fin <- reactive({
    req(input$runButton)
    aux <- as.data.frame(values$data)
    
    data_f() %>%
      inner_join(values$data, by=c("nivel", "cargos")) %>% 
      mutate(Aporte=Salario*Empleados*0.04)
      
  })
  
  #### Calulos de Aporte Reactivos ----
  
  output$po_num_empleados <- renderInfoBox({
    
    aux <- data_fin() %>% summarise(Empleados=sum(Empleados, na.rm = T)) %>% as.numeric()
    
    infoBox(title = "Empleados",
            subtitle = "Numero de Empleados",
            value = comma(aux),
            color = "navy",
            icon = icon("users")
    )
    
  })
  output$po_aporte <- renderInfoBox({
    
    aux <- data_fin() %>% summarise(Aporte=sum(Aporte, na.rm = T)) %>% as.numeric()
    
    infoBox(title = "Aprtes Estimados",
            subtitle = "Total de Aportes Estimados",
            value = dollar(aux),
            color = "navy",
            icon = icon("file-invoice-dollar")
    )
    
  })
  output$po_remanente <- renderInfoBox({
    
    aux <- data_fin() %>% mutate(Aporte=Aporte*0.32)  %>% summarise(Aporte=sum(Aporte, na.rm = T)) %>% as.numeric()
    
    infoBox(title = "Remanente Estimado",
            subtitle = "Total de Remanente Estimado",
            value = dollar(aux),
            color = "navy",
            icon = icon("hand-holding-usd")
    )
    
  })
  output$po_piramide1 <- renderInfoBox({
    
    aux <- data_fin() %>% 
      mutate(Aporte=Aporte*0.32)  %>% 
      summarise(Aporte=sum(Aporte, na.rm = T)) %>% 
      mutate(Piramide1=ifelse(Aporte<20000000, '4 Micro',
                              ifelse(Aporte<85000000, '3 Empresas Pymes',
                                     ifelse(Aporte<250000000, '2 Emp Medio', '1 Emp Grandes'))),
             Piramide2=ifelse(Aporte<20000000, '4.1 Estándar',
                              ifelse(Aporte<40000000, '3.2 VIP Estándar',
                                     ifelse(Aporte<85000000, '3.1 VIP',
                                            ifelse(Aporte<150000000, '2.2 Silver',
                                                   ifelse(Aporte<250000000, '2.1 Gold',
                                                          ifelse(Aporte<420000000, '1.2 Premium',"1.1 Platinum"))))))) %>% 
      select(Piramide1) %>% 
      as.character()
    
    infoBox(title = "Piramide Poblacional",
            subtitle = "Segmento empresarial (Pirmaide 1)",
            value = aux,
            color = "navy",
            icon = icon("sort-numeric-down")
    )
    
  })
  output$po_piramide2 <- renderInfoBox({
    
    aux <- data_fin() %>% 
      mutate(Aporte=Aporte*0.32)  %>% 
      summarise(Aporte=sum(Aporte, na.rm = T)) %>% 
      mutate(Piramide1=ifelse(Aporte<20000000, '4 Micro',
                              ifelse(Aporte<85000000, '3 Empresas Pymes',
                                     ifelse(Aporte<250000000, '2 Emp Medio', '1 Emp Grandes'))),
             Piramide2=ifelse(Aporte<20000000, '4.1 Estándar',
                              ifelse(Aporte<40000000, '3.2 VIP Estándar',
                                     ifelse(Aporte<85000000, '3.1 VIP',
                                            ifelse(Aporte<150000000, '2.2 Silver',
                                                   ifelse(Aporte<250000000, '2.1 Gold',
                                                          ifelse(Aporte<420000000, '1.2 Premium',"1.1 Platinum"))))))) %>% 
      select(Piramide2) %>% 
      as.character()
    
    infoBox(title = "Piramide Poblacional",
            subtitle = "Segmento empresarial (Pirmaide 2)",
            value = aux,
            color = "navy",
            icon = icon("sort-numeric-down")
    )
    
  })

  
})

