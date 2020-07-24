
shinyServer(function(input, output, session) {
  
  data_read <- reactive({
    req(input$file1)
    df <- read_excel(input$file1$datapath) %>%
      data.frame() %>%
      mutate_all(.funs = as.character) %>%
      select(1)
    names(df)[1] <- "id_persona"
    return(df)
  })
  
  output$num_informes <- renderValueBox({
    valueBox(
      value = formatC(nrow(data_read()),format="d"),
      subtitle = "Numero de personas a consultar",
      icon = icon("check"),
      color = "teal"
    )
  })
  
  # Meses cotizados
  data_tiempo_cotizado <- eventReactive(input$go,{
    df <- data_read() %>% 
      left_join(poblacion, by = c("id_persona" = "id_persona")) %>% 
      data.frame()
    return(df)
  })
  
  output$datos_cotiza <- renderDataTable({
    datatable(data_tiempo_cotizado(),
              options=list(dom="lt", searching = F, scrollX = F, scrollY = "400px", paging = FALSE), 
              rownames = F,
              colnames = c("Identificacion", "Meses Cotizados")
              )
  })
  
  # Entrega de subsidios
  data_subsidios_entregados <- eventReactive(input$go,{
    df <- data_read() %>% 
      left_join(subsidios_entregados, by = c("id_persona" = "id_persona")) %>% 
      data.frame()
    return(df)
  })
  
  output$datos_subsidio <- renderDataTable({
    datatable(data_subsidios_entregados(),
              options=list(dom="lt", searching = F, scrollX = F, scrollY = "400px", paging = FALSE), 
              rownames = F,
              colnames = c("Identificacion", "Estado", "CFC"))
  })
  
  # Estado afiliacion
  data_estado_afil <- eventReactive(input$go,{
    df <- data_read() %>% 
      mutate(Afiliacion = ifelse(id_persona %in% consolidada$id_persona, "Activa", "No activa")) %>% 
      select(id_persona,Afiliacion) %>% 
      data.frame()
    return(df)
  })
  
  output$datos_afil <- renderDataTable({
    datatable(data_estado_afil(),
              options=list(dom="lt", searching = F, scrollX = F, scrollY = "400px", paging = FALSE), 
              rownames = F,
              colnames = c("Identificacion", "Afiliacion"))
  })
  
  consulta_subsidios <- reactive({
    df <- data_tiempo_cotizado() %>% 
      left_join(data_subsidios_entregados(), by = "id_persona") %>% 
      left_join(data_estado_afil(), by = "id_persona") %>% 
      dplyr::rename("Meses Cotizados"="conteo")
  })

  # union consulta subsidios
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("consulta_subsidios", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(consulta_subsidios(), file)
    }
  )
  
  
})



