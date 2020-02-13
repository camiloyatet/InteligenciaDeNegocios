
shinyServer(function(input, output, session) {
  
  ### Actualizacion de los Inputs -----
  
  data_prod1 <- reactive({
    aux <- data %>% # Data inicial
      filter(if (input$Unidad!="Todas") UES==input$Unidad else TRUE )
    
    prods <- unique(aux$Producto)
    
  })

  observe({
    updateSelectInput(session, "Producto",choices = c(data_prod1(), "Todos"), selected = "Todos")
  })
  
  
  ### Bases de datos Reactivas -----
  
  data_f <- eventReactive(input$go,{
    data %>%
      filter(if (input$Unidad!="Todas") UES==input$Unidad else TRUE,
             if (input$Producto!="Todos") Producto==input$Producto else TRUE
             )
  }, ignoreNULL = T)
  
  data_af <- reactive({
    data_f() %>%
      filter(Afiliado)
  })

  
  ### Cajas Totales ----
  
  output$po_box_Total <- renderInfoBox({
    
    aux <- case_when(input$Medida=="Monto" ~ paste( data_f() %>% summarise(Valor=round(sum(Valor)/1e6, 0)) %>% as.numeric() %>% dollar(), "MM"),
                     input$Medida=="Personas" ~ data_f() %>% summarise(Valor=n_distinct(NumIdPersona)) %>% as.numeric() %>% comma(),
                     input$Medida=="Frecuencia" ~ data_f() %>% nrow() %>% as.numeric() %>% comma())
    
    infoBox(title = input$Medida,
            subtitle = paste("Consumo Individual entre", fec_min, "y", fec_max),
            value = aux,
            color = "navy",
            icon = icon("receipt")
    )
    
  })
  
  output$po_box_afiliados <- renderInfoBox({
    
    aux <- case_when(input$Medida=="Monto" ~ paste( data_f() %>% filter(Afiliado) %>% summarise(Valor=round(sum(Valor)/1e6, 0)) %>% as.numeric() %>% dollar(), "MM"),
                     input$Medida=="Personas" ~ data_f() %>% filter(Afiliado) %>% summarise(Valor=n_distinct(NumIdPersona)) %>% as.numeric() %>% comma(),
                     input$Medida=="Frecuencia" ~ data_f() %>% filter(Afiliado) %>% nrow() %>% as.numeric() %>% comma())
    
    infoBox(title = paste(input$Medida, "(Afilados)"),
            subtitle = "",
            value = aux,
            color = "navy",
            icon = icon("universal-access")
    )
    
  })
  
  output$po_box_NoAfiliados <- renderInfoBox({
    
    aux <- case_when(input$Medida=="Monto" ~ paste( data_f() %>% filter(!Afiliado) %>% summarise(Valor=round(sum(Valor)/1e6, 0)) %>% as.numeric() %>% dollar(), "MM"),
                     input$Medida=="Personas" ~ data_f() %>% filter(!Afiliado) %>% summarise(Valor=n_distinct(NumIdPersona)) %>% as.numeric() %>% comma(),
                     input$Medida=="Frecuencia" ~ data_f() %>% filter(!Afiliado) %>% nrow() %>% as.numeric() %>% comma())
    
    infoBox(title = paste(input$Medida, "(No Afilados)"),
            subtitle = "",
            value = aux,
            color = "navy",
            icon = icon("user-circle")
    )
    
  })
  
  output$Descargar1 <- downloadHandler(
    filename = function() {
      paste0("Consumo", format(Sys.time(), "%Y%b%d %H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv2(data_f(),file, row.names = FALSE)
    }
  )

  output$AfiliadosAbsoluto <- renderPlotly({

    aux1 <- data_f() %>% 
      group_by(Periodo) %>% 
      mutate(Monto_T= sum(Valor, na.rm = T),
             Personas_T= n_distinct(NumIdPersona),
             Frecuencia_T= n(),
             Afiliado=ifelse(Afiliado, "Afiliado", "No Afiliado")) %>% 
      ungroup() %>% group_by(Periodo=as.character(Periodo), Afiliado) %>% 
      summarise(Monto= sum(Valor, na.rm = T),
                Monto_PCT= sum(Valor, na.rm = T)/max(Monto_T),
                Personas= n_distinct(NumIdPersona),
                Personas_PCT= n_distinct(NumIdPersona)/max(Personas_T),
                Frecuencia= n(),
                Frecuencia_PCT= n()/max(Frecuencia_T)) %>% 
      ungroup() %>% 
      select(Periodo, Afiliado, starts_with(input$Medida))
    
    colnames(aux1) <- c('Periodo','Afiliado','Var','VarPCT')
    
    if(input$TipoMedida){
      p <- plot_ly(aux1, x = ~Periodo, y = ~Var, type = 'bar', color = ~Afiliado, hoverinfo = "text", colors = c("#4682B4", "#B22222"),
                   hovertext = paste("<b>Tipo de Cliente :</b>", aux1$Afiliado,
                                     "<br><b>Periodo :</b>", aux1$Periodo,
                                     "<br><b>", input$Medida, " :</b>", comma(aux1$Var),
                                     "<br><b>", input$Medida, " (Porcentaje) :</b>", percent(aux1$VarPCT))) %>%
        layout(title = "Consumo Individual",
               xaxis = list(title = "Periodo"),
               yaxis = list(title = input$Medida), 
               barmode = 'stack')
    }else{
      p <- plot_ly(aux1, x = ~Periodo, y = ~VarPCT, type = 'bar', color = ~Afiliado, hoverinfo = "text", colors = c("#4682B4", "#B22222"),
                   hovertext = paste("<b>Tipo de Cliente :</b>", aux1$Afiliado,
                                     "<br><b>Periodo :</b>", aux1$Periodo,
                                     "<br><b>", input$Medida, " :</b>", comma(aux1$Var),
                                     "<br><b>", input$Medida, " (Porcentaje) :</b>", percent(aux1$VarPCT))) %>%
        layout(title = "Consumo Individual",
               xaxis = list(title = "Periodo"),
               yaxis = list(title = paste(input$Medida, "PCT"), tickformat = "%"), 
               barmode = 'stack') 
    }
    
    p

  })

  
  ### Afiliados -----
  
  ### Piramide
  output$Piramide <- renderPlotly({

    aux1 <- data_af() %>%
      group_by(NumIdPersona, Periodo) %>% filter(row_number()==1) %>% ungroup() %>% 
      mutate(Quinquenio=cut(Edad, seq(0,120,5), right = F)) %>%
      group_by(Periodo) %>%
      mutate(Monto_T= sum(Valor, na.rm = T),
             Personas_T= n_distinct(NumIdPersona),
             Frecuencia_T= n()) %>%
      ungroup() %>% group_by(Periodo=as.character(Periodo), Quinquenio, Genero) %>%
      summarise(Monto= sum(Valor, na.rm = T),
                Monto_PCT= sum(Valor, na.rm = T)/max(Monto_T),
                Personas= n_distinct(NumIdPersona),
                Personas_PCT= n_distinct(NumIdPersona)/max(Personas_T),
                Frecuencia= n(),
                Frecuencia_PCT= n()/max(Frecuencia_T)) %>%
      select(Periodo, Quinquenio, Genero, starts_with(input$Medida)) %>%
      ungroup() %>%
      mutate_if(is.numeric, list(~ifelse(Genero=="M", .*-1, .))) %>%
      mutate(Genero=ifelse(Genero=="F", "Femenino", "Masculino"))

    colnames(aux1) <- c('Periodo','Quinquenio','Genero','Var','VarPCT')

    if(input$TipoMedida){
      p <- plot_ly(aux1, x = ~Var, y = ~Quinquenio, color = ~Genero, frame = ~Periodo, colors = c("#FA8072", "#87CEEB"),
                   hovertext = paste("<b>Quinquenio:</b>", aux1$Quinquenio,
                                     "<br><b>Genero :</b>", aux1$Genero,
                                     "<br><b>", input$Medida, " :</b>", comma(aux1$Var),
                                     "<br><b>", input$Medida, " (Porcentaje) :</b>", percent(aux1$VarPCT))) %>%  
        add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(abs(Var)), textposition = 'auto') %>% 
        layout(bargap = 0.1, barmode = 'overlay', title = "Consumo Individual",
               xaxis = list(title = "Poblacion", range = c(-round(max(aux1$Var)*1.4, digits = 0), round(max(aux1$Var)*1.4, digits = 0)),
                            showline = F, showticklabels = F),
               yaxis = list(title = "Quinquenio")) %>% 
        animation_opts(frame = 1000, easing = "elastic", redraw = T)
    }else{
      p <- plot_ly(aux1, x = ~VarPCT, y = ~Quinquenio, color = ~Genero, frame = ~Periodo, colors = c("#FA8072", "#87CEEB")) %>%
        add_bars(orientation = 'h', hoverinfo = 'text', text = ~percent(abs(VarPCT)), textposition = 'auto') %>% 
        layout(bargap = 0.1, barmode = 'overlay', title = "Consumo Individual",
               xaxis = list(title = "Poblacion", range = c(-0.5, 0.5),
                            showline = F, showticklabels = F),
               yaxis = list(title = "Quinquenio")) %>% 
        animation_opts(frame = 1000, easing = "elastic", redraw = T)
    }
    p
  })
  ### Categoria
  output$Categoria <- renderPlotly({
    
    aux1 <- data_af() %>%
      group_by(NumIdPersona, Periodo) %>% filter(row_number()==1) %>% ungroup() %>% 
      group_by(Periodo) %>%
      mutate(Monto_T= sum(Valor, na.rm = T),
             Personas_T= n_distinct(NumIdPersona),
             Frecuencia_T= n()) %>%
      ungroup() %>% group_by(Periodo=as.character(Periodo), Categoria) %>%
      summarise(Monto= sum(Valor, na.rm = T),
                Monto_PCT= sum(Valor, na.rm = T)/max(Monto_T),
                Personas= n_distinct(NumIdPersona),
                Personas_PCT= n_distinct(NumIdPersona)/max(Personas_T),
                Frecuencia= n(),
                Frecuencia_PCT= n()/max(Frecuencia_T)) %>%
      select(Periodo, Categoria, starts_with(input$Medida)) %>%
      ungroup() 
    
    colnames(aux1) <- c('Periodo','Categoria', 'Var','VarPCT')
    
    if(input$TipoMedida){
      p <- plot_ly(aux1, x = ~Var,  y = ~Categoria, type = 'bar', color = ~Categoria, frame = ~Periodo, hoverinfo = "text", 
                   hovertext = paste("<b>Categoria:</b>", aux1$Categoria,
                                     "<br><b>", input$Medida, " :</b>", comma(aux1$Var),
                                     "<br><b>", input$Medida, " (Porcentaje) :</b>", percent(aux1$VarPCT))) %>%  
        layout(title = "Categoria de Afiliacion",
               xaxis = list(title =  input$Medida),
               yaxis = list(title = "Categoria"), 
               barmode = 'stack') %>% 
        animation_opts(frame = 1000, easing = "elastic", redraw = T)
    }else{
      p <- plot_ly(aux1, x = ~VarPCT,  y = ~Categoria, type = 'bar', color = ~Categoria, frame = ~Periodo, hoverinfo = "text", 
                   hovertext = paste("<b>Categoria:</b>", aux1$Categoria,
                                     "<br><b>", input$Medida, " :</b>", comma(aux1$Var),
                                     "<br><b>", input$Medida, " (Porcentaje) :</b>", percent(aux1$VarPCT))) %>%  
        layout(title = "Categoria de Afiliacion",
               xaxis = list(title = paste(input$Medida, "PCT"), tickformat = "%"),
               yaxis = list(title = "Categoria", 
               barmode = 'stack')) %>% 
        animation_opts(frame = 1000, easing = "elastic", redraw = T)

    }
    p
  })
  
  ### Segmento poblacional
  output$Segmento <- renderPlotly({
    
    aux1 <- data_af() %>%
      group_by(NumIdPersona, Periodo) %>% filter(row_number()==1) %>% ungroup() %>% 
      group_by(Periodo) %>%
      mutate(Monto_T= sum(Valor, na.rm = T),
             Personas_T= n_distinct(NumIdPersona),
             Frecuencia_T= n()) %>%
      ungroup() %>% group_by(Periodo=as.character(Periodo), Segmento_poblacional) %>%
      summarise(Monto= sum(Valor, na.rm = T),
                Monto_PCT= sum(Valor, na.rm = T)/max(Monto_T),
                Personas= n_distinct(NumIdPersona),
                Personas_PCT= n_distinct(NumIdPersona)/max(Personas_T),
                Frecuencia= n(),
                Frecuencia_PCT= n()/max(Frecuencia_T)) %>%
      select(Periodo, Segmento_poblacional, starts_with(input$Medida)) %>%
      ungroup() 
    
    colnames(aux1) <- c('Periodo','Segmento_poblacional', 'Var','VarPCT')
    
    if(input$TipoMedida){
      p <- plot_ly(aux1, x = ~Var,  y = ~Segmento_poblacional, type = 'bar', color = ~Segmento_poblacional, frame = ~Periodo, hoverinfo = "text",  colors = "Set3",
                   hovertext = paste("<b>Segmento poblacional:</b>", aux1$Segmento_poblacional,
                                     "<br><b>", input$Medida, " :</b>", comma(aux1$Var),
                                     "<br><b>", input$Medida, " (Porcentaje) :</b>", percent(aux1$VarPCT))) %>%  
        layout(title = "Segmento poblacional",
               xaxis = list(title = input$Medida),
               yaxis = list(title = "Segmento_poblacional"), 
               barmode = 'stack') %>% 
        animation_opts(frame = 1000, easing = "elastic", redraw = T)
    }else{
      p <- plot_ly(aux1, x = ~VarPCT,  y = ~Segmento_poblacional, type = 'bar', color = ~Segmento_poblacional, frame = ~Periodo, hoverinfo = "text",  colors = "Set3",
                   hovertext = paste("<b>Segmento poblacional:</b>", aux1$Segmento_poblacional,
                                     "<br><b>", input$Medida, " :</b>", comma(aux1$Var),
                                     "<br><b>", input$Medida, " (Porcentaje) :</b>", percent(aux1$VarPCT))) %>%  
        layout(title = "Categoria de Afiliacion",
               xaxis = list(title = paste(input$Medida, "PCT"), tickformat = "%"),
               yaxis = list(title = "Segmento poblacional", 
                            barmode = 'stack')) %>% 
        animation_opts(frame = 1000, easing = "elastic", redraw = T)
    }
    p
  })
  
  ## Piramide Empresarial
  output$PiramideEmp <- renderPlotly({

    aux1 <- data_af() %>%
      group_by(NumIdPersona, Periodo) %>% filter(row_number()==1) %>% ungroup() %>%
      group_by(Periodo) %>%
      mutate(Monto_T= sum(Valor, na.rm = T),
             Personas_T= n_distinct(NumIdPersona),
             Frecuencia_T= n()) %>%
      ungroup() %>% group_by(Periodo=as.character(Periodo), Piramide1, Piramide2) %>%
      summarise(Monto= sum(Valor, na.rm = T),
                Monto_PCT= sum(Valor, na.rm = T)/max(Monto_T),
                Personas= n_distinct(NumIdPersona),
                Personas_PCT= n_distinct(NumIdPersona)/max(Personas_T),
                Frecuencia= n(),
                Frecuencia_PCT= n()/max(Frecuencia_T)) %>%
      select(Periodo, Piramide1, Piramide2, starts_with("Monto")) %>%
      ungroup()

    colnames(aux1) <- c('Periodo','Piramide1', 'Piramide2', 'Var','VarPCT')

    tmp1 <- bind_rows(
      aux1 %>% group_by(periodo=Periodo, labels="total", parents=NA, ids="total") %>% summarise(Var=sum(Var), VarPCT=sum(VarPCT)),
      aux1 %>% group_by(periodo=Periodo, labels=Piramide1, parents='total', ids=paste("total -",  Piramide1)) %>% summarise(Var=sum(Var), VarPCT=sum(VarPCT)),
      aux1 %>% group_by(periodo=Periodo, labels=Piramide2, parents=paste("total -",  Piramide1), ids=paste("total -",  Piramide1, "-", Piramide2)) %>%
        summarise(Var=sum(Var), VarPCT=sum(VarPCT))
    )

    p <- plot_ly(data = tmp1, ids = ~ids, labels= ~labels, parents = ~parents, values= ~Var, type='sunburst', branchvalues = 'total', frame = ~periodo,
                 hoverinfo = "text",  colors = "Set1",
                 hovertext = paste("<b>Piramide 1:</b>", tmp1$labels,
                                   "<br><b>", input$Medida, " :</b>", comma(tmp1$Var),
                                   "<br><b>", input$Medida, " (Porcentaje) :</b>", percent(tmp1$VarPCT))) %>% 
      animation_opts(frame = 1000, easing = "elastic", redraw = T)
    p
  })
  
  ## Actividad Economica
  output$CIIU <- renderPlotly({
    
    aux1 <- data_af() %>%
      group_by(NumIdPersona, Periodo) %>% filter(row_number()==1) %>% ungroup() %>%
      group_by(Periodo) %>%
      mutate(Monto_T= sum(Valor, na.rm = T),
             Personas_T= n_distinct(NumIdPersona),
             Frecuencia_T= n()) %>%
      ungroup() %>% group_by(Periodo=as.character(Periodo), SectorCIIU, DescripcionCIIU) %>%
      summarise(Monto= sum(Valor, na.rm = T),
                Monto_PCT= sum(Valor, na.rm = T)/max(Monto_T),
                Personas= n_distinct(NumIdPersona),
                Personas_PCT= n_distinct(NumIdPersona)/max(Personas_T),
                Frecuencia= n(),
                Frecuencia_PCT= n()/max(Frecuencia_T)) %>%
      select(Periodo, SectorCIIU, DescripcionCIIU, starts_with("Monto")) %>%
      ungroup()
    
    colnames(aux1) <- c('Periodo','SectorCIIU', 'DescripcionCIIU', 'Var','VarPCT')
    
    tmp1 <- bind_rows(
      aux1 %>% group_by(periodo=Periodo, labels="total", parents=NA, ids="total") %>% summarise(Var=sum(Var), VarPCT=sum(VarPCT)),
      aux1 %>% group_by(periodo=Periodo, labels=SectorCIIU, parents='total', ids=paste("total -",  SectorCIIU)) %>% summarise(Var=sum(Var), VarPCT=sum(VarPCT)),
      aux1 %>% group_by(periodo=Periodo, labels=DescripcionCIIU, parents=paste("total -",  SectorCIIU), ids=paste("total -",  SectorCIIU, "-", DescripcionCIIU)) %>%
        summarise(Var=sum(Var), VarPCT=sum(VarPCT))
    )
    
    p <- plot_ly(data = tmp1, ids = ~ids, labels= ~labels, parents = ~parents, values= ~Var, type='sunburst', branchvalues = 'total', frame = ~periodo,
                 hoverinfo = "text",  colors = "Set1",
                 hovertext = paste("<b>CIIU:</b>", tmp1$labels,
                                   "<br><b>", input$Medida, " :</b>", comma(tmp1$Var),
                                   "<br><b>", input$Medida, " (Porcentaje) :</b>", percent(tmp1$VarPCT))) %>% 
      animation_opts(frame = 1000, easing = "elastic", redraw = T)
    p
  })
  
  ### Prueba ----
  
  
  output$test <- renderDataTable({
    
    aux1 <- data_af() %>%  
      mutate(Quinquenio=cut(Edad, seq(0,120,5), right = F)) %>% 
      group_by(Periodo) %>% 
      mutate(Monto_T= sum(Valor, na.rm = T),
             Personas_T= n_distinct(NumIdPersona),
             Frecuencia_T= n()) %>% 
      ungroup() %>% group_by(Periodo=as.character(Periodo), Quinquenio, Genero) %>% 
      summarise(Monto= sum(Valor, na.rm = T),
                Monto_PCT= sum(Valor, na.rm = T)/max(Monto_T),
                Personas= n_distinct(NumIdPersona),
                Personas_PCT= n_distinct(NumIdPersona)/max(Personas_T),
                Frecuencia= n(),
                Frecuencia_PCT= n()/max(Frecuencia_T)) %>% 
      select(Periodo, Quinquenio, Genero, starts_with(input$Medida)) %>% 
      ungroup() %>% 
      mutate_if(is.numeric, list(~ifelse(Genero=="M", .*-1, .))) %>% 
      mutate(Genero=ifelse(Genero=="F", "Femenino", "Masculino"))
    
    colnames(aux1) <- c('Periodo','Quinquenio','Genero','Var','VarPCT')
    
    datatable(aux1)
  })
  
  
  
})
