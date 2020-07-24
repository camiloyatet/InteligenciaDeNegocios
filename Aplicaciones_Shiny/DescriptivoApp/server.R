shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2)
  # -------------------------------------- Reactive database  -----------------------------------------------
  data <- reactive({
    options(scipen=999)  
    req(input$file1)
    data <- fread(input$file1$datapath)
    if(ncol(data) > 1){
      data <- data[,1]
    }
    return(data)
  })
  
  infoAfiliados <- eventReactive(input$success,{
    aux <- data()
    if (input$radio == 1) {
      names(aux) <- c("id_persona")
    }else{
      names(aux) <- c("NumIdPersona")
      aux <- aux %>% mutate(NumIdPersona = as.character(NumIdPersona))
    }
    infoAfiliados <- aux %>% inner_join(BaseDatos)
    sendSweetAlert(
      session = session,
      title = "Correcto !!",
      text = "Todo en orden",
      type = "success"
    )
    return(infoAfiliados)
  }, ignoreNULL = F)
  
  # -------------------------------------- Output Tables----------------------------------------------------------
  output$datatTablecedula <- renderTable({
    if(nrow(data())>15){
      aux <-data()[1:15,]
      return(aux)
    }
    aux <- data()
  })
  
  output$datatTablecedula2 <- renderTable({
    if(nrow(infoAfiliados())>15){
      aux <-infoAfiliados()[1:15,c("id_persona","NumIdPersona","Segmento_poblacional")]
      return(aux)
    }
    aux <- infoAfiliados()[,c("id_persona","NumIdPersona","Segmento_poblacional")]
  })
  
  output$Categoria <- renderTable({
    aux <- infoAfiliados() %>% count(Categoria,Segmento_poblacional) %>% 
      tidyr::spread(key = Categoria,value = n)
  })
  
  output$PiramideTable <- renderTable({
    aux <- infoAfiliados() %>% group_by(Piramide1,Piramide2) %>% summarise(valor = n())
  })
  
  output$SegmentoGrupoFamiliar <- renderTable({
    aux <- infoAfiliados() %>% group_by(segmento_grupo_familiar) %>% summarise(valor = n())
  })
  
  output$EstratoPersona <- renderTable({
    aux <- infoAfiliados() %>% group_by(EstratoPersona) %>% summarise(valor = n())
  })
  
  # -------------------------------------- Output Plot Piramide poblacional --------------------------------------------------
  output$Piramide <- renderPlotly({
    aux <- infoAfiliados() %>%
      mutate(Edad=ifelse(Edad>=90, 90, Edad),
             Quinquenio=cut(Edad, seq(0,120,5), right = F)) %>%
      group_by(Quinquenio, Genero) %>%
      summarise(Personas= n_distinct(NumIdPersona)) %>% 
      mutate(Personas=ifelse(Genero=="M", Personas*-1, Personas),
             Genero=ifelse(Genero=="F", "Femenino", "Masculino"))
    
    p <- plot_ly(aux, x = ~Personas, y = ~Quinquenio, color = ~Genero, colors = c("#FA8072", "#87CEEB"),
                 hovertext = paste("<b>Quinquenio:</b>", aux$Quinquenio,
                                   "<br><b>Genero :</b>", aux$Genero,
                                   "<br><b> Personas: </b>", comma(aux$Personas))) %>%  
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(abs(Personas)), textposition = 'outside', 
      ) %>% 
      layout(bargap = 0.1, barmode = 'overlay', title = "Piramide Poblacional",
             xaxis = list(title = "Poblacion", range = c(-round(max(abs(aux$Personas))*1.4, digits = 0), round(max(abs(aux$Personas))*1.4, digits = 0)),
                          showline = F, showticklabels = F),
             yaxis = list(title = "Quinquenio"));
    p
  })
  
  # -------------------------------------- Output Plot Piramide poblacional Categoria --------------------------------------------------
  output$PiramideCategoriaA <- renderPlotly({
    aux <- infoAfiliados() %>%
      filter(Categoria == "A")%>%
      mutate(Edad=ifelse(Edad>=90, 90, Edad),
             Quinquenio=cut(Edad, seq(0,120,5), right = F)) %>%
      group_by(Quinquenio, Genero) %>%
      summarise(Personas= n_distinct(NumIdPersona)) %>% 
      mutate(Personas=ifelse(Genero=="M", Personas*-1, Personas),
             Genero=ifelse(Genero=="F", "Femenino", "Masculino"))
    
    p <- plot_ly(aux, x = ~Personas, y = ~Quinquenio, color = ~Genero, colors = c("#FA8072", "#87CEEB"),
                 hovertext = paste("<b>Quinquenio:</b>", aux$Quinquenio,
                                   "<br><b>Genero :</b>", aux$Genero,
                                   "<br><b> Personas: </b>", comma(aux$Personas))) %>%  
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(abs(Personas)), textposition = 'outside') %>% 
      layout(bargap = 0.1, barmode = 'overlay', title = "Categoria A",
             xaxis = list(title = "Poblacion", range = c(-round(max(abs(aux$Personas))*1.4, digits = 0), round(max(abs(aux$Personas))*1.4, digits = 0)),
                          showline = F, showticklabels = F),
             yaxis = list(title = "Quinquenio"));
    p
  })
  
  output$PiramideCategoriaB <- renderPlotly({
    aux <- infoAfiliados() %>%
      filter(Categoria == "B")%>%
      mutate(Edad=ifelse(Edad>=90, 90, Edad),
             Quinquenio=cut(Edad, seq(0,120,5), right = F)) %>%
      group_by(Quinquenio, Genero) %>%
      summarise(Personas= n_distinct(NumIdPersona)) %>% 
      mutate(Personas=ifelse(Genero=="M", Personas*-1, Personas),
             Genero=ifelse(Genero=="F", "Femenino", "Masculino"))
    
    p <- plot_ly(aux, x = ~Personas, y = ~Quinquenio, color = ~Genero, colors = c("#FA8072", "#87CEEB"),
                 hovertext = paste("<b>Quinquenio:</b>", aux$Quinquenio,
                                   "<br><b>Genero :</b>", aux$Genero,
                                   "<br><b> Personas: </b>", comma(aux$Personas))) %>%  
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(abs(Personas)), textposition = 'outside') %>% 
      layout(bargap = 0.1, barmode = 'overlay', title = "Categoria B",
             xaxis = list(title = "Poblacion", range = c(-round(max(abs(aux$Personas))*1.4, digits = 0), round(max(abs(aux$Personas))*1.4, digits = 0)),
                          showline = F, showticklabels = F),
             yaxis = list(title = "Quinquenio"));
    p
  })
  
  output$PiramideCategoriaC <- renderPlotly({
    aux <- infoAfiliados() %>%
      filter(Categoria == "C")%>%
      mutate(Edad=ifelse(Edad>=90, 90, Edad),
             Quinquenio=cut(Edad, seq(0,120,5), right = F)) %>%
      group_by(Quinquenio, Genero) %>%
      summarise(Personas= n_distinct(NumIdPersona)) %>% 
      mutate(Personas=ifelse(Genero=="M", Personas*-1, Personas),
             Genero=ifelse(Genero=="F", "Femenino", "Masculino"))
    
    p <- plot_ly(aux, x = ~Personas, y = ~Quinquenio, color = ~Genero, colors = c("#FA8072", "#87CEEB"),
                 hovertext = paste("<b>Quinquenio:</b>", aux$Quinquenio,
                                   "<br><b>Genero :</b>", aux$Genero,
                                   "<br><b> Personas: </b>", comma(aux$Personas))) %>%  
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(abs(Personas)), textposition = 'outside') %>% 
      layout(bargap = 0.1, barmode = 'overlay', title = "Categoria C",
             xaxis = list(title = "Poblacion", range = c(-round(max(abs(aux$Personas))*1.4, digits = 0), round(max(abs(aux$Personas))*1.4, digits = 0)),
                          showline = F, showticklabels = F),
             yaxis = list(title = "Quinquenio"));
    p
  })
  
  # -------------------------------------- Output Plot Piramide poblacional Segmento --------------------------------------------------
  #Alto Básico Joven Medio
  output$PiramideSegmentoBasico <- renderPlotly({
    aux <- infoAfiliados() %>%
      filter(Segmento_poblacional == "Básico")%>%
      mutate(Edad=ifelse(Edad>=90, 90, Edad),
             Quinquenio=cut(Edad, seq(0,120,5), right = F)) %>%
      group_by(Quinquenio, Genero) %>%
      summarise(Personas= n_distinct(NumIdPersona)) %>% 
      mutate(Personas=ifelse(Genero=="M", Personas*-1, Personas),
             Genero=ifelse(Genero=="F", "Femenino", "Masculino"))
    
    p <- plot_ly(aux, x = ~Personas, y = ~Quinquenio, color = ~Genero, colors = c("#FA8072", "#87CEEB"),
                 hovertext = paste("<b>Quinquenio:</b>", aux$Quinquenio,
                                   "<br><b>Genero :</b>", aux$Genero,
                                   "<br><b> Personas: </b>", comma(aux$Personas))) %>%  
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(abs(Personas)), textposition = 'outside') %>% 
      layout(bargap = 0.1, barmode = 'overlay', title = "Segmento Básico",
             xaxis = list(title = "Poblacion", range = c(-round(max(abs(aux$Personas))*1.4, digits = 0), round(max(abs(aux$Personas))*1.4, digits = 0)),
                          showline = F, showticklabels = F),
             yaxis = list(title = "Quinquenio"));
    p
  })
  
  output$PiramideSegmentoMedio <- renderPlotly({
    aux <- infoAfiliados() %>%
      filter(Segmento_poblacional == "Medio")%>%
      mutate(Edad=ifelse(Edad>=90, 90, Edad),
             Quinquenio=cut(Edad, seq(0,120,5), right = F)) %>%
      group_by(Quinquenio, Genero) %>%
      summarise(Personas= n_distinct(NumIdPersona)) %>% 
      mutate(Personas=ifelse(Genero=="M", Personas*-1, Personas),
             Genero=ifelse(Genero=="F", "Femenino", "Masculino"))
    
    p <- plot_ly(aux, x = ~Personas, y = ~Quinquenio, color = ~Genero, colors = c("#FA8072", "#87CEEB"),
                 hovertext = paste("<b>Quinquenio:</b>", aux$Quinquenio,
                                   "<br><b>Genero :</b>", aux$Genero,
                                   "<br><b> Personas: </b>", comma(aux$Personas))) %>%  
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(abs(Personas)), textposition = 'outside') %>% 
      layout(bargap = 0.1, barmode = 'overlay', title = "Segmento Medio",
             xaxis = list(title = "Poblacion", range = c(-round(max(abs(aux$Personas))*1.4, digits = 0), round(max(abs(aux$Personas))*1.4, digits = 0)),
                          showline = F, showticklabels = F),
             yaxis = list(title = "Quinquenio"));
    p
  })
  
  output$PiramideSegmentoJoven <- renderPlotly({
    aux <- infoAfiliados() %>%
      filter(Segmento_poblacional == "Joven")%>%
      mutate(Edad=ifelse(Edad>=90, 90, Edad),
             Quinquenio=cut(Edad, seq(0,120,5), right = F)) %>%
      group_by(Quinquenio, Genero) %>%
      summarise(Personas= n_distinct(NumIdPersona)) %>% 
      mutate(Personas=ifelse(Genero=="M", Personas*-1, Personas),
             Genero=ifelse(Genero=="F", "Femenino", "Masculino"))
    
    p <- plot_ly(aux, x = ~Personas, y = ~Quinquenio, color = ~Genero, colors = c("#FA8072", "#87CEEB"),
                 hovertext = paste("<b>Quinquenio:</b>", aux$Quinquenio,
                                   "<br><b>Genero :</b>", aux$Genero,
                                   "<br><b> Personas: </b>", comma(aux$Personas))) %>%  
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(abs(Personas)), textposition = 'outside') %>% 
      layout(bargap = 0.1, barmode = 'overlay', title = "Segmento Joven",
             xaxis = list(title = "Poblacion", range = c(-round(max(abs(aux$Personas))*1.4, digits = 0), round(max(abs(aux$Personas))*1.4, digits = 0)),
                          showline = F, showticklabels = F),
             yaxis = list(title = "Quinquenio"));
    p
  })
  
  output$PiramideSegmentoAlto <- renderPlotly({
    aux <- infoAfiliados() %>%
      filter(Segmento_poblacional == "Alto")%>%
      mutate(Edad=ifelse(Edad>=90, 90, Edad),
             Quinquenio=cut(Edad, seq(0,120,5), right = F)) %>%
      group_by(Quinquenio, Genero) %>%
      summarise(Personas= n_distinct(NumIdPersona)) %>% 
      mutate(Personas=ifelse(Genero=="M", Personas*-1, Personas),
             Genero=ifelse(Genero=="F", "Femenino", "Masculino"))
    
    p <- plot_ly(aux, x = ~Personas, y = ~Quinquenio, color = ~Genero, colors = c("#FA8072", "#87CEEB"),
                 hovertext = paste("<b>Quinquenio:</b>", aux$Quinquenio,
                                   "<br><b>Genero :</b>", aux$Genero,
                                   "<br><b> Personas: </b>", comma(aux$Personas))) %>%  
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(abs(Personas)), textposition = 'outside', 
      ) %>% 
      layout(bargap = 0.1, barmode = 'overlay', title = "Segmento Alto",
             xaxis = list(title = "Poblacion", range = c(-round(max(abs(aux$Personas))*1.4, digits = 0), round(max(abs(aux$Personas))*1.4, digits = 0)),
                          showline = F, showticklabels = F),
             yaxis = list(title = "Quinquenio"));
    p
  })
  
  # -------------------------------------- Output Plot pie ------------------ --------------------------------------------------
  
  output$PieSegmento <- renderPlotly({
    data <- infoAfiliados() %>% group_by(Segmento_poblacional) %>% summarise(valor = n())
    p <- plot_ly(data, labels = ~Segmento_poblacional, values = ~valor, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 text = ~paste(valor, ' Afiliados'),
                 marker = list(
                   line = list(color = '#FFFFFF', width = 1)),
                 showlegend = T) %>%
      layout(title = 'Afiliados por Segmneto Poblacional',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p
  })
  
  output$PieCategoria <- renderPlotly({
    data <- infoAfiliados() %>% group_by(Categoria) %>% summarise(valor = n())
    p <- plot_ly(data, labels = ~Categoria, values = ~valor, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 text = ~paste(valor, ' Afiliados'),
                 marker = list(
                   line = list(color = '#FFFFFF', width = 1)),
                 showlegend = T) %>%
      layout(title = 'Afiliados por Categoria',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p
  })
  
  output$PieGenero <- renderPlotly({
    data <- infoAfiliados() %>% group_by(Genero) %>% summarise(valor = n())
    p <- plot_ly(data, labels = ~Genero, values = ~valor, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 text = ~paste(valor, ' Afiliados'),
                 marker = list(
                   line = list(color = '#FFFFFF', width = 1)),
                 showlegend = T) %>%
      layout(title = 'Afiliados por Genero',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p
  })
  
  # -------------------------------------- Output horizontal bar ---------------------------------------------------------------------
  output$HorizontalBarCategoria <- renderPlotly({
    aux1 <- infoAfiliados() %>% group_by() %>% summarise("conteo" = n())
    aux <- infoAfiliados() %>% group_by(Categoria) %>% summarise("conteo" = n())
    
    var <- aux %>% filter(Categoria == "A")
    x1 <- round(var[1,2]/aux1*100) 
    
    var <- aux %>% filter(Categoria == "B")
    x2 <- round(var[1,2]/aux1*100)
    
    var <- aux %>% filter(Categoria == "C")
    x3 <- round(var[1,2]/aux1*100)
    
    top_labels <- c("A", 'B', 'C')
    y <- c('CATEGORIA DE LOS AFILIADOS')
    
    data <- data.frame(y, x1, x2, x3)
    
    names(data) <- c("y","x1","x2","x3")
    
    p <- plot_ly(data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
                 marker = list(color = 'rgba(38, 24, 74, 0.8)',
                               line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
      add_trace(x = ~x2, marker = list(color = 'rgba(71, 58, 131, 0.8)')) %>%
      add_trace(x = ~x3, marker = list(color = 'rgba(122, 120, 168, 0.8)')) %>%
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE,
                          domain = c(0.15, 1)),
             yaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE),
             barmode = 'stack',
             paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
             margin = list(l = 120, r = 10, t = 140, b = 80),
             showlegend = F) %>%
      # labeling the y-axis
      add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                      xanchor = 'right',
                      text = y,
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE, align = 'right') %>%
      # labeling the percentages of each bar (x_axis)
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 / 2, y = y,
                      text = paste(top_labels[1],data[,"x1"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 / 2, y = y,
                      text = paste(top_labels[2],"  (",data[,"x2"], '% )'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 + x3 / 2, y = y,
                      text = paste(top_labels[3],data[,"x3"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      # labeling the first Likert scale (on the top)
      add_annotations(xref = 'x', yref = 'paper',
                      x = 100 / 2,
                      y = 1.5,
                      text = "CATEGORIA (OPCIONES: A, B y C)",
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE)
    p
  })
  
  output$HorizontalBarCategoria2 <- renderPlotly({
    #---------------------
    aux1 <- infoAfiliados() %>% group_by() %>% summarise("conteo" = n())
    aux <- infoAfiliados() %>% group_by(Segmento_poblacional) %>% summarise("conteo" = n())
    
    var <- aux %>% filter(Segmento_poblacional == "Básico")
    x1 <- round(var[1,2]/aux1*100) 
    
    var <- aux %>% filter(Segmento_poblacional == "Joven")
    x2 <- round(var[1,2]/aux1*100)
    
    var <- aux %>% filter(Segmento_poblacional == "Medio")
    x3 <- round(var[1,2]/aux1*100)
    
    var <- aux %>% filter(Segmento_poblacional == "Alto")
    x4 <- round(var[1,2]/aux1*100)
    
    top_labels <- c("Básico", 'Joven', 'Medio',"Alto")
    y <- c('SEGMENTO POBLACIONAL')
    
    data <- data.frame(y, x1, x2, x3, x4)
    
    names(data) <- c("y","x1","x2","x3","x4")
    #---------------------
    
    
    p <- plot_ly(data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
                 marker = list(color = 'rgba(38, 24, 74, 0.8)',
                               line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
      add_trace(x = ~x2, marker = list(color = 'rgba(71, 58, 131, 0.8)')) %>%
      add_trace(x = ~x3, marker = list(color = 'rgba(122, 120, 168, 0.8)')) %>%
      add_trace(x = ~x4, marker = list(color = 'rgba(164, 163, 204, 0.85)')) %>%
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE,
                          domain = c(0.15, 1)),
             yaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE),
             barmode = 'stack',
             paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
             margin = list(l = 120, r = 10, t = 140, b = 80),
             showlegend = F) %>%
      # labeling the y-axis
      add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                      xanchor = 'right',
                      text = y,
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE, align = 'right') %>%
      # labeling the percentages of each bar (x_axis)
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 / 2, y = y,
                      text = paste(top_labels[1],data[,"x1"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 / 2, y = y,
                      text = paste(top_labels[2],"  (",data[,"x2"], '% )'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 + x3 / 2, y = y,
                      text = paste(top_labels[3],data[,"x3"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 + x3 + x4 / 2, y = y,
                      text = paste(top_labels[4],data[,"x4"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      # labeling the first Likert scale (on the top)
      add_annotations(xref = 'x', yref = 'paper',
                      x = 100 / 2,
                      y = 1.5,
                      text = "SEGMENTO POBLACIONAL (OPCIONES: BASICO, JOVEN, MEDIO y ALTO)",
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE)
    p
  })
  
  # -------------------------------------- output plotly  -------------------------------------------------------------------------
  output$plotGrupoFamiliar <- renderPlotly({ 

    aux <- infoAfiliados() %>%  
      group_by(segmento_grupo_familiar) %>% 
      summarise(n = n()) %>% 
      mutate(abs_pop = abs(n)) %>% 
      mutate(freq = n / sum(n))
    var <- aux %>% plot_ly(x = ~freq, y = ~segmento_grupo_familiar, color = ~n, colors = "RdBu", orientation = 'h') %>%
      add_bars(hoverinfo = 'text', text = ~paste(abs_pop, "(", round(freq*100,2), "%)"),textposition = 'auto') %>%
      layout(title = "Grupo Familiar",
             xaxis = list(title = "% Personas", tickformat = "%"), bargap = 0.2, barmode = 'stack', 
             yaxis = list(title = " "),
             legend = list(x = 100, y = 0.5))
  })
  
  output$plotEstratoPersona <- renderPlotly({ 
  aux <- infoAfiliados() %>%  
    group_by(EstratoPersona) %>% 
    summarise(n = n()) %>% 
    mutate(abs_pop = abs(n)) %>% 
    mutate(freq = n / sum(n)) 
  var <- aux %>% plot_ly(x = ~freq, y = ~EstratoPersona, color = ~EstratoPersona, colors = "RdBu",orientation = 'h') %>%
    add_bars(hoverinfo = 'text', text = ~paste(abs_pop, "(", round(freq*100,2), "%)"),textposition = 'auto') %>%
    layout(title = "EstratoPersona",
           xaxis = list(title = "% Personas", tickformat = "%"), bargap = 0.2, barmode = 'stack', 
           yaxis = list(title = " "),
           legend = list(x = 100, y = 0.5))
  
  })
  
  output$plotPiramide1 <- renderPlotly({ 
    aux <- infoAfiliados() %>%  
      group_by(Piramide1) %>% 
      summarise(n = n()) %>% 
      mutate(abs_pop = abs(n)) %>% 
      mutate(freq = n / sum(n)) 
    var <- aux %>% plot_ly(x = ~freq, y = ~Piramide1, color = ~n, colors = "RdBu",orientation = 'h') %>%
      add_bars(hoverinfo = 'text', text = ~paste(abs_pop, "(", round(freq*100,2), "%)"),textposition = 'auto') %>%
      layout(title = "Piramide 1",
             xaxis = list(title = "% Personas", tickformat = "%"), bargap = 0.2, barmode = 'stack', 
             yaxis = list(title = " "),
             legend = list(x = 100, y = 0.5))
    
  })
  
  output$plotPiramide2 <- renderPlotly({ 
    aux <- infoAfiliados() %>%  
      group_by(Piramide2) %>% 
      summarise(n = n()) %>% 
      mutate(abs_pop = abs(n)) %>% 
      mutate(freq = n / sum(n)) 
    var <- aux %>% plot_ly(x = ~freq, y = ~Piramide2, color = ~n, colors = "RdBu",orientation = 'h') %>%
      add_bars(hoverinfo = 'text', text = ~paste(abs_pop, "(", round(freq*100,2), "%)"),textposition = 'auto') %>%
      layout(title = "Piramide 2",
             xaxis = list(title = "% Personas", tickformat = "%"), bargap = 0.2, barmode = 'stack', 
             yaxis = list(title = " "),
             legend = list(x = 100, y = 0.5))
    
  })
  #-------------------------------------- Button -----------------------------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Informacion_Afiliados", "csv", sep = ".")
    },
        content = function(file) {
      sep <- ","
      var <- infoAfiliados() %>% select(id_persona,Nombre, NumIdPersona, Genero, Edad, Categoria,segmento_grupo_familiar, Segmento_poblacional,id_empresa,RazonSocial,Piramide1,Piramide2)
      write.table(var, file, sep = sep,
                  row.names = FALSE)
        }
  )
  
  # -------------------------------------- output box -----------------------------------------------------------------------------
  output$totalAfiliados <- renderInfoBox({
    infoBox(
      value = tags$p(comma(nrow(infoAfiliados())), style = "font-size: 145%;"),
      title = "Total Afiliados",
      icon = icon("users"),
      fill = T,
      color = "blue"
    )
  })
  
  output$totalAfiliadosCategoriaA <- renderInfoBox({
    aux <- nrow(infoAfiliados() %>% filter(Categoria == "A"))
    aux2 <- (aux/nrow(infoAfiliados()))
    infoBox(
      value = tags$p((paste0(aux,"   (",percent(aux2),")")), style = "font-size: 145%;"),
      title = "Categoria A",
      icon = icon("users"),
      fill = F,
      color = "blue"
    )
  })
  
  output$totalAfiliadosCategoriaB <- renderInfoBox({
    aux <- nrow(infoAfiliados() %>% filter(Categoria == "B"))
    aux2 <- (aux/nrow(infoAfiliados()))
    infoBox(
      value = tags$p((paste0(aux,"   (",percent(aux2),")")), style = "font-size: 145%;"),
      title = "Categoria B",
      icon = icon("users"),
      fill = F,
      color = "blue"
    )
  })
  
  output$totalAfiliadosCategoriaC <- renderInfoBox({
    aux <- nrow(infoAfiliados() %>% filter(Categoria == "C"))
    aux2 <- (aux/nrow(infoAfiliados()))
    infoBox(
      value = tags$p((paste0(aux,"   (",percent(aux2),")")), style = "font-size: 145%;"),
      title = "Categoria C",
      icon = icon("users"),
      fill = F,
      color = "blue"
    )
  })
  
  output$SegmentoPoblacionalBasico <- renderInfoBox({
    aux <- nrow(infoAfiliados() %>% filter(Segmento_poblacional == "Básico"))
    aux2 <- (aux/nrow(infoAfiliados()))
    infoBox(
      value = tags$p((paste0(aux,"   (",percent(aux2),")")), style = "font-size: 145%;"),
      title = "Básico",
      icon = icon("users"),
      fill = F,
      color = "blue"
    )
  })
  
  output$SegmentoPoblacionalJoven <- renderInfoBox({
    aux <- nrow(infoAfiliados() %>% filter(Segmento_poblacional == "Joven"))
    aux2 <- (aux/nrow(infoAfiliados()))
    infoBox(
      value = tags$p((paste0(aux,"   (",percent(aux2),")")), style = "font-size: 145%;"),
      title = "Joven",
      icon = icon("users"),
      fill = F,
      color = "blue"
    )
  })
  
  output$SegmentoPoblacionalMedio <- renderInfoBox({
    aux <- nrow(infoAfiliados() %>% filter(Segmento_poblacional == "Medio"))
    aux2 <- (aux/nrow(infoAfiliados()))
    infoBox(
      value = tags$p((paste0(aux,"   (",percent(aux2),")")), style = "font-size: 145%;"),
      title = "Medio",
      icon = icon("users"),
      fill = F,
      color = "blue"
    )
  })
  
  output$SegmentoPoblacionalAlto <- renderInfoBox({
    aux <- nrow(infoAfiliados() %>% filter(Segmento_poblacional == "Alto"))
    aux2 <- (aux/nrow(infoAfiliados()))
    infoBox(
      value = tags$p((paste0(aux,"   (",percent(aux2),")")), style = "font-size: 145%;"),
      title = "Alto",
      icon = icon("users"),
      fill = F,
      color = "blue"
    )
  })
  
  output$TotalRegistros <- renderUI({
    boxPlus(
      closable = F, 
      width = NULL,
      enable_label = F,
      label_text = 1,
      label_status = "danger",
      status = "danger", 
      solidHeader = F, 
      collapsible = F,
      descriptionBlock(
        number_color = "green", 
        header =  comma(nrow(data())), 
        text = "TOTAL REGISTROS", 
        right_border = TRUE,
        margin_bottom = FALSE
      )
    )
  })
  
  output$TotalRegistrosUnicos <- renderUI({
    boxPlus(
      closable = F, 
      width = NULL,
      enable_label = F,
      label_text = 1,
      label_status = "danger",
      status = "danger", 
      solidHeader = F, 
      collapsible = F,
      descriptionBlock(
        number_color = "green", 
        header = comma(nrow(unique(data()))) , 
        text = "TOTAL REGISTROS UNICOS", 
        right_border = TRUE,
        margin_bottom = FALSE
      )
    )
  })
})
