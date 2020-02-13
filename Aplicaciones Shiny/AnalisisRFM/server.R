shinyServer(function(input, output, session){
  
  # -------------------------------------------------------- Functions --------------------------------------------------------
  ftnNormalizar <- function(dfClientes){
    dfClientes <- dfClientes %>%
      mutate(Recencia = as.numeric(difftime(Sys.Date(),fecha,units = "days")))
    if (input$radio == 1){
      dfClientes <- dfClientes %>%
        group_by(identificador) %>%
        summarise(Recencia = min(Recencia),
                  Frecuencia = n_distinct(fecha),
                  Monto = sum(valor))
      return(dfClientes)
    }
    dfClientes <- dfClientes %>%
      group_by(identificador) %>%
      summarise(Recencia = min(Recencia),
                Frecuencia = n_distinct(fecha),
                Monto=sum(valor)/Frecuencia)
    return(dfClientes)
  }
  
  ftnCalcularPuntaje <- function(dfClientes){
    aux <- dfClientes %>%
      mutate(R_Score = cut2(Recencia, g = 5),
             F_Score = cut2(Frecuencia, g = 5),
             M_Score = cut2(Monto, g = 5))
    levels(aux$R_Score) <- seq(5,1, by = -1)
    levels(aux$F_Score) <- seq(1,5)
    levels(aux$M_Score) <- seq(1,5)
    return(aux)
  }
  
  ftnTransformarTipo <- function(valor){
    return(as.numeric(as.character(valor)))
  }
  
  ftnCalificar <- function(dfClientes){
    dfClientes <- dfClientes %>% mutate(
      Puntaje = ftnTransformarTipo(dfClientes$R_Score)*ftnTransformarTipo(dfClientes$F_Score)*ftnTransformarTipo(dfClientes$M_Score))
    dfClientes <- dfClientes %>% 
      mutate(
        Calificacion = case_when(
          Puntaje >= 80 ~ "1. Alto",
          Puntaje <80 & Puntaje>= 40 ~ "2. Medio",
          TRUE ~ "3. Bajo")
      ) 
  }
  
  # -------------------------------------------------------- Reactive database  -----------------------------------------------
  data <- reactive({
    options(scipen=999)  
    req(input$file1)
    data <- fread(input$file1$datapath)
    if(ncol(data) == 3){
      data <- data.frame(data,'Sin agrupador','Sin agrupador')
    }else if (ncol(data) == 4){
      data <- data.frame(data,'Sin agrupador')
    }else if (ncol(data) > 5){
      data <- data[,1:5]
    }
    return(data)
  })
  
  nombreBaseDatos <- reactive({
    nombres <-names(data())
    return(nombres)
  })
  
  output$NombreAgrupador1 <-  renderText({nombreBaseDatos()[4]}) 
  output$NombreAgrupador5 <- renderPrint({ nombreBaseDatos()[5] })
  
  baseDatos <- reactive({
    aux <- data()
    names(aux) <- c('identificador','fecha', 'valor','agrupador1','agrupador2')
    aux$valor <- as.numeric(aux$valor)
    aux$fecha <- as.Date(aux$fecha, "%d/%m/%Y")
    aux <- na.omit(aux)
  })
  
  observe({
    updatePickerInput(session,"Agrupador1Pick",
                      choices = unique(baseDatos()$agrupador1),
                      selected = unique(baseDatos()$agrupador1))
  })
  
  data_prod1 <- reactive({
    aux <- baseDatos() %>% 
      filter(agrupador1 %in% input$Agrupador1Pick)
    prods <- unique(aux$agrupador2)
    return(prods)
  })
  
  observe({
    updatePickerInput(session,"Agrupador2Pick",
                      choices = data_prod1(),
                      selected = data_prod1())
    
  })
  
  baseDatosClientes <- eventReactive(input$btnActualizar,{
    baseDatosClientes <- baseDatos()%>% 
      filter(agrupador1 %in% input$Agrupador1Pick)%>% 
      filter(agrupador2 %in% input$Agrupador2Pick)
  }, ignoreNULL = F)
  
  bd_rfm <- eventReactive(input$btnActualizar,{
    bd_rfm <- ftnNormalizar(baseDatosClientes()) %>% ftnCalcularPuntaje() %>% ftnCalificar()
    return(bd_rfm)
  }, ignoreNULL = F)
  
  # -------------------------------------------------------- Download ----------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("bd_rfm_Score.csv")
    },
    content = function(file) {
      write.csv(bd_rfm(), file, row.names = FALSE, sep = ";")
    }
  )
  
  # -------------------------------------------------------- Output Tables----------------------------------------------------------
  
  #Frecuencia
  output$TablaFrecuencia <- renderTable({
    t1<-bd_rfm() %>% group_by(`Score`=F_Score) %>%
      summarise(`Min`=comma(round(min(Frecuencia))),
                `Max`=comma(round(max(Frecuencia))),
                Clientes=comma(round(n_distinct(identificador))))
  }, spacing = 'xs')
  
  #Recencia
  output$TablaRecencia <- renderTable({
    t1<-bd_rfm() %>% group_by(`Score`=R_Score) %>%
      summarise(`Min`=comma(round(min(Recencia))),
                `Max`=comma(round(max(Recencia))),
                Clientes=comma(round(n_distinct(identificador)))) %>%
      arrange(desc(Score))
  }, spacing = 'xs')
  
  #Monto
  output$TablaMonto <- renderTable({
    t1<-bd_rfm() %>% group_by(`Score`=M_Score) %>%
      summarise(`Min`=dollar(round(min(Monto))),
                `Max`=dollar(round(max(Monto))),
                Clientes=comma(round(n_distinct(identificador))))
  }, spacing = 'xs')
  
  output$contents <- renderTable({
    aux <- data()[1:20,]
    names(aux) <- nombreBaseDatos()
    return (aux)
  })
  
  output$contentsAgrupador <- renderTable({
    aux <-baseDatos() %>% select(agrupador1,agrupador2) %>% unique()
    names(aux) <- nombreBaseDatos()[4:5]
    return(aux)
  })
  
  # -------------------------------------------------------- output widgets----------------------------------------------------------
  
  output$numCLientes <- renderInfoBox({
    infoBox(
      value = comma(nrow(bd_rfm())),
      title = "Clientes",
      subtitle = paste0("Ingreso:  $ ",comma(sum(bd_rfm()$Monto))),
      icon = icon("users"),
      fill = T,
      color = "blue"
    )
  })
  
  output$numCLientesAlto <- renderInfoBox({
    aux <- bd_rfm() %>% filter(Calificacion == "1. Alto")
    
    infoBox(
      value = paste0(comma(nrow(aux)),"      (",round(((nrow(aux)/nrow(bd_rfm()))*100),1)," %)"),
      title = "Clientes de alto valor",
      subtitle = paste0("Ingreso:  $ ",comma(sum(aux$Monto)),"      (",round((sum(aux$Monto)/sum(bd_rfm()$Monto))*100)," %)"),
      icon = icon("arrow-circle-up"),
      fill = F,
      color = "green"
    )
  })
  
  output$numCLientesMedio <- renderInfoBox({
    aux <- bd_rfm() %>% filter(Calificacion == "2. Medio")
    infoBox(
      value = paste0(comma(nrow(aux)),"      (",round(((nrow(aux)/nrow(bd_rfm()))*100),1)," %)"),
      title = "Clientes de valor medio",
      subtitle = paste0("Ingreso:  $ ",comma(sum(aux$Monto)),"      (",round((sum(aux$Monto)/sum(bd_rfm()$Monto))*100)," %)"),
      icon = icon("balance-scale"),
      fill = F,
      color = "orange"
    )
  })
  
  output$numCLientesBajo <- renderInfoBox({
    aux <- bd_rfm() %>% filter(Calificacion == "3. Bajo")
    infoBox(
      value = paste0(comma(nrow(aux)),"      (",round(((nrow(aux)/nrow(bd_rfm()))*100),1)," %)"),
      title = "Clientes de bajo valor",
      subtitle = paste0("Ingreso:  $ ",comma(sum(aux$Monto)),"      (",round((sum(aux$Monto)/sum(bd_rfm()$Monto))*100)," %)"),
      icon = icon("arrow-circle-down"),
      fill = F,
      color = "red"
    )
  })
  
  # -------------------------------------------------------- Output Plot----------------------------------------------------------
  output$threemapAgrupador1 <- renderPlot({ 
    Resultado = baseDatosClientes() %>% left_join(select(bd_rfm(),identificador,Calificacion), by = "identificador")
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
    plot(c(0,1), c(0,1),axes=F, col="white")
    vps <- baseViewports()
    temp=Resultado
    .tm <<- treemap(temp, 
                    index="agrupador1", 
                    vSize="valor", 
                    vColor="valor",
                    type="value",
                    title = "",
                    palette="Blues",
                    border.col ="white",
                    position.legend="right",
                    fontsize.labels = 16,
                    title.legend="")
  })
  
  output$threemapAgrupador2 <- renderPlot({ 
    Resultado = baseDatosClientes() %>% left_join(select(bd_rfm(),identificador,Calificacion), by = "identificador")
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
    plot(c(0,1), c(0,1),axes=F, col="white")
    vps <- baseViewports()
    temp=Resultado
    .tm <<- treemap(temp, 
                    index="agrupador2", 
                    vSize="valor", 
                    vColor="valor",
                    type="value",
                    title = "",
                    palette="Blues",
                    border.col ="white",
                    position.legend="right",
                    fontsize.labels = 16,
                    title.legend="")
  })
  
  output$tortaDistribucionCalificacion <- renderPlotly({ 
    aux <- bd_rfm() %>%  
      group_by(Calificacion) %>% 
      summarise(n = round(n_distinct(identificador)))
    p <- plot_ly(aux, labels = ~Calificacion, values = ~n, type = 'pie') %>%
      layout(title = "",
             xaxis = list(title = "% Personas", tickformat = "%"), bargap = 0, barmode = 'stack',
             yaxis = list(title = " "),
             legend = list(x = 100, y = 0.5))%>% config(displayModeBar = F)
    return(p)
  })
  
  output$HBarChar <- renderPlotly({
    
    baseCalificada = baseDatosClientes() %>% left_join(bd_rfm() %>% select(identificador,Calificacion), by = "identificador")
    
    aux <- baseCalificada %>% 
      mutate(Calificacion=as.factor(Calificacion)) %>% 
      group_by(Calificacion, agrupador1) %>% 
      summarise(Monto=sum(valor))
    
    p <- plot_ly(aux, x=~agrupador1, y=~Monto, color = ~Calificacion, type = 'bar', hoverinfo='text',
                 hovertext = paste("<b>Categoria: </b>", aux$agrupador1,
                                   "<br><b> Monto:</b>", dollar(aux$Monto/1e6), "MM")) %>% 
      layout(barmode = 'stack',
             xaxis = list(title = nombreBaseDatos()[4]),
             title=paste("Ingresos por calificación de cliente y", nombreBaseDatos()[4])) %>% 
      config(displayModeBar = F)
    p
    
  })
  
  output$barTicketPromedio <- renderPlotly({
    aux <- bd_rfm() %>%  
      group_by(Calificacion) %>% 
      summarise(n = round(((sum(Monto))/(n_distinct(identificador))))) 
    aux <- aux[-8,] %>% 
      mutate(abs_pop = abs(n)) %>% 
      mutate(freq = n / sum(n))  
    aux$Calificacion <- as.factor(aux$Calificacion)
    aux$y <- " "
    aux %>%
      plot_ly(x = ~y, y = ~freq, color = ~Calificacion, colors = "Spectral") %>%
      add_bars(hoverinfo = 'text', text = ~paste(dollar(abs_pop) , "(", round(freq*100,2), "%)"),textposition = 'outside') %>%
      layout(title = "",
             yaxis = list(title = "% Valor", tickformat = "%"), bargap = 0.5, barmode = 'stack', 
             xaxis = list(title = " "),
             legend = list(x = 100, y = 0.5)) %>%
      config(displayModeBar = F)
    
  })
  
  output$ResumenRFM <- renderPlot({
    aux2 <- bd_rfm() %>%
      group_by(R_Score, F_Score) %>%
      summarise(MontoPromedio=median(Monto), Freq=n())
    p1 <- ggplot(aux2, aes(x=F_Score, y=Freq, fill=MontoPromedio))+
      facet_grid(R_Score~.)+
      geom_bar(stat = "identity")+
      ylim(0,max(aux2$Freq)*1.3)+
      geom_text(aes(label=comma(Freq)), position=position_dodge(width=0.5), vjust=-0.5, size=4.4)+
      labs(title = "Puntajes RFM",y="Puntaje Resencia", x="Puntaje Frecuencia")+
      scale_fill_gradient2(high="navy",mid="white",low="blue", labels = dollar,
                           name="Monto", midpoint = (max(aux2$MontoPromedio)+min(aux2$MontoPromedio))/2)+
      theme(text = element_text(size=12), plot.title = element_text(hjust = 0.5), legend.text = element_text(size=11),
            legend.position="bottom", legend.key.width = unit(6.5, "cm"))
    return(p1)
  })
})