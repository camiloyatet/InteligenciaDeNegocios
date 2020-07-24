shinyServer(function(input, output, session) {
  
  addClass(selector = "body", class = "sidebar-collapse")

  # Datos Reactivos ----
  data_input <- reactive({
    req(input$archivo)
    fread(input$archivo$datapath, col.names = c('nodocumento'), colClasses = rep('character', 1))
    })

  
  masivos <- reactive ({
    data %>% inner_join(data_input(), by="nodocumento")
  })
  
  nocoinc <- reactive({
    data_input() %>% anti_join(data, by="nodocumento")
  })
  
  
  ind <- reactive({
    req(input$Identificaicon)
    data %>% filter(nodocumento== input$Identificaicon)
  })
  
  
  ### Descargas ------
  
  output$vb_RiesgoMB <- renderInfoBox({
    
    aux <- data %>% filter(Riesgo=="Muy Bajo") %>% nrow()
    
    infoBox(title = "Riesgo Muy Bajo",
            subtitle = "Empleados",
            value = comma(aux),
            color = "green",
            icon = icon("check")
    )
  })
  
  output$DescargaRMB <- downloadHandler(
    filename = function() {
      paste0("RiesgoMuyBajo", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv2(data %>% filter(Riesgo=="Muy Bajo") %>% select(nodocumento, nombre, clasedecontrato, tipodevinculo:motivodeaprobaciongral, divisiondepersonal:areaorganizativa, Riesgo),
                                 file, row.names = FALSE)
    }
  )
  
  output$vb_RiesgoB <- renderInfoBox({
    
    aux <- data %>% filter(Riesgo=="Bajo") %>% nrow()
    
    infoBox(title = "Riesgo Bajo",
            subtitle = "Empleados",
            value = comma(aux),
            color = "yellow",
            icon = icon("exclamation-triangle")
    )
  })
  
  output$DescargaRB <- downloadHandler(
    filename = function() {
      paste0("RiesgoMuyBajo", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv2(data %>% filter(Riesgo=="Bajo") %>% select(nodocumento, nombre, clasedecontrato, tipodevinculo:motivodeaprobaciongral, divisiondepersonal:areaorganizativa, Riesgo),
                 file, row.names = FALSE)
    }
  )
  
  output$vb_RiesgoM <- renderInfoBox({
    
    aux <- data %>% filter(Riesgo=="Medio") %>% nrow()
    
    infoBox(title = "Riesgo Medio",
            subtitle = "Empleados",
            value = comma(aux),
            color = "orange",
            icon = icon("exclamation-circle")
    )
  })
  
  output$DescargaRM <- downloadHandler(
    filename = function() {
      paste0("RiesgoMuyBajo", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv2(data %>% filter(Riesgo=="Medio") %>% select(nodocumento, nombre, clasedecontrato, tipodevinculo:motivodeaprobaciongral, divisiondepersonal:areaorganizativa, Riesgo),
                 file, row.names = FALSE)
    }
  )
  
  output$vb_RiesgoA <- renderInfoBox({
    
    aux <- data %>% filter(Riesgo=="Alto") %>% nrow()
    
    infoBox(title = "Riesgo Alto",
            subtitle = "Empleados",
            value = comma(aux),
            color = "red",
            icon = icon("times-circle")
    )
  })
  
  output$DescargaRA <- downloadHandler(
    filename = function() {
      paste0("RiesgoMuyBajo", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv2(data %>% filter(Riesgo=="Alto") %>% select(nodocumento, nombre, clasedecontrato, tipodevinculo:motivodeaprobaciongral, divisiondepersonal:areaorganizativa, Riesgo),
                 file, row.names = FALSE)
    }
  )
  
  output$DescargaTotal <- downloadHandler(
    filename = function() {
      paste0("RiesgoMuyBajo", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv2(data %>% select(nodocumento, nombre, clasedecontrato, tipodevinculo:motivodeaprobaciongral, divisiondepersonal:areaorganizativa, Riesgo),
                 file, row.names = FALSE)
    }
  )
  
  ### Consulta masiva ----
  output$vb_Registros <- renderInfoBox({
    
    infoBox(title = "",
            subtitle = "Registros Cargados",
            value = comma(nrow(data_input())),
            color = "navy",
            icon = icon("upload")
    )
  })
  
  output$vb_coincidentes <- renderInfoBox({
    
    infoBox(title = "",
            subtitle = "Registros Coincidentes",
            value = comma(nrow(masivos())),
            color = "navy",
            icon = icon("check")
    )
  })
  
  output$NoCoincidentes <- renderDataTable({
    
    if(nrow(nocoinc())){
      aux <- nocoinc()
      datatable(aux, options=list(pageLength =20, dom = 'tl', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F ,
                colnames = c("Id Persona", "Razón Social", "Código CIIU"))
    }
    else {aux <- data.frame(Resultados="Todas las identificaciones son coincidentes con el modelo")}
    
    datatable(aux, options=list(pageLength =20, dom = 'tl', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F ,
              colnames = c("Resultados"))
    
  })
  output$DescargaTransacciones1 <- downloadHandler(
    filename = function() {
      paste0("NoCoincidentes", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv2(nocoinc(),file, row.names = FALSE)
    }
  )
  output$Calificados <- renderDataTable({
    
    aux <- masivos() %>% select(nodocumento, nombre, clasedecontrato, tipodevinculo:motivodeaprobaciongral, divisiondepersonal:areaorganizativa, Riesgo)
    
    datatable(aux, options=list(pageLength =20, dom = 'tl', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F)
    
  })
  output$DescargaTransacciones2 <- downloadHandler(
    filename = function() {
      paste0("Coincidentes", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv2(aux <- masivos() %>% select(nodocumento, nombre, clasedecontrato, tipodevinculo:motivodeaprobaciongral, divisiondepersonal:areaorganizativa, RiesgoFinal),file, row.names = FALSE)
    }
  )
  
  #### Calificacion Individual -----
  
  output$vb_Id <- renderInfoBox({
    
    aux <- ind() %>% select(nodocumento) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Identificacion del Empleado",
            value = aux,
            color = "navy",
            icon = icon("id-card")
    )
  })
  output$vb_Nombre <- renderInfoBox({
    
    aux <- ind() %>% select(nombre) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Nombre del Empleado",
            value = aux,
            color = "navy",
            icon = icon("file-signature")
    )
  })
  output$vb_Contrato <- renderInfoBox({
    
    aux <- ind() %>% select(clasedecontrato) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Tipo de Contrato",
            value = aux,
            color = "navy",
            icon = icon("file-contract")
    )
  })
  output$vb_Ubicacion <- renderInfoBox({
    
    aux <- ind() %>% select(ubicacion) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Ubicacion de Trabajo",
            value = aux,
            color = "navy",
            icon = icon("map-marked")
    )
  })
  
  output$vb_Division <- renderInfoBox({
    
    aux <- ind() %>% select(divisiondepersonal) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Division de Personal",
            value = aux,
            color = "light-blue",
            icon = icon("sitemap")
    )
  })
  output$vb_Subdivision <- renderInfoBox({
    
    aux <- ind() %>% select(subdivisiondepersonal) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Subdivision de Personal",
            value = aux,
            color = "light-blue",
            icon = icon("sitemap")
    )
  })
  output$vb_Area <- renderInfoBox({
    
    aux <- ind() %>% select(areaorganizativa) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Area de Personal",
            value = aux,
            color = "light-blue",
            icon = icon("sitemap")
    )
  })
  output$vb_Cargo <- renderInfoBox({
    
    aux <- ind() %>% select(posicion) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Posicion",
            value = aux,
            color = "light-blue",
            icon = icon("sitemap")
    )
  })
  
  output$vb_Score <- renderInfoBox({
    
    aux <- ind() %>% select(Score) %>% as.numeric()
    
    if(aux <= 200){
      infoBox(title = "",
              subtitle = "Score de Riesgo",
              value = aux,
              color = "olive",
              icon = icon("check")
      )
    }
    else if(aux <= 400){
      infoBox(title = "",
              subtitle = "Score de Riesgo",
              value = aux,
              color = "olive",
              icon = icon("check")
      )
    }
    else if(aux <= 700){
      infoBox(title = "",
              subtitle = "Score de Riesgo",
              value = aux,
              color = "orange",
              icon = icon("exclamation-triangle")
      )
    }
    else {
      infoBox(title = "",
              subtitle = "Score de Riesgo",
              value = aux,
              color = "red",
              icon = icon("exclamation-circle")
      )
    }
  })
  output$vb_Riesgo <- renderInfoBox({
    
    aux <- ind() %>% select(Riesgo) %>% as.character()
    
    if(aux=="Muy Bajo"){
      infoBox(title = "",
              subtitle = "Nivel de Riesgo",
              value = aux,
              color = "olive",
              icon = icon("check")
      )
    }
    else if(aux=="Bajo"){
      infoBox(title = "",
              subtitle = "Nivel de Riesgo",
              value = aux,
              color = "olive",
              icon = icon("check")
      )
    }
    else if(aux=="Medio"){
      infoBox(title = "",
              subtitle = "Nivel de Riesgo",
              value = aux,
              color = "orange",
              icon = icon("exclamation-triangle")
      )
    }
    else if(aux=="Alto"){
      infoBox(title = "",
              subtitle = "Nivel de Riesgo",
              value = aux,
              color = "red",
              icon = icon("exclamation-circle")
      )
    }

  })
  
})