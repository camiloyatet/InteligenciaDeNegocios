shinyServer(function(input, output, session) {
  
  data_prod1 <- reactive({ listado %>% filter(TipoEmpresa==input$estrategia) })
  outVar1 <- reactive({ unique(data_prod1()$RazonSocial ) })
  observe({ updateSelectInput(session, "EmpresaAfiliada",choices = outVar1()) })
  observe({ updateSelectInput(session, "EmpresaNoAfiliada",choices = outVar1()) })
  
  data_f <- reactive({
    
    aux <- listado %>% filter(RazonSocial==input$EmpresaAfiliada) %>% select(NIT) %>% as.numeric()
    
    if(input$estrategia=='Atracción'){
      listado %>% filter(NIT==aux)
    }
    else{
      afiliadas %>% filter(NumIdEmpresa_SD==aux)
    }
  })
  data_p <- reactive({
    aux <- listado %>% filter(RazonSocial==input$EmpresaAfiliada) %>% select(NIT) %>% as.numeric()
    
    personas %>% filter(NumIdEmpresa_SD==aux)
  })
  
  data_f_na <- reactive({
    
    aux <- listado %>% filter(RazonSocial==input$EmpresaNoAfiliada) %>% select(NIT) %>% as.numeric()
    
    noafiliadas %>% filter(IdEmpresa==aux)
    
  })
  
  data_f_vecinos <- reactive({
    
    aux <- listado %>% filter(RazonSocial==input$EmpresaNoAfiliada) %>% select(NIT) %>% as.numeric()
    
    vecinos %>% filter(IdEmpresa==aux)
    
  })

  #   output$test <- renderDataTable({
  #   data_f_na()
  # })
  # 
  
  #### Afiliadas ----
  
  ### Mapa ----
  
  output$mapa_afiliados <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 1, maxZoom = 15)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lat=4.60971, lng = -74.08175, zoom = 11) %>%
      addHeatmap(data = data_p(), lng = ~cx_persona, lat = ~cy_persona, blur = 50, radius = 20, max=0.6) %>% 
      addMarkers(data = data_f(), lng=~cx_empresa, lat=~cy_empresa, label = ~RazonSocial) %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="AGENCIA DE EMPLEO"), lng=~CX, lat=~CY, icon = Empleo , label = ~NOMBRE, group = 'AGENCIA DE EMPLEO') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="ALIMENTOS Y BEBIDAS"), lng=~CX, lat=~CY, icon = AyB , label = ~NOMBRE, group = 'ALIMENTOS Y BEBIDAS') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="CENTROS DE SERVICIO"), lng=~CX, lat=~CY, icon = CS , label = ~NOMBRE, group = 'CENTROS DE SERVICIO') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="CREDITO"), lng=~CX, lat=~CY, icon = Credito , label = ~NOMBRE, group = 'CREDITO') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="CULTURA"), lng=~CX, lat=~CY, icon = Cultura , label = ~NOMBRE, group = 'CULTURA') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="EDUCACION"), lng=~CX, lat=~CY, icon = Educacion , label = ~NOMBRE, group = 'EDUCACION') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="MERCADEO SOCIAL"), lng=~CX, lat=~CY, icon = Super , label = ~NOMBRE, group = 'MERCADEO SOCIAL') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="RECREACION Y TURISMO"), lng=~CX, lat=~CY, icon = RyT , label = ~NOMBRE, group = 'RECREACION Y TURISMO') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="SALUD"), lng=~CX, lat=~CY, icon = Salud , label = ~NOMBRE, group = 'SALUD') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="VIVIENDA"), lng=~CX, lat=~CY, icon = Vivienda , label = ~NOMBRE, group = 'VIVIENDA') %>% 
      addLayersControl(
        overlayGroups = c('MERCADEO SOCIAL','CREDITO','AGENCIA DE EMPLEO','ALIMENTOS Y BEBIDAS','CULTURA','EDUCACION','RECREACION Y TURISMO',
                          'SALUD','CENTROS DE SERVICIO','VIVIENDA'),
        options = layersControlOptions(collapsed = T), position = "bottomright")
    
  })
  
 
  ### Cajas -----
  output$po_box_NIT <- renderInfoBox({
    
    aux <- data_f() %>% select(NumIdEmpresa_SD) %>% distinct() %>% as.character()
    
    infoBox(title = "NIT (Sin dígito)",
            value = aux,
            color = "navy",
            icon = icon("building")
    )
    
  })
  output$po_box_Pir1 <- renderInfoBox({
    
    aux <- data_f() %>% select(Piramide1) %>% distinct() %>% as.character()
    
    infoBox(title = "Pirámide 1",
            value = aux,
            color = "navy",
            icon = icon("sort-by-attributes", lib="glyphicon")
    )
    
  })
  output$po_box_Pir2 <- renderInfoBox({
    
    aux <- data_f() %>% select(Piramide2) %>% distinct() %>% as.character()
    
    infoBox(title = "Pirámide 2",
            value = aux,
            color = "navy",
            icon = icon("sort-by-attributes", lib="glyphicon")
    )
    
  })
  output$po_box_CIIU1 <- renderInfoBox({
    
    aux <- data_f() %>% select(SectorCIIU) %>% distinct() %>% as.character()
    
    infoBox(title = "Sector Económico",
            value = aux,
            color = "navy",
            icon = icon("gopuram")
    )
    
  })
  output$po_box_CIIU2 <- renderInfoBox({
    
    aux <- data_f() %>% select(ActividadCIIU) %>% distinct() %>% as.character()
    
    infoBox(title = "Actividad Económica",
            value = aux,
            color = "navy",
            icon = icon("gopuram")
    )
    
  })
  output$po_box_CIIU3 <- renderInfoBox({
    
    aux <- data_f() %>% select(DescripcionCIIU) %>% distinct() %>% as.character()
    
    infoBox(title = "Descripcion CIIU",
            value = aux,
            color = "navy",
            icon = icon("gopuram")
    )
    
  })
  output$po_box_Aporte <- renderInfoBox({
    
    aux <- data_f() %>% select(promedio_aportes) %>% distinct() %>% as.numeric()
    
    infoBox(title = "Aporte Promedio",
            value = dollar(aux),
            color = "navy",
            icon = icon("money-bill")
    )
    
  })
  output$po_box_Remanente <- renderInfoBox({
    
    aux <- data_f() %>% select(promedio_remaneto) %>% distinct() %>% as.numeric()
    
    infoBox(title = "Remanente Neto Promedio",
            value = dollar(aux),
            color = "navy",
            icon = icon("hand-holding-usd")
    )
    
  })
  output$po_box_Empleados <- renderInfoBox({
    
    aux <- data_f() %>% select(NumEmpleados) %>% distinct() %>% as.numeric()
    
    infoBox(title = "Número de Empleados",
            value = comma(aux),
            color = "navy",
            icon = icon("user-friends")
    )
    
  })
  output$po_box_CATA <- renderInfoBox({
    
    aux <- data_f() %>% select(Cat_A) %>% distinct() %>% as.numeric()
    
    infoBox(title = "Número de Empleados Categoria A",
            value = comma(aux),
            color = "navy",
            icon = icon("user-friends")
    )
    
  })
  output$po_box_CATB <- renderInfoBox({
    
    aux <- data_f() %>% select(Cat_B) %>% distinct() %>% as.numeric()
    
    infoBox(title = "Número de Empleados Categoria B",
            value = comma(aux),
            color = "navy",
            icon = icon("user-friends")
    )
    
  })
  output$po_box_CATC <- renderInfoBox({
    
    aux <- data_f() %>% select(Cat_B) %>% distinct() %>% as.numeric()
    
    infoBox(title = "Número de Empleados Categoria C",
            value = comma(aux),
            color = "navy",
            icon = icon("user-friends")
    )
    
  })
  
### No Afiliadas ----
  
  ### Mapa ----
  
  output$mapa_no_afiliados <- renderLeaflet({
    
    aux_lat <- data_f_na() %>% select(Lat) %>% filter(row_number()==1) %>% as.numeric()
    aux_long <- data_f_na() %>% select(Long) %>% filter(row_number()==1) %>% as.numeric()
    
    leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 1, maxZoom = 15)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lat=aux_lat, lng = aux_long, zoom = 14) %>%
      addMarkers(data = data_f_na(), lng=~Long, lat=~Lat, label = ~RazonSocial) %>% 
      addCircles(data = data_f_na(), lng=~Long, lat=~Lat, color = "steelblue", radius = 2000) %>%
      addMarkers(data = Infraestructura %>% filter(UES=="AGENCIA DE EMPLEO"), lng=~CX, lat=~CY, icon = Empleo , label = ~NOMBRE, group = 'AGENCIA DE EMPLEO') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="ALIMENTOS Y BEBIDAS"), lng=~CX, lat=~CY, icon = AyB , label = ~NOMBRE, group = 'ALIMENTOS Y BEBIDAS') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="CENTROS DE SERVICIO"), lng=~CX, lat=~CY, icon = CS , label = ~NOMBRE, group = 'CENTROS DE SERVICIO') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="CREDITO"), lng=~CX, lat=~CY, icon = Credito , label = ~NOMBRE, group = 'CREDITO') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="CULTURA"), lng=~CX, lat=~CY, icon = Cultura , label = ~NOMBRE, group = 'CULTURA') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="EDUCACION"), lng=~CX, lat=~CY, icon = Educacion , label = ~NOMBRE, group = 'EDUCACION') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="MERCADEO SOCIAL"), lng=~CX, lat=~CY, icon = Super , label = ~NOMBRE, group = 'MERCADEO SOCIAL') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="RECREACION Y TURISMO"), lng=~CX, lat=~CY, icon = RyT , label = ~NOMBRE, group = 'RECREACION Y TURISMO') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="SALUD"), lng=~CX, lat=~CY, icon = Salud , label = ~NOMBRE, group = 'SALUD') %>% 
      addMarkers(data = Infraestructura %>% filter(UES=="VIVIENDA"), lng=~CX, lat=~CY, icon = Vivienda , label = ~NOMBRE, group = 'VIVIENDA') %>% 
      addLayersControl(
        overlayGroups = c('MERCADEO SOCIAL','CREDITO','AGENCIA DE EMPLEO','ALIMENTOS Y BEBIDAS','CULTURA','EDUCACION','RECREACION Y TURISMO',
                          'SALUD','CENTROS DE SERVICIO','VIVIENDA'),
        options = layersControlOptions(collapsed = T), position = "bottomright")
    
  })
  
  
  

  ### Cajas -----
  output$po_box_NIT_na <- renderInfoBox({
    
    aux <- data_f_na() %>% select(IdEmpresa) %>% distinct() %>% as.character()
    
    infoBox(title = "NIT (Sin dígito)",
            value = aux,
            color = "navy",
            icon = icon("building")
    )
    
  })
  output$po_box_Pir1_na <- renderInfoBox({
    
    aux <- data_f_na() %>% select(Piramide1) %>% distinct() %>% as.character()
    
    infoBox(title = "Pirámide 1 Estimada",
            value = aux,
            color = "navy",
            icon = icon("sort-by-attributes", lib="glyphicon")
    )
    
  })
  output$po_box_Pir2_na <- renderInfoBox({
    
    aux <- data_f_na() %>% select(Piramide2) %>% distinct() %>% as.character()
    
    infoBox(title = "Pirámide 2 Estimada",
            value = aux,
            color = "navy",
            icon = icon("sort-by-attributes", lib="glyphicon")
    )
    
  })
  output$po_box_CIIU1_na <- renderInfoBox({
    
    aux <- data_f_na() %>% select(SectorEconomico) %>% distinct() %>% as.character()
    
    infoBox(title = "Sector Económico",
            value = aux,
            color = "navy",
            icon = icon("gopuram")
    )
    
  })
  output$po_box_CIIU2_na <- renderInfoBox({
    
    aux <- data_f_na() %>% select(ActividadEconomica) %>% distinct() %>% as.character()
    
    infoBox(title = "Actividad Económica",
            value = aux,
            color = "navy",
            icon = icon("gopuram")
    )
    
  })
  output$po_box_CIIU3_na <- renderInfoBox({
    
    aux <- data_f_na() %>% select(DescripcionCIIU) %>% distinct() %>% as.character()
    
    infoBox(title = "Descripcion CIIU",
            value = aux,
            color = "navy",
            icon = icon("gopuram")
    )
    
  })
  output$po_box_Aporte_na <- renderInfoBox({
    
    aux <- data_f_na() %>% select(Aporte) %>% distinct() %>% as.numeric()
    
    infoBox(title = "Aporte Promedio Estimado",
            value = dollar(aux),
            color = "navy",
            icon = icon("money-bill")
    )
    
  })
  output$po_box_Remanente_na <- renderInfoBox({
    
    aux <- data_f_na() %>% select(Remanente) %>% distinct() %>% as.numeric()
    
    infoBox(title = "Remanente Neto Promedio Estimado",
            value = dollar(aux),
            color = "navy",
            icon = icon("hand-holding-usd")
    )
    
  })
  output$po_box_Empleados_na <- renderInfoBox({
    
    aux <- data_f_na() %>% select(Empleados) %>% distinct() %>% as.numeric()
    
    infoBox(title = "Número de Empleados",
            value = comma(aux),
            color = "navy",
            icon = icon("user-friends")
    )
    
  })
  output$po_box_Vecino1 <- renderInfoBox({
    
    aux1 <- data_f_vecinos() %>% select(IdEmpresa.1) %>% distinct() %>% filter(row_number()==1) %>% as.character()
    aux2 <- data_f_vecinos() %>% select(RazonSocial.1) %>% distinct() %>% filter(row_number()==1) %>% as.character()
    
    infoBox(title = aux1,
            value = aux2,
            color = "navy",
            icon = icon("city")
    )
    
  })
  output$po_box_Vecino2 <- renderInfoBox({
    
    aux1 <- data_f_vecinos() %>% select(IdEmpresa.1) %>% distinct() %>% filter(row_number()==2) %>% as.character()
    aux2 <- data_f_vecinos() %>% select(RazonSocial.1) %>% distinct() %>% filter(row_number()==2) %>% as.character()
    
    infoBox(title = aux1,
            value = aux2,
            color = "navy",
            icon = icon("city")
    )
    
  })
  output$po_box_Vecino3 <- renderInfoBox({
    
    aux1 <- data_f_vecinos() %>% select(IdEmpresa.1) %>% distinct() %>% filter(row_number()==3) %>% as.character()
    aux2 <- data_f_vecinos() %>% select(RazonSocial.1) %>% distinct() %>% filter(row_number()==3) %>% as.character()
    
    infoBox(title = aux1,
            value = aux2,
            color = "navy",
            icon = icon("city")
    )
    
  })

  
})
