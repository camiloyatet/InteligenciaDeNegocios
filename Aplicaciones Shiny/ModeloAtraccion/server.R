shinyServer(function(input, output, session) {
  
  addClass(selector = "body", class = "sidebar-collapse")

  # Datos Reactivos ----
  data <- reactive({
    req(input$archivo)
    fread(input$archivo$datapath, col.names = c('Id_Empresa','RazonSocial','Empleados','CIIU'), colClasses = rep('character', 4)) %>%
      mutate(CIIU=as.numeric(CIIU),
             Empleados=as.numeric(ifelse(is.na(Empleados) | Empleados < 1, 1, Empleados))) %>%
      left_join(CIIU, by = "CIIU") %>%
      left_join(Salario, by = "CIIU") %>%
      left_join(Relacion, by = "CIIU") %>%
      mutate(Salario=ifelse(is.na(Salario), mean(Salario, na.rm = T), Salario),
             RelacionAM_AP=ifelse(is.na(RelacionAM_AP), mean(RelacionAM_AP, na.rm = T), RelacionAM_AP)
      )
    })
  data_NC <- reactive({
    data() %>% filter(is.na(GrupoCIIU)) %>% select(Id_Empresa, RazonSocial, CIIU)
  })
  data_C <- reactive({
    
    aux <- data() %>% filter(!is.na(GrupoCIIU)) %>% mutate(TipoId='NIT')
    Preds=predict(modelo,aux)

    data_calif <- cbind(aux, Preds) %>%
      mutate(min_aporte=round((smlmv*Empleados)*0.04, 0),
             max_aporte=round((Salario*Empleados)*0.04, 0),
             Aporte=ifelse(Preds<min_aporte, min_aporte,
                           ifelse(Preds>max_aporte, max_aporte, Preds)),
             Remanente=Aporte*RelacionAM_AP,
             Piramide1=ifelse(Aporte<20000000, '4 Micro',
                              ifelse(Aporte<85000000, '3 Empresas Pymes',
                                     ifelse(Aporte<250000000, '2 Emp Medio', '1 Emp Grandes'))),
             Piramide2=ifelse(Aporte<20000000, '4.1 Estándar',
                              ifelse(Aporte<40000000, '3.2 VIP Estándar',
                                     ifelse(Aporte<85000000, '3.1 VIP',
                                            ifelse(Aporte<150000000, '2.2 Silver',
                                                   ifelse(Aporte<250000000, '2.1 Gold',
                                                          ifelse(Aporte<420000000, '1.2 Premium',"1.1 Platinum"))))))) %>%
      mutate_at(c("Aporte", "Remanente"), list(~round(.,0))) %>%
    select(-c(SectorEconomico:max_aporte)) %>%
    left_join(Sectores, by = c("CIIU")) %>%
    select(IdEmpresa=Id_Empresa, RazonSocial:CIIU,ActividadEconomica, SectorEconomico, DivisionCIIU, GrupoCIIU, DescripcionCIIU, Aporte:Piramide2) %>%
    mutate(RazonSocial=RazonSocial)

    return(data_calif)
  })
  data_ind <- reactive({
    req(input$CIIU, input$RazonSocial, input$Empleados, input$idempresa)
    aux <- data.frame(
      Id_Empresa=input$idempresa,
      RazonSocial=input$RazonSocial,
      Empleados=input$Empleados,
      CIIU=input$CIIU,
      TipoId='NIT',
      stringsAsFactors = F) %>% 
      mutate(CIIU=as.numeric(CIIU),
             Empleados=as.numeric(ifelse(is.na(Empleados) | Empleados < 1, 1, Empleados))) %>%
      left_join(CIIU, by = "CIIU") %>%
      left_join(Salario, by = "CIIU") %>%
      left_join(Relacion, by = "CIIU") %>%
      mutate(Salario=ifelse(is.na(Salario), mean(Salario, na.rm = T), Salario),
             RelacionAM_AP=ifelse(is.na(RelacionAM_AP), mean(RelacionAM_AP, na.rm = T), RelacionAM_AP)
      )
    
    Preds=predict(modelo,aux)

    data_calif <- cbind(aux, Preds) %>%
      mutate(min_aporte=round((smlmv*Empleados)*0.04, 0),
             max_aporte=round((Salario*Empleados)*0.04, 0),
             Aporte=ifelse(Preds<min_aporte, min_aporte,
                           ifelse(Preds>max_aporte, max_aporte, Preds)),
             Remanente=Aporte*RelacionAM_AP,
             Piramide1=ifelse(Aporte<20000000, '4 Micro',
                              ifelse(Aporte<85000000, '3 Empresas Pymes',
                                     ifelse(Aporte<250000000, '2 Emp Medio', '1 Emp Grandes'))),
             Piramide2=ifelse(Aporte<20000000, '4.1 Estándar',
                              ifelse(Aporte<40000000, '3.2 VIP Estándar',
                                     ifelse(Aporte<85000000, '3.1 VIP',
                                            ifelse(Aporte<150000000, '2.2 Silver',
                                                   ifelse(Aporte<250000000, '2.1 Gold',
                                                          ifelse(Aporte<420000000, '1.2 Premium',"1.1 Platinum"))))))) %>%
      mutate_at(c("Aporte", "Remanente"), list(~round(.,0))) %>%
      select(-c(SectorEconomico:max_aporte)) %>%
      left_join(Sectores, by = c("CIIU")) %>%
      select(IdEmpresa=Id_Empresa, RazonSocial:CIIU,ActividadEconomica, SectorEconomico, DivisionCIIU, GrupoCIIU, DescripcionCIIU, Aporte:Piramide2) %>%
      mutate(RazonSocial=RazonSocial)
    
    return(data_calif)
  })
  
  # Consulta masiva ----
  output$vb_Registros <- renderInfoBox({
    
    infoBox(title = "",
            subtitle = "Registros Cargados",
            value = comma(nrow(data())),
            color = "navy",
            icon = icon("upload")
    )
  })
  output$vb_coincidentes <- renderInfoBox({
    
    infoBox(title = "",
            subtitle = "Registros con CIIU Coincidente",
            value = comma(nrow(data_C())),
            color = "navy",
            icon = icon("check")
    )
  })
  output$vb_nocoincidentes <- renderInfoBox({
    
    infoBox(title = "",
            subtitle = "Registros sin CIIU Coincidente",
            value = comma(nrow(data_NC())),
            color = "navy",
            icon = icon("times")
    )
  })
  output$NoCoincidentes <- renderDataTable({
    
    if(nrow(data_NC())){
      aux <- data_NC()
      datatable(aux, options=list(pageLength =20, dom = 'tl', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F ,
                colnames = c("Id Persona", "Razón Social", "Código CIIU"))
    }
    else {aux <- data.frame(Resultados="Todos los CIIUS son coincidentes con el modelo")}
    
    datatable(aux, options=list(pageLength =20, dom = 'tl', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F ,
              colnames = c("Resultados"))
    
  })
  output$DescargaTransacciones1 <- downloadHandler(
    filename = function() {
      paste0("NoCoincidentes", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv2(data_NC(),file, row.names = FALSE)
    }
  )
  output$Calificados <- renderDataTable({
    
    aux <- data_C() %>% select(IdEmpresa, RazonSocial, Empleados, CIIU, DescripcionCIIU, Piramide1, Piramide2, Aporte, Remanente)
    
    datatable(aux, options=list(pageLength =20, dom = 'tl', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F, 
              colnames = c("IdEmpresa", "Razón Social", "Empleados", "CIIU", "Descripción CIIU", "Piámide 1", "Pirámide 2", "Aporte", "Remanente")) %>% 
      formatCurrency(c("Aporte", "Remanente"), digits = 0)
    
  })
  output$DescargaTransacciones2 <- downloadHandler(
    filename = function() {
      paste0("Coincidentes", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv2(data_C(),file, row.names = FALSE)
    }
  )
  
  # Consulta Individual ----

  output$vb_NIT <- renderInfoBox({
    
    aux <- data_ind() %>% select(IdEmpresa) %>% as.character()

    infoBox(title = "",
            subtitle = "Identificacion de la Empresa",
            value = aux,
            color = "navy",
            icon = icon("building")
    )
  })
  output$vb_RazonSocial <- renderInfoBox({

    aux <- data_ind() %>% select(RazonSocial) %>% as.character()

    infoBox(title = "",
            subtitle = "Razon Social de la Empresa",
            value = aux,
            color = "navy",
            icon = icon("file-signature")
    )
  })
  output$vb_Empleados <- renderInfoBox({

    aux <- data_ind() %>% select(Empleados) %>% as.character()

    infoBox(title = "",
            subtitle = "Numero de empleados",
            value = aux,
            color = "navy",
            icon = icon("users")
    )
  })
  output$vb_CIIU <- renderInfoBox({

    aux <- data_ind() %>% select(CIIU) %>% as.character()

    infoBox(title = "",
            subtitle = "Codigo CIIU",
            value = aux,
            color = "navy",
            icon = icon("industry")
    )
  })
  
  output$vb_Actividad <- renderInfoBox({

    aux <- data_ind() %>% select(ActividadEconomica) %>% as.character()

    infoBox(title = "",
            subtitle = "Actividad Economica",
            value = aux,
            color = "blue",
            icon = icon("industry")
    )
  })
  output$vb_Sector <- renderInfoBox({
    
    aux <- data_ind() %>% select(SectorEconomico) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Sector Economico",
            value = aux,
            color = "blue",
            icon = icon("industry")
    )
  })
  output$vb_Grupo <- renderInfoBox({
    
    aux <- data_ind() %>% select(GrupoCIIU) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Grupo Actividad Economica",
            value = aux,
            color = "blue",
            icon = icon("industry")
    )
  })
  output$vb_Descripcion <- renderInfoBox({
    
    aux <- data_ind() %>% select(DescripcionCIIU) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Descripcion CIIU",
            value = aux,
            color = "blue",
            icon = icon("industry")
    )
  })
  
  
  output$vb_Piramide1 <- renderInfoBox({
    
    aux <- data_ind() %>% select(Piramide1) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Piramide 1",
            value = aux,
            color = "aqua",
            icon = icon("sort-numeric-up")
    )
  })
  output$vb_Piramide2 <- renderInfoBox({
    
    aux <- data_ind() %>% select(Piramide2) %>% as.character()
    
    infoBox(title = "",
            subtitle = "Piramide 2",
            value = aux,
            color = "aqua",
            icon = icon("sort-numeric-up")
    )
  })
  output$vb_Aporte <- renderInfoBox({
    
    aux <- data_ind() %>% select(Aporte	) %>% as.numeric()
    
    infoBox(title = "",
            subtitle = "Aporte Estimado",
            value = dollar(round(aux, digits = 0)),
            color = "aqua",
            icon = icon("hand-holding-usd")
    )
  })
  output$vb_Remanante <- renderInfoBox({
    
    aux <- data_ind() %>% select(Remanente) %>% as.numeric()
    
    infoBox(title = "",
            subtitle = "Remanente	Estimado",
            value = dollar(round(aux, digits = 0)),
            color = "aqua",
            icon = icon("money-bill-wave")
    )
  })

  
})