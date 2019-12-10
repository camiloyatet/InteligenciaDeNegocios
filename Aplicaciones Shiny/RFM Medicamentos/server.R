function(input, output, session) {
    
    ### Bases de Datos reactivas ----
    
    # Listados de PLUs ingresados por el usuario
    data_PLU <- reactive({
        listado <- fread(input$plus$datapath, encoding = "UTF-8", na.strings = c(""), col.names = c("prod_id")) %>% 
            distinct()
    })
    
    # Filtros: En esta base se aplican los filtros seleccionados por el usuario
    data_filtro1 <-eventReactive(input$go,{
        
        if (is.null(input$plus)){
            data_f <- BD %>%
                filter(Fecha >= input$Fecha[1] & Fecha <=input$Fecha[2],
                       if (!(input$Patologia %in% "TODAS")) Patologia %in% input$Patologia else TRUE,
                       if (!(input$Categoria %in% "TODAS")) Categoria %in% input$Categoria else TRUE,
                       if (!(input$Proveedor %in% "TODOS")) prod_proveedor_nombre  %in% input$Proveedor else TRUE,
                       if (input$Habeas != "Todos") Autorizado==input$Habeas else TRUE,
                       if (input$Afiliado != "Todos") Afiliado==input$Afiliado else TRUE)
        } else {
            data_f <- BD %>%
                filter(Fecha >= input$Fecha[1] & Fecha <=input$Fecha[2],
                       if (!(input$Patologia %in% "TODAS")) Patologia %in% input$Patologia else TRUE,
                       if (!(input$Categoria %in% "TODAS")) Categoria %in% input$Categoria else TRUE,
                       if (!(input$Proveedor %in% "TODOS")) prod_proveedor_nombre  %in% input$Proveedor else TRUE,
                       if (input$Habeas != "Todos") Autorizado==input$Habeas else TRUE,
                       if (input$Afiliado != "Todos") Afiliado==input$Afiliado else TRUE) %>% 
                inner_join(data_PLU(), by="prod_id")
        }
        return(data_f)
    }, ignoreNULL = F)
    data_filtro2 <-eventReactive(input$go,{
        
        if (input$Afiliado == "Si"){
            data_f <- data_filtro1() %>%
                filter(
                    Edad >= input$Edad[1] & Edad <= input$Edad[2],
                    if (input$Cupo != "TODAS") EstadoCredito==input$Cupo else TRUE,
                    if (input$CategoriaAfil != "TODAS") CategoriaAfiliacion==input$CategoriaAfil else TRUE,
                    if (input$Genero != "Todos") Genero==input$Genero else TRUE,
                    if (!(input$Segmento %in% "TODOS")) Segmento %in% input$Segmento else TRUE
                )
        } else {
            data_f <- data_filtro1()
        }
        return(data_f)
    }, ignoreNULL = F)
    
    # RFM: Cálculo del Score RFM a partir de la base filtrada
    bd_rfm <- eventReactive(input$go,{
        aux1<-data_filtro2() %>% 
            select(NumIdPersona, Fecha,Venta_Bruta) %>%
            mutate(Recencia=as.numeric(difftime(as.Date(input$Fecha[2]),Fecha,units="days"))) %>%
            group_by(NumIdPersona) %>%
            summarise(Recencia = min(Recencia),
                      Frecuencia=n_distinct(Fecha),
                      Monto=sum(Venta_Bruta, na.rm = T)/Frecuencia
            ) %>%
            mutate(R_Score=cut2(Recencia, g = 5),
                   F_Score=cut2(Frecuencia, g = 5),
                   M_Score=cut2(Monto, g = 5))
        
        levels(aux1$R_Score)<-seq(5,1, by = -1)
        levels(aux1$F_Score)<-seq(1,5)
        levels(aux1$M_Score)<-seq(1,5)
        return(aux1)
    }, ignoreNULL = F)
    
    data_f <- eventReactive(input$go,{
        data_f <- data_filtro2() %>% 
            left_join(bd_rfm() %>% select(NumIdPersona, R_Score:M_Score), by="NumIdPersona") %>% 
            filter(if (input$Recencia !="Todos") R_Score %in% input$Recencia else TRUE,
                   if (input$Frecuencia !="Todos") F_Score %in% input$Frecuencia else TRUE,
                   if (input$Monto !="Todos") M_Score  %in% input$Monto else TRUE )
    }, ignoreNULL = F)
    
    ### Actualizacion de Inputs -----
    
    observeEvent(input$Afiliado, {
        if(input$Afiliado=="Si") shinyjs::enable("Cupo")
        else shinyjs::disable("Cupo")
    })
    observeEvent(input$Afiliado, {
        if(input$Afiliado=="Si") shinyjs::enable("CategoriaAfil")
        else shinyjs::disable("CategoriaAfil")
    })
    observeEvent(input$Afiliado, {
        if(input$Afiliado=="Si") shinyjs::enable("Edad")
        else shinyjs::disable("Edad")
    })
    observeEvent(input$Afiliado, {
        if(input$Afiliado=="Si") shinyjs::enable("Genero")
        else shinyjs::disable("Genero")
    })
    observeEvent(input$Afiliado, {
        if(input$Afiliado=="Si") shinyjs::enable("Segmento")
        else shinyjs::disable("Segmento")
    })

    
    ### Magnitudes Generales ----
    
    output$Transacciones <- renderValueBox({
        
        aux <- data_f() %>% summarise(n()) %>% as.numeric()
        valueBox(
            value = comma(aux),
            subtitle = "Numero de Transacciones",
            icon = icon("list", lib = "font-awesome"),
            color = "blue"
        )
    })
    output$DescargaTransacciones <- downloadHandler(
        filename = function() {
            paste0("Transacciones", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv2(data_f() %>% 
                           select(Cliente=NumIdPersona, Genero:Afiliado, nombre_Sucursal, Fecha, prod_id, prod_nombre, Patologia, Categoria, prod_proveedor_nombre, 
                                  Cantidad_Productos, Venta_Neta, Venta_Bruta), 
                       file, row.names = FALSE)
        }
    )
    
    output$Clientes <- renderValueBox({
        
        aux <- data_f() %>% summarise(n_distinct(NumIdPersona)) %>% as.numeric()
        valueBox(
            value = comma(aux),
            subtitle = "Numero de Clientes",
            icon = icon("user-friends", lib = "font-awesome"),
            color = "blue"
        )
    })
    output$DescargaClientes <- downloadHandler(
        filename = function() {
            paste0("Clientes", Sys.time(), ".csv")
        },
        content = function(file) {
            write.csv2(data_f() %>% select(NumIdPersona) %>% distinct() %>% left_join(bd_rfm(), by="NumIdPersona"), 
                       file, row.names = FALSE)
        }
    )
    output$Productos <- renderValueBox({
        
        aux <- data_f() %>% summarise(n_distinct(prod_id)) %>% as.numeric()
        valueBox(
            value = comma(aux),
            subtitle = "Numero de Productos Distintos",
            icon = icon("user-friends", lib = "font-awesome"),
            color = "blue"
        )
    })
    output$DescargaProductos <- downloadHandler(
        filename = function() {
            paste0("Productos", Sys.time(), ".csv")
        },
        content = function(file) {
            write.csv2(data_f() %>% group_by(prod_id) %>% 
                           summarise(VentaBruta=sum(Venta_Bruta), VentaNeta=sum(Venta_Neta), Clientes=n_distinct(NumIdPersona)), 
                       file, row.names = FALSE)
        }
    )
    
    output$Productos <- renderValueBox({
        
        aux <- data_f() %>% summarise(n_distinct(prod_id)) %>% as.numeric()
        valueBox(
            value = comma(aux),
            subtitle = "Numero de Productos Distintos",
            icon = icon("user-friends", lib = "font-awesome"),
            color = "blue"
        )
    })
    output$VentaNeta <- renderValueBox({
        
        aux <- data_f() %>% summarise(sum(Venta_Neta, na.rm = T)) %>% as.numeric()
        valueBox(
            value = paste(dollar(aux, scale=0.000001, accuracy = 1), "MM"),
            subtitle = "Venta Neta Total",
            icon = icon("money-bill", lib = "font-awesome"),
            color = "blue"
        )
    })
    output$VentaBruta <- renderValueBox({
        
        aux <- data_f() %>% summarise(sum(Venta_Bruta, na.rm = T)) %>% as.numeric()
        valueBox(
            value = paste(dollar(aux, scale=0.000001, accuracy = 1),"MM"),
            subtitle = "Venta Bruta Total",
            icon = icon("money-bill-alt", lib = "font-awesome"),
            color = "blue"
        )
    })
    output$TicketPromedio <- renderValueBox({
        
        aux <- data_f() %>% group_by(NumIdPersona, Fecha, Id_Sucursal) %>% summarise(Venta=sum(Venta_Bruta, na.rm = T)) %>% ungroup() %>% 
            summarise(mean(Venta, na.rm = T)) %>% as.numeric()
        
        valueBox(
            value = paste(dollar(aux, scale=1, accuracy = 1)),
            subtitle = "Ticket Promedio",
            icon = icon("hand-holding-usd", lib = "font-awesome"),
            color = "blue"
        )
    })
    
    output$MapaPunto <- renderLeaflet({
        
        aux <- data_f() %>%
            group_by(Id_Sucursal, nombre_Sucursal) %>%
            summarise(Venta_Bruta=sum(Venta_Bruta, na.rm = T),
                      Venta_Neta=sum(Venta_Neta, na.rm = T),
                      Clientes=n_distinct(NumIdPersona)
            ) %>%
            left_join(suc, by = "Id_Sucursal") %>%
            mutate_("VarColor"=input$VariableCol,
                    "VarTamano"=input$VariableRad) %>% 
            filter(!is.na(CX))
        
        labs <- lapply(seq(nrow(aux)), function(i) {
            paste0( '<p>', "Sucursal: ",aux[i, "nombre_Sucursal"],
                    '<br>',"Clientes: ",aux[i, "Clientes"],
                    '<br>',"Venta Bruta: ",round(aux[i, "Venta_Bruta"]/1000000, 2), "(MM)",
                    '<br>',"Venta Neta: " ,round(aux[i, "Venta_Neta"]/1000000, 2), "(MM)")
        })
        
        pal <- colorNumeric("RdBu", aux$VarColor, reverse = F)
        
        leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 5, maxZoom = 13)) %>%
             addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
            setView(lat=4.6477908, lng = -74.108419, zoom = 11) %>%
            addCircleMarkers(data = aux, lat =~CY , lng =~CX, label=lapply(labs, HTML),  color = ~pal(VarColor),
                             radius=~rescale(VarTamano, to = c(1,5))*input$Tamano, fillOpacity = 0.6, stroke = F,
                             labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                         style = list(
                                                             "color" = "black",
                                                             "font-family" = "serif",
                                                             "font-style" = "arial",
                                                             "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                             "font-size" = "12px",
                                                             "border-color" = "rgba(0,0,0,0.5)"
                                                         ))) %>%
            addLegend(data =aux , pal = pal, values = ~VarColor, opacity = 1, title = input$VariableCol)
    })
    
    ### Tab Resumen RFM ----
    
    output$ResumenRFM <- renderPlot({
        
        aux2<-bd_rfm() %>%
            group_by(R_Score, F_Score) %>%
            summarise(MontoPromedio=median(Monto), Freq=n())
        
        p1 <- ggplot(aux2, aes(x=F_Score, y=Freq, fill=MontoPromedio))+
            facet_grid(R_Score~.)+
            geom_bar(stat = "identity")+
            ylim(0,max(aux2$Freq)*1.3)+
            geom_text(aes(label=comma(Freq)), position=position_dodge(width=0.5), vjust=-0.5, size=4.4)+
            labs(title = "Puntajes RFM",y="Número de Pacientes", x="Puntaje Frecuencia")+
            scale_fill_gradient2(high="dodgerblue4",mid="gray84",low="firebrick1", labels = dollar,
                                 name="Monto", midpoint = (max(aux2$MontoPromedio)+min(aux2$MontoPromedio))/2)+
            theme(text = element_text(size=12), plot.title = element_text(hjust = 0.5), legend.text = element_text(size=11),
                  legend.position="bottom", legend.key.width = unit(6.5, "cm"))
        p1
    })
    
    #Frecuencia
    output$TablaFrecuencia <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=F_Score) %>%
            summarise(`Mínimo`=comma(round(min(Frecuencia))),
                      `Máximo`=comma(round(max(Frecuencia))),
                      Pacientes=comma(round(n_distinct(NumIdPersona))))
        
    }, spacing = 'xs')
    #Recencia
    output$TablaRecencia <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=R_Score) %>%
            summarise(`Mínimo`=comma(round(min(Recencia))),
                      `Máximo`=comma(round(max(Recencia))),
                      Pacientes=comma(round(n_distinct(NumIdPersona)))) %>% 
            arrange(desc(Score))
        
    }, spacing = 'xs')
    #Monto
    output$TablaMonto <- renderTable({
        t1<-bd_rfm() %>% group_by(`Score`=M_Score) %>%
            summarise(`Mínimo`=dollar(round(min(Monto))),
                      `Máximo`=dollar(round(max(Monto))),
                      Pacientes=comma(round(n_distinct(NumIdPersona))))
    }, spacing = 'xs')
    
    ### Tops Interactivos ----
    
    output$default <- renderText({
        print(dim(bd_rfm()))
    })
    
    output$TopDInamico <- renderDataTable({
        
        myCol1 <- paste0("desc(", input$VariableOrden,")")
        
        t1<-data_f() %>% group_by_(input$VariableTop) %>% 
            summarise(Pacientes=n_distinct(NumIdPersona),
                      VentaBruta=sum(Venta_Bruta, na.rm = T),
                      VentaNeta=sum(Venta_Neta, na.rm = T),
                      Frec=n(),
                      Cantidad=sum(Cantidad_Productos, na.rm = T),
                      UsoPromedio=Frec/Pacientes,
                      VentaBPromedio=VentaBruta/Frec,
                      VentaNPromedio=VentaNeta/Frec
            ) %>% 
            arrange_(.dots = c(myCol1))
        
        if(input$VariableTop =="Patologia") nom="Patología"
        if(input$VariableTop =="Categoria") nom="Categoría"
        if(input$VariableTop =="nombre_Sucursal") nom="Sucursal"
        if(input$VariableTop =="prod_nombre") nom="Producto"
        if(input$VariableTop =="prod_proveedor_nombre") nom="Proveedor"
        if(input$VariableTop =="jerarquia1") nom="Jerarquía 1" 
        if(input$VariableTop =="jerarquia2") nom="Jerarquía 2"
        if(input$VariableTop =="principioconcatenado") nom="Principio Activo"
        if(input$VariableTop =="NumIdPersona") nom="Paciente"
        
        datatable(t1, options=list(pageLength =input$top,dom = 't', searching= FALSE), rownames=F, 
                  colnames = c(nom,"Pacientes", "Venta Bruta","Venta Neta","Frecuencia de Compra","Unidades","Uso Promedio",
                               "Venta Bruta Promedio","Venta Neta Promedio")) %>% 
            formatCurrency(c("VentaBruta", "VentaNeta", "VentaBPromedio", "VentaNPromedio"), digits = 0) %>% 
            formatRound(c("Pacientes", "Cantidad", "Frec"), digits = 0) %>% 
            formatRound(c("UsoPromedio"), digits = 2)
        
    })
    
    
}
