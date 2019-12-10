function(input, output, session) {
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  values <- reactiveValues()
    
    output$DescargaTransacciones <- downloadHandler(
        filename = function() {
            paste0("Transacciones", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv2(Resultados,
                       file, row.names = FALSE)
        }
    )
    
### Bases de Datos reactivas ----
    
    data_fil <-reactive({
        aux <- data 
        })
    
    data_res <-reactive({
      aux <- Resultados 
      })
    

### Pestaña RFM ----
    
    output$ResumenRFM <- renderPlot({
        
        aux1<-data_fil() %>%
            group_by(R_Score, F_Score) %>%
            summarise(MontoPromedio=median(Monto), 
                      Freq=n())
        
        p1 <- ggplot(aux1, aes(x=F_Score, y=Freq, fill=MontoPromedio))+
            facet_grid(R_Score~.)+
            geom_bar(stat = "identity")+
            ylim(0,max(aux1$Freq)*1.3)+
            geom_text(aes(label=comma(Freq)), position=position_dodge(width=0.5), vjust=-0.5, size=4.4)+
            labs(title = "Puntajes RFM",y="Número Productos", x="Puntaje Frecuencia")+
            scale_fill_gradient2(high="dodgerblue4",mid="gray84",low="firebrick1", labels = dollar,
                                 name="Monto", midpoint = (max(aux1$MontoPromedio)+min(aux1$MontoPromedio))/2)+
            theme(text = element_text(size=12), plot.title = element_text(hjust = 0.5), legend.text = element_text(size=11),
                  legend.position="bottom", legend.key.width = unit(6.5, "cm")) +
            scale_y_continuous(labels = comma)
        p1
    })
    
    #Frecuencia
    output$TablaFrecuencia <- renderTable({
        t1<-data_fil() %>% group_by(`Score`=F_Score) %>%
            summarise(`Mínimo`=comma(round(min(Frecuencia))),
                      `Máximo`=comma(round(max(Frecuencia))),
                      Pacientes=comma(round(n_distinct(prod_id))))
        
    }, spacing = 'xs')
    #Recencia
    output$TablaRecencia <- renderTable({
        t1<-data_fil() %>% group_by(`Score`=R_Score) %>%
            summarise(`Mínimo`=comma(round(min(Recencia))),
                      `Máximo`=comma(round(max(Recencia))),
                      Pacientes=comma(round(n_distinct(prod_id)))) %>% 
            arrange(desc(Score))
        
    }, spacing = 'xs')
    #Monto
    output$TablaMonto <- renderTable({
        t1<-data_fil() %>% group_by(`Score`=M_Score) %>%
            summarise(`Mínimo`=dollar(round(min(Monto))),
                      `Máximo`=dollar(round(max(Monto))),
                      Pacientes=comma(round(n_distinct(prod_id))))
    }, spacing = 'xs')

    
### Pestaña Segmentacion ----
    
    output$CompR <- renderPlot({
        
        aux1 <- data_fil() %>% 
            select(prod_id, Recencia:Monto, Tipologia, Cluster) %>% 
            distinct()
        
        aux1$Cluster <-  factor(aux1$Cluster, levels=c('E', 'D','C','B','A','AA'))
        
        ggplot(aux1, aes(x=Cluster, y=Recencia, fill=Cluster)) +
            geom_boxplot(outlier.shape = NA)+
            geom_hline(yintercept = median(aux1$Recencia), color="darkgreen", linetype="dashed")+
            coord_flip()+
            theme_minimal()+
            labs(title="")+
            scale_y_continuous(limits = quantile(aux1$Recencia, c(0.1, 0.95)), labels = comma)
    })
    output$CompF <- renderPlot({
        aux1 <- data_fil() %>% 
            select(prod_id, Recencia:Monto, Tipologia, Cluster) %>% 
            distinct()
        
        aux1$Cluster <-  factor(aux1$Cluster, levels=c('E', 'D','C','B','A','AA'))
        
        ggplot(aux1, aes(x=Cluster, y=Frecuencia, fill=Cluster)) +
            geom_boxplot(outlier.shape = NA)+
            geom_hline(yintercept = median(aux1$Frecuencia), color="darkgreen", linetype="dashed")+
            coord_flip()+
            theme_minimal()+
            labs(title="")+
            scale_y_continuous(limits = quantile(aux1$Frecuencia, c(0.1, 0.95)), labels = comma)
    })
    output$CompM <- renderPlot({
        aux1 <- data_fil() %>% 
            select(prod_id, Recencia:Monto, Tipologia, Cluster) %>% 
            distinct()
        
        aux1$Cluster <-  factor(aux1$Cluster, levels=c('E', 'D','C','B','A','AA'))
        
        ggplot(aux1, aes(x=Cluster, y=Monto, fill=Cluster)) +
            geom_boxplot(outlier.shape = NA)+
            geom_hline(yintercept = median(aux1$Monto), color="darkgreen", linetype="dashed")+
            coord_flip()+
            theme_minimal()+
            labs(title="")+
            scale_y_continuous(limits = quantile(aux1$Monto, c(0.1, 0.95)), labels = comma)
    })
    output$biplot <- renderPlot({
        aux0 <- data_fil() %>% 
            select(prod_id, Cluster) %>% 
            distinct()
        
        aux1 <- data_fil() %>% 
            select(prod_id, Recencia:Monto, Tipologia) %>% 
            distinct()
        
        aux2 <- as.data.frame(predict(dummyVars(~ . , data = aux1), newdata = aux1)) %>% 
            mutate_all(as.numeric) %>% 
            na.omit()
        
        pca <- prcomp(aux2[-1],  scale = T, center = T)
        
        fviz_pca_biplot(pca, geom = "point", habillage = aux0$Cluster ,addEllipses=F, ellipse.level=0.95, 
                        title = "", repel = F, legend.title = "Cluster") +
            labs(title ="", x = "PC1", y = "PC2")
        
    })
    
### Pestaña de Comparacion de Precios ----
    
    comparison_data_1 <- reactive({
        aux1 <- data %>% 
            filter(prod_id == input$po_input_product1,
                   Regional == input$po_input_region1) 
        
    })
    comparison_data_2 <- reactive({

        aux1 <- comparison_data_1() %>% 
            mutate(Fecha=cut(Fecha, "month")) %>% 
            group_by(prod_id, Regional, Fecha) %>% 
            summarise(BASE_PRICE=median(BASE_PRICE, na.rm = T))
        
        aux2 <- Precios %>% 
            filter(prod_id == input$po_input_product1) %>% 
            arrange(prod_id, desc(Mes)) %>% 
            group_by(prod_id) %>% 
            filter(row_number()==1) %>% 
            select(prod_id, starts_with("Precio")) 
        
        aux3 <- aux1 %>% left_join(aux2, by = "prod_id")

    })
    
    # Descripcion
    output$po_box_description_1 <- renderInfoBox({
        
        infoBox(title = "Producto",
                value = as.character(comparison_data_1()$prod_nombre)[1],
                color = "navy",
                icon = icon("pills")
        )
        
    })
    # Categoria
    output$po_box_category_1 <- renderInfoBox({
        
        infoBox(title = "Categoria",
                value = paste0(as.character(comparison_data_1()$categoria)[1]),
                color = "navy",
                icon = icon("prescription-bottle")
        )
        
    })
    # Patologia
    output$po_box_patology_1 <- renderInfoBox({
        
        infoBox(title = "Patologia",
                value = paste0(as.character(comparison_data_1()$Patologia)[1]),
                color = "navy",
                icon = icon("prescription-bottle-alt")
        )
        
    })
    # Tipologia
    output$po_box_tipology_1 <- renderInfoBox({
        
        infoBox(title = "Tipologia",
                value = paste0(as.character(comparison_data_1()$Tipologia)[1]),
                color = "navy",
                icon = icon("file-prescription")
        )
        
    })
    # Cluster
    output$po_box_cluster_1 <- renderInfoBox({
        
        infoBox(title = "Cluster",
                value = paste0(as.character(comparison_data_1()$Cluster)[1]),
                color = "navy",
                icon = icon("object-group")
        )
        
    })
    # RFM
    output$po_box_rfm_1 <- renderInfoBox({
        
        infoBox(title = "Puntajes RFM",
                value = paste0("Recencia: ",as.character(comparison_data_1()$R_Score)[1],
                               " Frecuencia: ", as.character(comparison_data_1()$F_Score)[1],
                               " Monto: ", as.character(comparison_data_1()$M_Score)[1]),
                color = "navy",
                icon = icon("layer-group")
        )
        
    })
    
    output$Comparacion_Precios <- renderPlotly({
        
        p <- plot_ly(comparison_data_2(), x = ~Fecha, y = ~BASE_PRICE, name = 'Precio Colsubsidio', type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~PrecioOlimpica, name = 'Olimpica', line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dot')) %>%
            add_trace(y = ~PrecioRebaja, name = 'La Rebaja', line = list(color = 'rgb(255,69,0)', width = 2, dash = 'dot')) %>% 
            add_trace(y = ~PrecioCafam, name = 'Cafam', line = list(color = 'rgb(70,130,180)', width = 2, dash = 'dot')) %>%
            add_trace(y = ~PrecioCruzVerde, name = 'Cruz Verde', line = list(color = 'rgb(46,139,87)', width = 2, dash = 'dot')) %>% 
            add_trace(y = ~PrecioFarmatodo, name = 'Farmatodo', line = list(color = 'rgb(30,144,255)', width = 2, dash = 'dot')) %>%
            add_trace(y = ~PrecioAcuna, name = 'Acuna', line = list(color = 'rgb(218,165,32)', width = 2, dash = 'dot')) %>% 
            add_trace(y = ~PrecioAlemana, name = 'Alemana', line = list(color = 'rgb(0,0,0)', width = 2, dash = 'dot')) %>%
            add_trace(y = ~PrecioPasteur, name = 'Pasteur', line = list(color = 'rgb(112,128,144)', width = 2, dash = 'dot')) %>% 
            add_trace(y = ~PrecioJunin, name = 'Junin', line = list(color = 'rgb(216,191,216)', width = 2, dash = 'dot')) %>%
            add_trace(y = ~PrecioInglesa, name = 'Inglesa', line = list(color = 'rgb(147,112,219)', width = 2, dash = 'dot')) %>% 
            add_trace(y = ~PrecioFarmavida, name = 'Farmavida', line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dot')) %>%
            add_trace(y = ~PrecioBotica, name = 'La Botica', line = list(color = 'rgb(255,127,80)', width = 2, dash = 'dot')) %>% 
            add_trace(y = ~PrecioMultidrogas, name = 'Multidrogas', line = list(color = 'rgb(173,255,47)', width = 2, dash = 'dot')) %>%
            add_trace(y = ~PrecioComfandi, name = 'Comfandi', line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dot')) %>% 
            add_trace(y = ~PrecioLa14, name = 'La 14', line = list(color = 'rgb(47,79,79)', width = 2, dash = 'dot')) %>% 
            add_trace(y = ~PrecioComfenalco, name = 'Comfenalco', line = list(color = 'rgb(65,105,225)', width = 2, dash = 'dot')) %>% 
            add_trace(y = ~PrecioSanJorge, name = 'San Jorge', line = list(color = 'rgb(245,222,179)', width = 2, dash = 'dot')) %>% 
            add_trace(y = ~PrecioMultifamiliar, name = 'Multifamiliar', line = list(color = 'rgb(255,20,147)', width = 2, dash = 'dot')) %>% 
            layout(yaxis = list(rangemode = "tozero", title = "Precio Unitario"), 
                   xaxis = list(title = ""), 
                   legend = list(orientation = 'h'))
        p  
        
    })

    
    
### Pestaña optimizacion ----
    ### Bases de Datos Reactivas ----
    
    product_data <- reactive({
        
        data %>% filter(prod_id == input$po_input_product,
                        Regional == input$po_input_region) %>% 
            arrange(WEEK_NO)
        
    })
    
    visualize_data <- reactive({
        
      product_data() %>% select(Week=Fecha, Price=PRICE, Demand=UNITS)
        
    })
    
    ### Funciones de Calculo ----
    optimums <- reactive({
        
        withProgress(message = "", value = 0, style="old", {
            
            ts_prediction <- predict_units_prophet(product_data())$units
            ts_units_category <- units_to_factor(x = ts_prediction, data = product_data())
            
        })
        
        return(list(optimums = optimize_one_product_lmer(data = product_data(), factor_spend = ts_units_category),
                    ts_units_category = ts_units_category,
                    predicted_level = units_to_factor(x = ts_prediction, data = product_data())))
        
    })
    predicted_demand <- reactive({
        
        withProgress(message = "", value = 0, style="old", {
            
            req(!is.null(input$po_input_price))
            
            df <- data.frame(BASE_PRICE = input$po_input_price,
                             FEATURE = input$po_input_letter,
                             DISPLAY = F,
                             units_factor = as.character(optimums()$ts_units_category),
                             PCT_CAT=0.3,
                             PCT_PAT=0.3,
                             PCT_TIP=0.3)
            
            predict(optimums()$optimums$model, df)
            
        })
        
    })
    
    ### Actualizacion de Datos del UI ----
    output$po_render_price <- renderUI({
        
        sliderInput(inputId = "po_input_price",
                    label = "Precio",
                    min = round((min(product_data()$COST)),0), # Costo del Producto
                    max = round((max(product_data()$BASE_PRICE) * 1.6),0), # Doble del precio maximo historico
                    value = round((round((max(product_data()$BASE_PRICE) * 1.6),0)-min(product_data()$COST))/2,0),
                    step =50)
        
    })
    
    ### Indicadores y Resumen ----

    # Descripcion
    output$po_box_description <- renderInfoBox({
        
        infoBox(title = "Producto",
                value = as.character(product_data()$prod_nombre)[1],
                color = "navy",
                icon = icon("pills")
        )
        
    })
    # Categoria
    output$po_box_category <- renderInfoBox({
        
        infoBox(title = "Categoria",
                value = paste0(as.character(product_data()$categoria)[1]),
                color = "navy",
                icon = icon("prescription-bottle")
        )
        
    })
    # Patologia
    output$po_box_patology <- renderInfoBox({
        
        infoBox(title = "Patologia",
                value = paste0(as.character(product_data()$Patologia)[1]),
                color = "navy",
                icon = icon("prescription-bottle-alt")
        )
        
    })
    # Tipologia
    output$po_box_tipology <- renderInfoBox({
        
        infoBox(title = "Tipologia",
                value = paste0(as.character(product_data()$Tipologia)[1]),
                color = "navy",
                icon = icon("file-prescription")
        )
        
    })
    # Cluster
    output$po_box_cluster <- renderInfoBox({
        
        infoBox(title = "Cluster",
                value = paste0(as.character(product_data()$Cluster)[1]),
                color = "navy",
                icon = icon("object-group")
        )
        
    })
    # RFM
    output$po_box_rfm <- renderInfoBox({
        
        infoBox(title = "Puntajes RFM",
                value = paste0("Recencia: ",as.character(product_data()$R_Score)[1],
                               " - Frecuencia: ", as.character(product_data()$F_Score)[1],
                               " - Monto: ", as.character(product_data()$M_Score)[1]),
                color = "navy",
                icon = icon("layer-group")
        )
        
    })
    
    # Demanda predicha
    output$po_box_predicted_demand <- renderValueBox({
        
        valueBox(round(predicted_demand() %>% max_with_zero(), 0),
                 "Demanda esperada siguiente periodo",
                 color = "light-blue",
                 icon = icon("line-chart")
        )
        
    })
    # Nivel de demanda predicho
    output$po_box_predicted_demand_level <- renderInfoBox({
        
        cat <- optimums()$predicted_level
        
        infoBox(title = "Nivel de demanda esperado, siguiente periodo",
                value = ifelse(cat == 1, "Baja", ifelse(cat == 2, "Media", "Alta")),
                color = ifelse(cat == 1, "light-blue", ifelse(cat == 2, "blue", "navy")),
                fill = TRUE,
                icon = icon("bar-chart-o")
        )
        
    })
    # Elasticidad
    output$po_box_elasticity <- renderInfoBox({
        
        b <- summary(optimums()$optimums$model)$coefficients
        b <- abs(b[which(rownames(b) == "BASE_PRICE"), 1])
        
        infoBox(title = "Demanda del producto",
                value = ifelse(b < 1, "Inelastica", "Elastica"),
                color = "purple",
                fill = TRUE,
                icon = icon("bar-chart-o")
        )
        
    })
    
    # Precio Actual
    output$po_box_current_price <- renderValueBox({
        
        valueBox(dollar(input$po_input_price), 
                 "Precio Dinamico",
                 color = "light-blue",
                 icon = icon("dollar")
        )
        
    })
    # Venta Actual
    output$po_box_current_profit <- renderValueBox({
        
        valueBox(dollar(round(input$po_input_price * max_with_zero(predicted_demand()), 0)),
                 "Venta Esperada con el precio actual",
                 color = "light-blue",
                 icon = icon("money")
        )
        
    })
    # Precio Unitario del Producto
    output$po_box_current_cost <- renderValueBox({
        
        valueBox(dollar(product_data()$COST[1]),
                 "Precio Unitario Real",
                 color = "light-blue",
                 icon = icon("percentage")
        )
        
    })
    # Costo de Venta Actual
    output$po_box_current_cost_ttl <- renderValueBox({
        
        valueBox(dollar(round(product_data()$COST[1] * max_with_zero(predicted_demand()),0)),
                 "Costo de venta",
                 color = "light-blue",
                 icon = icon("percentage")
        )
        
    })
    # Utilidad PCT Actual
    output$po_box_current_profit_pct <- renderValueBox({
        
        Venta=input$po_input_price * max_with_zero(predicted_demand())
        Costo=product_data()$COST[1] * max_with_zero(predicted_demand())
        Utilidad = 1-(Costo/Venta)
        
        valueBox(percent(Utilidad),
                 "Utilidad Esperada con el precio actual",
                 color = "light-blue",
                 icon = icon("percentage")
        )
    })
    
    # Precio optimo
    output$po_box_optimum_price <- renderValueBox({
        
        valueBox(dollar(round(optimums()$optimums$opt$maximum, 0)),
                 "Precio Optimo",
                 color = "olive",
                 icon = icon("dollar")
        )
        
    })
    # Demana optima
    output$po_box_optimum_demand <- renderValueBox({
        
        valueBox(round(optimums()$optimums$opt$opt_units, 0),
                 "Unidades esperadas a precio optimo",
                 color = "olive",
                 icon = icon("line-chart")
        )
        
    })
    # Venta Optima
    output$po_box_optimum_profit <- renderValueBox({
        
        valueBox(dollar(round(optimums()$optimums$opt$objective, 0)),
                 "Ventas esperadas a precio optimo",
                 color = "olive",
                 icon = icon("money")
        )
        
    })
    # Costo de Venta Optimo
    output$po_box_optimum_cost_ttl <- renderValueBox({
        
        valueBox(dollar(round(product_data()$COST[1] * optimums()$optimums$opt$opt_units,0)),
                 "Costo de venta con el precio optimo",
                 color = "olive",
                 icon = icon("money")
        )
        
    })
    # Utilidad Optima
    output$po_box_optimum_profit_pct <- renderValueBox({
        
        Venta=optimums()$optimums$opt$maximum * optimums()$optimums$opt$opt_units
        Costo=product_data()$COST[1] * optimums()$optimums$opt$opt_units
        Utilidad = 1-(Costo/Venta)
        
        valueBox(percent(Utilidad),
                 "Utilidad Esperada con el precio optimo",
                 color = "olive",
                 icon = icon("percentage")
        )
        
    })
    
    ### Historicos ----
    # Precio Historico
    output$price_history <- renderDygraph({
        
        dygraph(as.xts(visualize_data()$Price, order.by = visualize_data()$Week),
                ylab = "Precio",
                group = "Historia") %>%
            dyOptions(colors = "black")
        
    })
    
    # Demanda Historico
    output$demand_history <- renderDygraph({
        
        dygraph(as.xts(visualize_data()$Demand, order.by = visualize_data()$Week),
                ylab = "Unidades",
                group = "Historia") %>%
            dyOptions(colors = "black")
        
    })
    
    # Niveles de Demanda
    output$demand_levels <- renderDygraph({
        
        periods_steps <- data.table(Demand = visualize_data()$Demand,
                                    Week = visualize_data()$Week,
                                    Demand_level = units_to_factor(x = product_data()$UNITS, data = product_data()))
        
        dygraph(as.xts(periods_steps$Demand_level, order.by = periods_steps$Week),
                ylab = "Nivel de Demanda",
                group = "Historia") %>%
            dyOptions(stepPlot = TRUE, fillGraph = TRUE)
        
    })
    
    ### Resultados del modelo ----
    # Ventas Precio
    output$po_profit_plot <- renderPlotly({
        
        optimums()$optimums$profit_plot
        
    })
    
    # Demanda - Precio
    output$po_demand_vs_price_plot <- renderPlotly({
        
        optimums()$optimums$plot
        
    })
    
    # Prueba de Tukey
    
    output$po_tukey <- renderPlotly({
      aux <- product_data() %>%
        mutate(Promocion=as.factor(ifelse(FEATURE==1, "Con Promocion", "Sin Promocion"))) %>% 
        select(UNITS, Promocion)
      
      aux %>% 
        plot_ly(y=~Promocion, x=~UNITS, color = ~Promocion, type = 'box') %>% 
        layout(title = paste(""),
               xaxis = list(title="Unidades", fixedrange=TRUE, tickformat = ","),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F)
    })
    
    output$tukeyDif <- renderInfoBox({
      
      aux <- product_data() %>%
        mutate(Promocion=as.factor(ifelse(FEATURE==1, "Con Promocion", "Sin Promocion"))) %>% 
        select(UNITS, Promocion)
      
      m1 <- lm(UNITS~Promocion, data = aux)
      hsd <- HSD.test(m1, "Promocion")
      Dif=ifelse(n_distinct(hsd$groups$groups)==2,"Diferente", "Indiferente")
      
      infoBox(title = "Diferencia de Unidades Significativa Respecto a la Promocion",
              value = Dif,
              color = "teal",
              icon = icon("th-large")
      )
    })
    
    output$tukeyDifUnidades <- renderInfoBox({
      
      aux <- product_data() %>%
        mutate(Promocion=as.factor(ifelse(FEATURE==1, "Con Promocion", "Sin Promocion"))) %>% 
        select(UNITS, Promocion)
      
      m1 <- lm(UNITS~Promocion, data = aux)
      hsd <- HSD.test(m1, "Promocion")
      Cant=abs(hsd$means[2,1]-hsd$means[1,1])
      
      infoBox(title = "Diferencia absoluta de Unidades Promedio Respecto a la Promocion",
              value = comma(round(Cant, 2)),
              color = "teal",
              icon = icon("receipt")
      )
    })
    
    ### Reiniciar checkboxes ------
    observeEvent(input$po_input_product, {

      updateCheckboxInput(session, "po_input_display", value = FALSE)
      updateCheckboxInput(session, "po_input_letter", value = FALSE)

      if (optimums()$optimums$vars_predictors[1]) {
        shinyjs::enable("po_input_display")
      } else {
        shinyjs::disable("po_input_display")
      }

      if (optimums()$optimums$vars_predictors[2]) {
        shinyjs::enable("po_input_letter")
      } else {
        shinyjs::disable("po_input_letter")
      }


    })
  
    
### Pestaña de Resultados ----
    
    output$BarrasRegional <- renderPlotly({
      aux <- data_res() %>% 
        mutate(Elasticidad=if_else(is.na(Elasticidad), "Sin Determinar",Elasticidad)) %>% 
        group_by(Regional, Elasticidad) %>% 
        summarise(Freq=n())
      
      aux$Regional <-  factor(aux$Regional, levels=sort(unique(aux$Regional), decreasing = T))
      
      p <- aux %>%
        plot_ly(source="BarrasRegional") %>%
        add_trace(y=~Regional, x=~Freq, type="bar", color = ~Elasticidad, colors=c("firebrick","steelblue","palegreen4"), 
                  text = ~paste("Regional:", Regional, "\n", 
                                "Elasticidad:", Elasticidad, "\n",
                                "Productos", comma(Freq)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "," ),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
      })
    output$BarrasCategoria <- renderPlotly({
      aux <- data_res() %>% 
        mutate(Elasticidad=if_else(is.na(Elasticidad), "Sin Determinar",Elasticidad)) %>% 
        group_by(categoria, Elasticidad) %>% 
        summarise(Freq=n())
      
      aux$categoria <-  factor(aux$categoria, levels=sort(unique(aux$categoria), decreasing = T))
      
      p <- aux %>%
        plot_ly(source="BarrasCategoria") %>%
        add_trace(y=~categoria, x=~Freq, type="bar", color = ~Elasticidad, colors=c("firebrick","steelblue","palegreen4"),
                  text = ~paste("Categoria:", categoria, "\n", 
                                "Elasticidad:", Elasticidad, "\n",
                                "Productos", comma(Freq)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "," ),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })
    output$BarrasTipologia <- renderPlotly({
      aux <- data_res() %>% 
        mutate(Elasticidad=if_else(is.na(Elasticidad), "Sin Determinar",Elasticidad)) %>% 
        group_by(Tipologia, Elasticidad) %>% 
        summarise(Freq=n())
      
      aux$Tipologia <-  factor(aux$Tipologia, levels=sort(unique(aux$Tipologia), decreasing = T))
      
      p <- aux %>%
        plot_ly(source="BarrasTipologia") %>%
        add_trace(y=~Tipologia, x=~Freq, type="bar", color = ~Elasticidad, colors=c("firebrick","steelblue","palegreen4"),
                  text = ~paste("Tipologia:", Tipologia, "\n", 
                                "Elasticidad:", Elasticidad, "\n",
                                "Productos", comma(Freq)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "," ),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })
    output$BarrasCluster <- renderPlotly({
      aux <- data_res() %>% 
        mutate(Elasticidad=if_else(is.na(Elasticidad), "Sin Determinar",Elasticidad)) %>% 
        group_by(Cluster, Elasticidad) %>% 
        summarise(Freq=n())
      
      aux$Cluster <-  factor(aux$Cluster, levels=c('E', 'D', 'C','B', 'A','AA'))
      
      p <- aux %>%
        plot_ly(source="BarrasCluster") %>%
        add_trace(y=~Cluster, x=~Freq, type="bar", color = ~Elasticidad, colors=c("firebrick","steelblue","palegreen4"),
                  text = ~paste("Cluster:", Cluster, "\n", 
                                "Elasticidad:", Elasticidad, "\n",
                                "Productos", comma(Freq)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "," ),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })

    output$DetalleResultados <- renderDataTable({
      
      r_  <- event_data("plotly_click", source = "BarrasRegional")
      ca_ <- event_data("plotly_click", source = "BarrasCategoria")
      t_  <- event_data("plotly_click", source = "BarrasTipologia")
      cl_ <- event_data("plotly_click", source = "BarrasCluster")
      
      re=ifelse(!is.null(r_)  , T, F)
      ca=ifelse(!is.null(ca_) , T, F)
      ti=ifelse(!is.null(t_)  , T, F)
      cl=ifelse(!is.null(cl_) , T, F)
      
      aux1 <- data_res() %>% 
        filter(if (re) Regional  ==as.character(r_[4]) else T,
               if (ca) categoria ==as.character(ca_[4]) else T,
               if (ti) Tipologia ==as.character(t_[4]) else T,
               if (cl) Cluster   ==as.character(cl_[4]) else T
               ) %>% 
        select(1, 2, 9:15, 18:20)
      
      datatable(aux1, options = list(pageLength = 20, dom="t", scrollX = TRUE, scrollY = "700px"), rownames = F, escape = F, 
                colnames = c("Codigo Producto","Nombre Producto","Categoria","Patologia","Tipologia","Cluster","Regional","Eslasticidad","Betta","Precio Optimo",
                             "Diferencia (Tukey)", "Diferencia de Cantidades (Promedio)")) %>% 
        formatCurrency(c("PrecioOptimo"), digits = 0) %>% 
        formatRound("Betta", digits = 4) %>% 
        formatRound("Dif_Cant", digits = 0)
    })
    
    output$DescargaResultados <- downloadHandler(
      filename = function() {
        paste0("Resultados", Sys.Date(), ".csv")
      },
      content = function(file) {
        
        r_  <- event_data("plotly_click", source = "BarrasRegional")
        ca_ <- event_data("plotly_click", source = "BarrasCategoria")
        t_  <- event_data("plotly_click", source = "BarrasTipologia")
        cl_ <- event_data("plotly_click", source = "BarrasCluster")
        
        re=ifelse(!is.null(r_)  , T, F)
        ca=ifelse(!is.null(ca_) , T, F)
        ti=ifelse(!is.null(t_)  , T, F)
        cl=ifelse(!is.null(cl_) , T, F)
        
        aux1 <- data_res() %>% 
          filter(if (re) Regional  ==as.character(r_[4]) else T,
                 if (ca) categoria ==as.character(ca_[4]) else T,
                 if (ti) Tipologia ==as.character(t_[4]) else T,
                 if (cl) Cluster   ==as.character(cl_[4]) else T
          ) %>% 
          select(1, 2, 9:15, 18:20)
        
        write.csv2(aux1,file, row.names = FALSE)
      }
    )
    
    observeEvent(input$reset, js$resetClick())
    
### Pestaña de Modificacion Masiva ----
    # Tab de Cargue ----
    data_input <- reactive({
      req(input$archivo)
      fread(input$archivo$datapath, col.names = c('prod_id', 'Regional', 'PrecioNuevo'), colClasses = c('numeric', 'character', 'numeric'))
    })
    masivos <- reactive ({
      Resultados %>% inner_join(data_input(),  c('prod_id', 'Regional'))
    })
    nocoinc <- reactive({
      data_input() %>% anti_join(Resultados, by=c('prod_id', 'Regional'))
    })
    NuevosPrecios <- reactive ({
      Resultados %>% left_join(data_input(),  c('prod_id', 'Regional'))
    })
    
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
        datatable(aux, options=list(pageLength =20, dom = 'tl', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=T)
      }
      else {aux <- data.frame(Resultados="Todas las identificaciones son coincidentes con el modelo")}
      
      datatable(aux, options=list(pageLength =20, dom = 'tl', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=T)
      
    })
    output$Coincidentes <- renderDataTable({
      
      aux <- masivos()
      
      datatable(aux, options=list(pageLength =20, dom = 'tl', searching= T, scrollX = TRUE, scrollY = "400px"), rownames=F)
      
    })
    # Tab de Resultados ----
    data_modif_2 <- reactive({
      
      aux <- data %>% select(prod_id, Regional, COST) %>% distinct()
      
      data_modif <- NuevosPrecios() %>% 
        left_join(aux, c("prod_id", "Regional")) %>% 
        select(prod_id, Regional, categoria, Patologia, Tipologia, Cluster, COST, PrecioOptimo, PrecioNuevo) %>% 
        mutate(PrecioFinal=ifelse(is.na(PrecioNuevo), PrecioOptimo, PrecioNuevo),
               Margen=(1-(COST/PrecioFinal))
        )
      
      return(data_modif)
    })
    
    output$po_margen_total_1 <- renderInfoBox({

      aux <- data_modif_2() %>% summarise(Margen=mean(Margen, na.rm = T)) %>% as.numeric()

      infoBox(title = "Margen de Utilidad Total",
              value = percent(aux),
              color = "navy",
              icon = icon("coins")
      )

    })

    output$BarrasRegional_1_1 <- renderPlotly({
      aux <- data_modif_2() %>%
        group_by(Regional) %>%
        summarise(Margen=median(Margen, na.rm = T))

      aux$Regional <-  factor(aux$Regional, levels=sort(unique(aux$Regional), decreasing = T))

      p <- aux %>%
        plot_ly() %>%
        add_trace(y=~Regional, x=~Margen, type="bar",
                  text = ~paste("Regional:", Regional, "\n",
                                "Margen", percent(Margen)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "%"),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })
    output$BarrasTipologia_1_1 <- renderPlotly({
      aux <- data_modif_2() %>%
        group_by(Tipologia) %>%
        summarise(Margen=median(Margen, na.rm = T))

      aux$Tipologia <-  factor(aux$Tipologia, levels=c('A','B','C','D','N'))

      p <- aux %>%
        plot_ly() %>%
        add_trace(y=~Tipologia, x=~Margen, type="bar",
                  text = ~paste("Tipologia:", Tipologia, "\n",
                                "Margen", percent(Margen)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "%"),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })
    output$BarrasCluster_1_1 <- renderPlotly({
      aux <- data_modif_2() %>%
        group_by(Cluster) %>%
        summarise(Margen=median(Margen, na.rm = T))

      aux$Cluster <-  factor(aux$Cluster, levels=c('AA','A','B','C','D','E'))

      p <- aux %>%
        plot_ly() %>%
        add_trace(y=~Cluster, x=~Margen, type="bar",
                  text = ~paste("Cluster:", Cluster, "\n",
                                "Margen", percent(Margen)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "%"),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })
    output$BarrasCategoria_1_1 <- renderPlotly({
      aux <- data_modif_2() %>%
        group_by(categoria) %>%
        summarise(Margen=median(Margen, na.rm = T))

      aux$categoria <-  factor(aux$categoria, levels=sort(unique(aux$categoria), decreasing = T))

      p <- aux %>%
        plot_ly() %>%
        add_trace(y=~categoria, x=~Margen, type="bar",
                  text = ~paste("Categoria:", categoria, "\n",
                                "Margen", percent(Margen)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "%"),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })
    output$BarrasPatologia_1_1 <- renderPlotly({
      aux <- data_modif_2() %>%
        group_by(Patologia) %>%
        summarise(Margen=median(Margen, na.rm = T))

      aux$Patologia <-  factor(aux$Patologia, levels=sort(unique(aux$Patologia), decreasing = T))

      p <- aux %>%
        plot_ly() %>%
        add_trace(y=~Patologia, x=~Margen, type="bar",
                  text = ~paste("Patologia:", Patologia, "\n",
                                "Margen", percent(Margen)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "%"),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })
### Modificacion de Precios -----
    
    data_tabla <- reactive({
      
      Resultados %>% 
        filter(categoria == input$ta_categoria,
               Cluster == input$ta_cluster,
               Dif == input$ta_diferencia) %>% 
        select(prod_id, prod_nombre, categoria, Patologia, Cluster, Regional, Elasticidad, Betta, Dif, PrecioOptimo)

    })
    output$Precios <- renderRHandsontable({
      
      aux <- data_tabla()
      
      rhandsontable(aux, rowHeaders=NULL, colHeaders = c("PLU","Producto","Regional","Categoria","Patologia","Cluster","Elasticidad","Betta","Diferencia",
                                                         "Precio"), readOnly = T, search = TRUE, width = 1600) %>% 
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>% 
        hot_col("Precio", format = "0.00 $", readOnly = F) %>% 
        hot_col("Betta", format = "0.000000", readOnly = T) %>% 
        hot_col("PLU", format = "0", readOnly = T)
    })
    
    observeEvent(input$runButton, {
      values$data <-  hot_to_r(input$Precios)
    })
    data_modif <- reactive({
      
      aux <- data %>% select(prod_id, Regional, COST) %>% distinct()
      
      data_modif <- Resultados %>% 
        left_join(as.data.frame(values$data) %>% select(prod_id, Regional, PrecioNuevo=PrecioOptimo), by=c("prod_id", "Regional")) %>%  
        left_join(aux, c("prod_id", "Regional")) %>% 
        select(prod_id, Regional, categoria, Patologia, Tipologia, Cluster, COST, PrecioOptimo, PrecioNuevo) %>% 
        mutate(PrecioFinal=ifelse(is.na(PrecioNuevo), PrecioOptimo, PrecioNuevo),
               Margen=(1-(COST/PrecioFinal))
        )
      
      return(data_modif)
    })
    
    output$po_margen_total <- renderInfoBox({
      
      aux <- data_modif() %>% summarise(Margen=mean(Margen, na.rm = T)) %>% as.numeric()
      
      infoBox(title = "Margen de Utilidad Total",
              value = percent(aux),
              color = "navy",
              icon = icon("coins")
      )
      
    })
    
    output$BarrasRegional_1 <- renderPlotly({
      aux <- data_modif() %>% 
        group_by(Regional) %>% 
        summarise(Margen=median(Margen, na.rm = T))
      
      aux$Regional <-  factor(aux$Regional, levels=sort(unique(aux$Regional), decreasing = T))
      
      p <- aux %>%
        plot_ly() %>%
        add_trace(y=~Regional, x=~Margen, type="bar", 
                  text = ~paste("Regional:", Regional, "\n", 
                                "Margen", percent(Margen)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "%"),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })
    output$BarrasTipologia_1 <- renderPlotly({
      aux <- data_modif() %>% 
        group_by(Tipologia) %>% 
        summarise(Margen=median(Margen, na.rm = T))
      
      aux$Tipologia <-  factor(aux$Tipologia, levels=c('A','B','C','D','N'))

      p <- aux %>%
        plot_ly() %>% 
        add_trace(y=~Tipologia, x=~Margen, type="bar", 
                  text = ~paste("Tipologia:", Tipologia, "\n", 
                                "Margen", percent(Margen)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "%"),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })
    output$BarrasCluster_1 <- renderPlotly({
      aux <- data_modif() %>% 
        group_by(Cluster) %>% 
        summarise(Margen=median(Margen, na.rm = T))
      
      aux$Cluster <-  factor(aux$Cluster, levels=c('AA','A','B','C','D','E'))
      
      p <- aux %>%
        plot_ly() %>%
        add_trace(y=~Cluster, x=~Margen, type="bar", 
                text = ~paste("Cluster:", Cluster, "\n", 
                              "Margen", percent(Margen)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "%"),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })
    output$BarrasCategoria_1 <- renderPlotly({
      aux <- data_modif() %>% 
        group_by(categoria) %>% 
        summarise(Margen=median(Margen, na.rm = T))
      
      aux$categoria <-  factor(aux$categoria, levels=sort(unique(aux$categoria), decreasing = T))
      
      p <- aux %>%
        plot_ly() %>%
        add_trace(y=~categoria, x=~Margen, type="bar", 
                  text = ~paste("Categoria:", categoria, "\n", 
                                "Margen", percent(Margen)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "%"),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })
    output$BarrasPatologia_1 <- renderPlotly({
      aux <- data_modif() %>% 
        group_by(Patologia) %>% 
        summarise(Margen=median(Margen, na.rm = T))
      
      aux$Patologia <-  factor(aux$Patologia, levels=sort(unique(aux$Patologia), decreasing = T))
      
      p <- aux %>%
        plot_ly() %>%
        add_trace(y=~Patologia, x=~Margen, type="bar", 
                  text = ~paste("Patologia:", Patologia, "\n", 
                                "Margen", percent(Margen)), hoverinfo="text") %>%
        layout(title = paste(""),
               xaxis = list(title="", fixedrange=TRUE, tickformat = "%"),
               yaxis = list(title = "", zeroline = T, showline = T, showticklabels = T, showgrid = T)) %>%
        config(displayModeBar = F);p
    })

}