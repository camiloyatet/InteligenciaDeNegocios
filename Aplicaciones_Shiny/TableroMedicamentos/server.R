library(shiny)

shinyServer(function(input, output, session) {
  
  # data_departamento <- eventReactive(input$go,{
  #   data_f1 <- agrupados_departamento %>% 
  #     dplyr::filter(if (input$xeps != "Todos") NOMBRE_EPS %in% input$xeps else TRUE)
  #   return(data_f1)
  # })
  # 
  # data_vulnerable_no <- eventReactive(input$go,{
  #   data_f1 <- agrupados_vulnerable_no %>% 
  #     dplyr::filter(if (input$xeps != "Todos") NOMBRE_EPS %in% input$xeps else TRUE)
  #   return(data_f1)
  # })
  # 
  # data_vulnerable_si <- eventReactive(input$go,{
  #   data_f1 <- agrupados_vulnerable_si %>% 
  #     dplyr::filter(if (input$xeps != "Todos") NOMBRE_EPS %in% input$xeps else TRUE)
  #   return(data_f1)
  # })
  # 
  # data_atentidos_si_vulnerables_si <- eventReactive(input$go,{
  #   data_f1 <- agrupados_atentidos_si_vulnerables_si %>% 
  #     dplyr::filter(if (input$xeps != "Todos") NOMBRE_EPS %in% input$xeps else TRUE)
  #   return(data_f1)
  # })
  # 
  # data_atentidos_si_vulnerables_no <- eventReactive(input$go,{
  #   data_f1 <- agrupados_atentidos_si_vulnerables_no %>% 
  #     dplyr::filter(if (input$xeps != "Todos") NOMBRE_EPS %in% input$xeps else TRUE)
  #   return(data_f1)
  # })
  # 
  # data_plot_reactive <- eventReactive(input$go,{
  #   data_f1 <- data_plot %>% 
  #     dplyr::filter(if (input$xeps != "Todos") NOMBRE_EPS %in% input$xeps else TRUE) %>% 
  #     group_by(Fecha.doc.) %>% 
  #     summarise(consumo = sum(consumo, na.rm = T))
  #   return(data_f1)
  # })
  # 
  # ### Indicadores ====
  # output$info_transacciones <- renderValueBox({
  #   previo <- data_departamento()
  #   aux <- agrupados_totales %>% 
  #     filter(Tipo == "Total general transacciones") %>% 
  #     mutate(Total = rowSums(.[,c(2:ncol(agrupados_totales))], na.rm = T))
  #   valueBox(
  #     value = formatC(aux$Total,digits = 0, format = "d", big.mark=","),
  #     subtitle = "Total Transacciones",
  #     icon = icon("check"),
  #     color = "blue"
  #   )
  # })
  # 
  # output$info_vulnerables_si <- renderValueBox({
  #   aux <- data_vulnerable_si() %>% 
  #     mutate(Total = rowSums(.[,c(2:ncol(data_vulnerable_si()))], na.rm = T))
  #   valueBox(
  #     value = formatC(sum(aux$Total, na.rm = T),digits = 0, format = "d", big.mark=","),
  #     subtitle = "Transacciones Vulnerables",
  #     icon = icon("check"),
  #     color = "blue"
  #   )
  # })
  # 
  # output$info_vulnerables_no <- renderValueBox({
  #   aux <- data_vulnerable_no() %>%
  #     mutate(Total = rowSums(.[,c(2:ncol(data_vulnerable_no()))], na.rm = T))
  #   valueBox(
  #     value = formatC(sum(aux$Total, na.rm = T),digits = 0, format = "d", big.mark=","),
  #     subtitle = "Transacciones no Vulnerables",
  #     icon = icon("check"),
  #     color = "blue"
  #   )
  # })
  # 
  # output$info_atendidos <- renderValueBox({
  #   aux1 <- data_atentidos_si_vulnerables_si() %>% 
  #     mutate(Total = rowSums(.[,c(2:ncol(data_atentidos_si_vulnerables_si()))], na.rm = T))
  #   aux2 <- data_atentidos_si_vulnerables_no() %>% 
  #     mutate(Total = rowSums(.[,c(2:ncol(data_atentidos_si_vulnerables_no()))], na.rm = T))
  #   aux <- sum(aux1$Total, na.rm = T) + sum(aux2$Total, na.rm = T)
  #   valueBox(
  #     value = formatC(aux,digits = 0, format = "d", big.mark=","),
  #     subtitle = "Total eventos atendidos",
  #     icon = icon("check"),
  #     color = "blue"
  #   )
  # })
  # 
  # output$info_atendidos_si_vulnerables_si <- renderValueBox({
  #   aux <- data_atentidos_si_vulnerables_si() %>% 
  #     mutate(Total = rowSums(.[,c(2:ncol(data_atentidos_si_vulnerables_si()))], na.rm = T))
  #   valueBox(
  #     value = formatC(sum(aux$Total, na.rm = T),digits = 0, format = "d", big.mark=","),
  #     subtitle = "Atendidos Vulnerables",
  #     icon = icon("check"),
  #     color = "blue"
  #   )
  # })
  # 
  # output$info_atendidos_si_vulnerables_no <- renderValueBox({
  #   aux <- data_atentidos_si_vulnerables_no() %>% 
  #     mutate(Total = rowSums(.[,c(2:ncol(data_atentidos_si_vulnerables_no()))], na.rm = T))
  #   valueBox(
  #     value = formatC(sum(aux$Total, na.rm = T),digits = 0, format = "d", big.mark=","),
  #     subtitle = "Atendidos no Vulnerables",
  #     icon = icon("check"),
  #     color = "blue"
  #   )
  # })
  # 
  # ### Totales ====
  # output$plot_volumen_pedidos <- renderPlotly({
  # 
  #   m <- list(l = 50,r = 50,b = 0,t = 0, pad = 0)
  #   colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
  #   f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
  #   f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
  # 
  #   p1 <- plot_ly(data_plot_reactive(), x = ~Fecha.doc., y = ~consumo, type = 'scatter', 
  #                 mode = 'lines+markes', text = ~comma(consumo), textposition = 'outside') %>%
  #     layout(margin = m,
  #            title = 'Solicitudes por día',
  #            font = list(color = 'lightgrey'),
  #            xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            legend = list(x = 0.8, y = 0.5),
  #            paper_bgcolor='transparent',
  #            plot_bgcolor='transparent') %>%
  #     config(displayModeBar = F)
  #   p1
  # })
  # 
  # output$plot_tr_departamento <- renderPlotly({
  #   previo <- data_departamento()
  #   grap1 <- agrupados_totales %>%
  #     filter(Tipo == "Total general transacciones") %>% 
  #     gather("DEPARTAMENTO","Transacciones", 2:ncol(agrupados_totales)) %>% 
  #     arrange(desc(Transacciones)) %>%
  #     filter(row_number() <= 10)
  # 
  #   m <- list(l = 50,r = 50,b = 0,t = 0, pad = 0)
  #   colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
  #   f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
  #   f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
  # 
  #   grap1 %>%
  #     plot_ly(x = ~Transacciones, y = ~reorder(DEPARTAMENTO,Transacciones), type = 'bar', orientation = 'h',
  #             text = ~comma(Transacciones), textposition = 'auto', hoverinfo = 'text') %>%
  #     layout(margin = m,
  #            title = "Top 10 Departamentos",
  #            xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            paper_bgcolor='transparent',
  #            plot_bgcolor='transparent') %>%
  #     config(displayModeBar = F)
  # })
  # 
  # ### Vulnerables ====
  # output$plot_tr_vulnerables_si <- renderPlotly({
  #   grap1 <- data_vulnerable_si() %>%
  #     mutate(Total = rowSums(.[,c(2:ncol(data_vulnerable_si()))], na.rm = T)) %>% 
  #     arrange(desc(Total)) %>%
  #     select(NOMBRE_EPS,Total) %>% 
  #     filter(row_number() <= 10)
  #   
  #   m <- list(l = 50,r = 50,b = 0,t = 0, pad = 0)
  #   colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
  #   f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
  #   f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
  #   
  #   grap1 %>%
  #     plot_ly(x = ~Total, y = ~reorder(NOMBRE_EPS,Total), type = 'bar', orientation = 'h',
  #             text = ~comma(Total), textposition = 'auto', hoverinfo = 'text') %>%
  #     layout(margin = m,
  #            title = "Top 10",
  #            xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            paper_bgcolor='transparent',
  #            plot_bgcolor='transparent') %>%
  #     config(displayModeBar = F)
  # })
  # 
  # output$plot_tr_vulnerables_no <- renderPlotly({
  #   grap1 <- data_vulnerable_no() %>%
  #     mutate(Total = rowSums(.[,c(2:ncol(data_vulnerable_no()))], na.rm = T)) %>% 
  #     arrange(desc(Total)) %>%
  #     select(NOMBRE_EPS,Total) %>% 
  #     filter(row_number() <= 10)
  #   
  #   m <- list(l = 50,r = 50,b = 0,t = 0, pad = 0)
  #   colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
  #   f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
  #   f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
  #   
  #   grap1 %>%
  #     plot_ly(x = ~Total, y = ~reorder(NOMBRE_EPS,Total), type = 'bar', orientation = 'h',
  #             text = ~comma(Total), textposition = 'auto', hoverinfo = 'text') %>%
  #     layout(margin = m,
  #            title = "Top 10",
  #            xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            paper_bgcolor='transparent',
  #            plot_bgcolor='transparent') %>%
  #     config(displayModeBar = F)
  # })
  # 
  # ### Contactados ====
  # 
  # output$plot_tr_atendidos_si_vulnerables_si <- renderPlotly({
  #   grap1 <- data_atentidos_si_vulnerables_si() %>%
  #     mutate(Total = rowSums(.[,c(2:ncol(data_atentidos_si_vulnerables_si()))], na.rm = T)) %>%
  #     arrange(desc(Total)) %>%
  #     select(NOMBRE_EPS,Total) %>%
  #     filter(row_number() <= 10)
  # 
  #   m <- list(l = 50,r = 50,b = 0,t = 0, pad = 0)
  #   colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
  #   f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
  #   f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
  # 
  #   grap1 %>%
  #     plot_ly(x = ~Total, y = ~reorder(NOMBRE_EPS,Total), type = 'bar', orientation = 'h',
  #             text = ~comma(Total), textposition = 'auto', hoverinfo = 'text') %>%
  #     layout(margin = m,
  #            title = "Top 10",
  #            xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            paper_bgcolor='transparent',
  #            plot_bgcolor='transparent') %>%
  #     config(displayModeBar = F)
  # })
  # 
  # output$plot_tr_atendidos_si_vulnerables_no <- renderPlotly({
  #   grap1 <- data_atentidos_si_vulnerables_no() %>%
  #     mutate(Total = rowSums(.[,c(2:ncol(data_atentidos_si_vulnerables_no()))], na.rm = T)) %>%
  #     arrange(desc(Total)) %>%
  #     select(NOMBRE_EPS,Total) %>%
  #     filter(row_number() <= 10)
  #   
  #   m <- list(l = 50,r = 50,b = 0,t = 0, pad = 0)
  #   colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
  #   f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
  #   f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
  #   
  #   grap1 %>%
  #     plot_ly(x = ~Total, y = ~reorder(NOMBRE_EPS,Total), type = 'bar', orientation = 'h',
  #             text = ~comma(Total), textposition = 'auto', hoverinfo = 'text') %>%
  #     layout(margin = m,
  #            title = "Top 10",
  #            xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
  #            paper_bgcolor='transparent',
  #            plot_bgcolor='transparent') %>%
  #     config(displayModeBar = F)
  # })
  # 
})
