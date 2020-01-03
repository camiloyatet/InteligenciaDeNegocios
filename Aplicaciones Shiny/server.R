tipo_documento <- c("NIT","CC","CE","CD","PAS")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #Crea las tablas por eventos reactivos====
  data_ind<-eventReactive(input$Run,{
    data_ind <- filtrar_base(consumo_individual,input$numero_documento,month(min(input$estacionalidad)),month(max(input$estacionalidad)))
  })
  data_emp<- eventReactive(input$Run,{
    data_emp <- filtrar_base(consumo_empresarial,input$numero_documento,month(min(input$estacionalidad)),month(max(input$estacionalidad)))
  })
  data_info<-eventReactive(input$Run,{
    data_info<-informacion_empresa %>% 
      filter(id_empresa==input$numero_documento)
  })
#=============CREDITO====
  #Credito individual----
  output$individual_credito <- renderDataTable(datatable(
    data = totalizar_base(data_ind(),"anno") %>% 
      filter(ues=="Credito Social")%>%
      ungroup() %>% 
      select(-ues),
    extensions = c('FixedColumns',"FixedHeader"),
    class="table table-striped table-bordered  ",
    options = parametros_table_consumo_individual, 
    rownames = F,
    container = head_anual,
    callback = evento_click_credito_individual
  )%>% 
    formatCurrency(columnas_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
    formatCurrency(columnas_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
  )
  observeEvent(input$click_credito_individual, {
    servicio_filtro <- input$click_credito_individual
    output$individual_credito_descripcion <- renderDataTable(datatable(
      data = totalizar_base(data_ind(),"mes") %>%
        filter(servicio==servicio_filtro[1])%>%
        ungroup() %>% 
        select(-id_empresa,-ues,-servicio),
      extensions = c('FixedColumns',"FixedHeader"),
      class="table table-striped table-bordered",
      options = parametros_table_consumo_individual_detalle,
      rownames = F
    )%>% 
      formatCurrency(columnas_detalle_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
      formatCurrency(columnas_detalle_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
    )
  })
  observeEvent(input$click_credito_individual, {
    output$detalle_credito_servicio<- renderText(input$click_credito_individual[1]) 
  }
  )
  # output$total_consumo_2014<-renderInfoBox({
  #   infoBox(
  #     title = "",value= data_info()[1],subtitle = "Nit empresarial",icon = icon("address-card")
  #   )
  # })
  
  #Credito empresarial----
  output$empresarial_credito <- renderDataTable(datatable(
    data = totalizar_base_empresarial(data_emp(),"anno") %>% 
      filter(ues=="Credito Social")%>%
      ungroup() %>% 
      select(-ues),
    extensions = c('FixedColumns',"FixedHeader"),
    class="table table-striped table-bordered ",
    options = parametros_table_consumo_individual, 
    rownames = F,
    container = head_anual_empresarial,
    callback = evento_click_credito_empresarial
  )%>% 
    formatCurrency(columnas_empresarial_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
    formatCurrency(columnas_empresarial_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
  )
  observeEvent(input$click_credito_empresarial, {
    servicio_filtro <- input$click_credito_empresarial
    output$empresarial_credito_descripcion <- renderDataTable(datatable(
      data = totalizar_base_empresarial(data_emp(),"mes") %>%
        filter(servicio==servicio_filtro[1])%>%
        ungroup() %>% 
        select(-id_empresa,-ues,-servicio),
      extensions = c('FixedColumns',"FixedHeader"),
      class="table table-striped table-bordered",
      options = parametros_table_consumo_individual_detalle,
      rownames = F
    )%>% 
      formatCurrency(columnas_detalle_empresarial_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
      formatCurrency(columnas_detalle_empresarial_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
    )
  })
  observeEvent(input$click_credito_empresarial, {
    output$detalle_credito_servicio_empresarial<- renderText(input$click_credito_empresarial[1]) 
  }
  )
#=============EDUCACION====
  #Educación individual====    
  output$individual_educacion <- renderDataTable(datatable(
    data = totalizar_base(data_ind(),"anno") %>% 
      filter(ues=="Educacion")%>%
      ungroup() %>% 
      select(-ues),
    extensions = c('FixedColumns',"FixedHeader"),
    class="table table-striped table-bordered ",
    options = parametros_table_consumo_individual, 
    rownames = F,
    container = head_anual,
    callback = evento_click_educacion_individual
  )%>% 
    formatCurrency(columnas_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
    formatCurrency(columnas_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
  )
  observeEvent(input$click_educacion_individual, {
    servicio_filtro <- input$click_educacion_individual
    output$individual_educacion_descripcion <- renderDataTable(datatable(
      data = totalizar_base(data_ind(),"mes") %>%
        filter(servicio==servicio_filtro[1])%>%
        ungroup() %>% 
        select(-id_empresa,-ues,-servicio),
      extensions = c('FixedColumns',"FixedHeader"),
      class="table table-striped table-bordered",
      options = parametros_table_consumo_individual_detalle,
      rownames = F
    )%>% 
      formatCurrency(columnas_detalle_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
      formatCurrency(columnas_detalle_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
    )
  })
  observeEvent(input$click_educacion_individual, {
    output$detalle_educacion_servicio<- renderText(input$click_educacion_individual[1]) 
  }
  )
  #Educación empresarial----
output$empresarial_educacion <- renderDataTable(datatable(
    data = totalizar_base_empresarial(data_emp(),"anno") %>% 
      filter(ues=="Educacion")%>%
      ungroup() %>% 
      select(-ues),
    extensions = c('FixedColumns',"FixedHeader"),
    class="table table-striped table-bordered ",
    options = parametros_table_consumo_individual, 
    rownames = F,
    container = head_anual_empresarial,
    callback = evento_click_educacion_empresarial
  )%>% 
    formatCurrency(columnas_empresarial_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
    formatCurrency(columnas_empresarial_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
  )
  observeEvent(input$click_educacion_empresarial, {
    servicio_filtro <- input$click_educacion_empresarial
    output$empresarial_educacion_descripcion <- renderDataTable(datatable(
      data = totalizar_base_empresarial(data_emp(),"mes") %>%
        filter(servicio==servicio_filtro[1])%>%
        ungroup() %>% 
        select(-id_empresa,-ues,-servicio),
      extensions = c('FixedColumns',"FixedHeader"),
      class="table table-striped table-bordered",
      options = parametros_table_consumo_individual_detalle,
      rownames = F
    )%>% 
      formatCurrency(columnas_detalle_empresarial_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
      formatCurrency(columnas_detalle_empresarial_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
    )
  })
  observeEvent(input$click_educacion_empresarial, {
    output$detalle_educacion_servicio_empresarial<- renderText(input$click_educacion_empresarial[1]) 
  }
  )
#=============MERCADEO SOCIAL====
  #Mercadeo individual====    
  output$individual_mercadeo <- renderDataTable(datatable(
    data = totalizar_base(data_ind(),"anno") %>% 
      filter(ues=="Mercadeo social")%>%
      ungroup() %>% 
      select(-ues),
    extensions = c('FixedColumns',"FixedHeader"),
    class="table table-striped table-bordered ",
    options = parametros_table_consumo_individual, 
    rownames = F,
    container = head_anual,
    callback = evento_click_mercadeo_individual
  )%>% 
    formatCurrency(columnas_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
    formatCurrency(columnas_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
  )
  observeEvent(input$click_mercadeo_individual, {
    servicio_filtro <- input$click_mercadeo_individual
    output$individual_mercadeo_descripcion <- renderDataTable(datatable(
      data = totalizar_base(data_ind(),"mes") %>%
        filter(servicio==servicio_filtro[1])%>%
        ungroup() %>% 
        select(-id_empresa,-ues,-servicio),
      extensions = c('FixedColumns',"FixedHeader"),
      class="table table-striped table-bordered",
      options = parametros_table_consumo_individual_detalle,
      rownames = F
    )%>% 
      formatCurrency(columnas_detalle_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
      formatCurrency(columnas_detalle_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
    )
  })
  observeEvent(input$click_mercadeo_individual, {
    output$detalle_mercadeo_servicio<- renderText(input$click_mercadeo_individual[1]) 
  }
  )
  #Mercadeo empresarial----
  output$empresarial_mercadeo <- renderDataTable(datatable(
    data = totalizar_base_empresarial(data_emp(),"anno") %>% 
      filter(ues=="Mercadeo social")%>%
      ungroup() %>% 
      select(-ues),
    extensions = c('FixedColumns',"FixedHeader"),
    class="table table-striped table-bordered ",
    options = parametros_table_consumo_individual, 
    rownames = F,
    container = head_anual_empresarial,
    callback = evento_click_mercadeo_empresarial
  )%>% 
    formatCurrency(columnas_empresarial_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
    formatCurrency(columnas_empresarial_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
  )
  observeEvent(input$click_mercadeo_empresarial, {
    servicio_filtro <- input$click_mercadeo_empresarial
    output$empresarial_mercadeo_descripcion <- renderDataTable(datatable(
      data = totalizar_base_empresarial(data_emp(),"mes") %>%
        filter(servicio==servicio_filtro[1])%>%
        ungroup() %>% 
        select(-id_empresa,-ues,-servicio),
      extensions = c('FixedColumns',"FixedHeader"),
      class="table table-striped table-bordered",
      options = parametros_table_consumo_individual_detalle,
      rownames = F
    )%>% 
      formatCurrency(columnas_detalle_empresarial_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
      formatCurrency(columnas_detalle_empresarial_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
    )
  })
  observeEvent(input$click_mercadeo_empresarial, {
    output$detalle_mercadeo_servicio_empresarial<- renderText(input$click_mercadeo_empresarial[1]) 
  }
  )
#=============RECREACION Y TURISMO====
  #Recreacion individual====    
  output$individual_recreacion <- renderDataTable(datatable(
    data = totalizar_base(data_ind(),"anno") %>% 
      filter(ues=="RyT")%>%
      ungroup() %>% 
      select(-ues),
    extensions = c('FixedColumns',"FixedHeader"),
    class="table table-striped table-bordered ",
    options = parametros_table_consumo_individual, 
    rownames = F,
    container = head_anual,
    callback = evento_click_recreacion_individual
  )%>% 
    formatCurrency(columnas_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
    formatCurrency(columnas_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
  )
  observeEvent(input$click_recreacion_individual, {
    servicio_filtro <- input$click_recreacion_individual
    output$individual_recreacion_descripcion <- renderDataTable(datatable(
      data = totalizar_base(data_ind(),"mes") %>%
        filter(servicio==servicio_filtro[1])%>%
        ungroup() %>% 
        select(-id_empresa,-ues,-servicio),
      extensions = c('FixedColumns',"FixedHeader"),
      class="table table-striped table-bordered",
      options = parametros_table_consumo_individual_detalle,
      rownames = F
    )%>% 
      formatCurrency(columnas_detalle_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
      formatCurrency(columnas_detalle_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
    )
  })
  observeEvent(input$click_recreacion_individual, {
    output$detalle_recreacion_servicio<- renderText(input$click_recreacion_individual[1]) 
  }
  )
  #Recreacion empresarial----
  output$empresarial_recreacion <- renderDataTable(datatable(
    data = totalizar_base_empresarial(data_emp(),"anno") %>% 
      filter(ues=="RyT")%>%
      ungroup() %>% 
      select(-ues),
    extensions = c('FixedColumns',"FixedHeader"),
    class="table table-striped table-bordered ",
    options = parametros_table_consumo_individual, 
    rownames = F,
    container = head_anual_empresarial,
    callback = evento_click_recreacion_empresarial
  )%>% 
    formatCurrency(columnas_empresarial_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
    formatCurrency(columnas_empresarial_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
  )
  observeEvent(input$click_recreacion_empresarial, {
    servicio_filtro <- input$click_recreacion_empresarial
    output$empresarial_recreacion_descripcion <- renderDataTable(datatable(
      data = totalizar_base_empresarial(data_emp(),"mes") %>%
        filter(servicio==servicio_filtro[1])%>%
        ungroup() %>% 
        select(-id_empresa,-ues,-servicio),
      extensions = c('FixedColumns',"FixedHeader"),
      class="table table-striped table-bordered",
      options = parametros_table_consumo_individual_detalle,
      rownames = F
    )%>% 
      formatCurrency(columnas_detalle_empresarial_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
      formatCurrency(columnas_detalle_empresarial_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
    )
  })
  observeEvent(input$click_recreacion_empresarial, {
    output$detalle_recreacion_servicio_empresarial<- renderText(input$click_recreacion_empresarial[1]) 
  }
  )
#=============SALUD====
  #Salud individual====    
  output$individual_salud <- renderDataTable(datatable(
    data = totalizar_base(data_ind(),"anno") %>% 
      filter(ues=="Salud")%>%
      ungroup() %>% 
      select(-ues),
    extensions = c('FixedColumns',"FixedHeader"),
    class="table table-striped table-bordered ",
    options = parametros_table_consumo_individual, 
    rownames = F,
    container = head_anual,
    callback = evento_click_salud_individual
  )%>% 
    formatCurrency(columnas_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
    formatCurrency(columnas_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
  )
  observeEvent(input$click_salud_individual, {
    servicio_filtro <- input$click_salud_individual
    output$individual_salud_descripcion <- renderDataTable(datatable(
      data = totalizar_base(data_ind(),"mes") %>%
        filter(servicio==servicio_filtro[1])%>%
        ungroup() %>% 
        select(-id_empresa,-ues,-servicio),
      extensions = c('FixedColumns',"FixedHeader"),
      class="table table-striped table-bordered",
      options = parametros_table_consumo_individual_detalle,
      rownames = F
    )%>% 
      formatCurrency(columnas_detalle_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
      formatCurrency(columnas_detalle_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
    )
  })
  observeEvent(input$click_salud_individual, {
    output$detalle_salud_servicio<- renderText(input$click_salud_individual[1]) 
  }
  )
  #Salud empresarial----
  output$empresarial_salud <- renderDataTable(datatable(
    data = totalizar_base_empresarial(data_emp(),"anno") %>% 
      filter(ues=="Salud")%>%
      ungroup() %>% 
      select(-ues),
    extensions = c('FixedColumns',"FixedHeader"),
    class="table table-striped table-bordered ",
    options = parametros_table_consumo_individual, 
    rownames = F,
    container = head_anual_empresarial,
    callback = evento_click_salud_empresarial
  )%>% 
    formatCurrency(columnas_empresarial_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
    formatCurrency(columnas_empresarial_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
  )
  observeEvent(input$click_salud_empresarial, {
    servicio_filtro <- input$click_salud_empresarial
    output$empresarial_salud_descripcion <- renderDataTable(datatable(
      data = totalizar_base_empresarial(data_emp(),"mes") %>%
        filter(servicio==servicio_filtro[1])%>%
        ungroup() %>% 
        select(-id_empresa,-ues,-servicio),
      extensions = c('FixedColumns',"FixedHeader"),
      class="table table-striped table-bordered",
      options = parametros_table_consumo_individual_detalle,
      rownames = F
    )%>% 
      formatCurrency(columnas_detalle_empresarial_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
      formatCurrency(columnas_detalle_empresarial_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
    )
  })
  observeEvent(input$click_salud_empresarial, {
    output$detalle_salud_servicio_empresarial<- renderText(input$click_salud_empresarial[1]) 
  }
  )
#=============VIVIENDA====
#Vivienda individual====    
  output$individual_vivienda <- renderDataTable(datatable(
    data = totalizar_base(data_ind(),"anno") %>% 
      filter(ues=="Vivienda")%>%
      ungroup() %>% 
      select(-ues),
    extensions = c('FixedColumns',"FixedHeader"),
    class="table table-striped table-bordered ",
    options = parametros_table_consumo_individual, 
    rownames = F,
    container = head_anual,
    callback = evento_click_vivienda_individual
  )%>% 
    formatCurrency(columnas_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
    formatCurrency(columnas_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
  )
  observeEvent(input$click_vivienda_individual, {
    servicio_filtro <- input$click_vivienda_individual
    output$individual_vivienda_descripcion <- renderDataTable(datatable(
      data = totalizar_base(data_ind(),"mes") %>%
        filter(servicio==servicio_filtro[1])%>%
        ungroup() %>% 
        select(-id_empresa,-ues,-servicio),
      extensions = c('FixedColumns',"FixedHeader"),
      class="table table-striped table-bordered",
      options = parametros_table_consumo_individual_detalle,
      rownames = F
    )%>% 
      formatCurrency(columnas_detalle_individual_moneda,currency = "$ ",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))%>% 
      formatCurrency(columnas_detalle_individual_numero,currency = "",digits = 0,interval = 3, mark = ".",dec.mark=getOption("OutDec"))
    )
  })
  observeEvent(input$click_vivienda_individual, {
    output$detalle_vivienda_servicio<- renderText(input$click_vivienda_individual[1]) 
  }
  )
#=============INFORMACION EMPRESA====
  #Credito----
  output$nit_empresarial_credito<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[1],subtitle = "Nit empresarial",icon = icon("address-card")
    )
  })
  output$razon_social_credito<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[2],subtitle = "Razón social"
    )
  })
  output$estado_empresa_credito<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[3],subtitle = "Estado empresarial",icon = icon("fas fa-check-circle")
    )
  })
  #Educacion----
  output$nit_empresarial_educacion<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[1],subtitle = "Nit empresarial",icon = icon("address-card")
    )
  })
  output$razon_social_educacion<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[2],subtitle = "Razón social"
    )
  })
  output$estado_empresa_educacion<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[3],subtitle = "Estado empresarial",icon = icon("fas fa-check-circle")
    )
  })
  #Mercadeo----
  output$nit_empresarial_mercadeo<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[1],subtitle = "Nit empresarial",icon = icon("address-card")
    )
  })
  output$razon_social_mercadeo<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[2],subtitle = "Razón social"
    )
  })
  output$estado_empresa_mercadeo<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[3],subtitle = "Estado empresarial",icon = icon("fas fa-check-circle")
    )
  })
  #Recreacion----
  output$nit_empresarial_recreacion<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[1],subtitle = "Nit empresarial",icon = icon("address-card")
    )
  })
  output$razon_social_recreacion<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[2],subtitle = "Razón social"
    )
  })
  output$estado_empresa_recreacion<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[3],subtitle = "Estado empresarial",icon = icon("fas fa-check-circle")
    )
  })
  #Salud----
  output$nit_empresarial_salud<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[1],subtitle = "Nit empresarial",icon = icon("address-card")
    )
  })
  output$razon_social_salud<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[2],subtitle = "Razón social"
    )
  })
  output$estado_empresa_salud<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[3],subtitle = "Estado empresarial",icon = icon("fas fa-check-circle")
    )
  })
  
  #Vivienda----
  output$nit_empresarial_vivienda<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[1],subtitle = "Nit empresarial",icon = icon("address-card")
    )
  })
  output$razon_social_vivienda<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[2],subtitle = "Razón social"
    )
  })
  output$estado_empresa_vivienda<-renderInfoBox({
    infoBox(
      title = "",value= data_info()[3],subtitle = "Estado empresarial",icon = icon("fas fa-check-circle")
    )
  })
})


### Funciones ====

#Filtra la base completa de consumo
filtrar_base<- function(base,empresa,min,max){
  filtro<-base%>%
    filter(id_empresa==empresa, between(mes,min,max))
  return(filtro)
}
# Agrupa la base para hacer el pivot.
agrupar_base<-function(base,filtro_anno){
  respuesta <- base%>%
    filter(anno==filtro_anno) %>% 
    ungroup()%>%
    select(ues,servicio,afiliados,consumo,-id_empresa,-anno)
  names(respuesta)[3]<- paste0("afiliados",as.character(filtro_anno))
  names(respuesta)[4]<- paste0("consumo",as.character(filtro_anno))
  return(respuesta)
}
# Agrupa la base para hacer el pivot empresarial.
agrupar_base_empresarial<-function(base,filtro_anno){
  respuesta <- base%>%
    filter(anno==filtro_anno) %>% 
    ungroup()%>%
    select(ues,servicio,transacciones,consumo,-id_empresa,-anno)
  names(respuesta)[3]<- paste0("transaccion",as.character(filtro_anno))
  names(respuesta)[4]<- paste0("consumo",as.character(filtro_anno))
  return(respuesta)
}
# Cuadra la base por columnas
moldear_base <- function(base){
  agrupacion_2015 <- agrupar_base(base,2015)
  agrupacion_2016 <- agrupar_base(base,2016)
  agrupacion_2017 <- agrupar_base(base,2017)
  agrupacion_2018 <- agrupar_base(base,2018)
  agrupacion_2019 <- agrupar_base(base,2019)
  base_agrupada<-base %>% 
    ungroup() %>% 
    select(ues,servicio,-anno,-id_empresa)%>% 
    group_by(ues,servicio)%>%
    filter(row_number()==1) %>% 
    left_join(agrupacion_2015,by = c("ues"="ues","servicio"="servicio")) %>% 
    left_join(agrupacion_2016,by = c("ues"="ues","servicio"="servicio")) %>% 
    left_join(agrupacion_2017,by = c("ues"="ues","servicio"="servicio")) %>% 
    left_join(agrupacion_2018,by = c("ues"="ues","servicio"="servicio")) %>% 
    left_join(agrupacion_2019,by = c("ues"="ues","servicio"="servicio")) 
  rm(agrupacion_2015)
  rm(agrupacion_2016)
  rm(agrupacion_2017)
  rm(agrupacion_2018)
  rm(agrupacion_2019)
  return(base_agrupada)
}
# Cuadra la base por columnas empresarial
moldear_base_empresarial <- function(base){
  agrupacion_2015 <- agrupar_base_empresarial(base,2015)
  agrupacion_2016 <- agrupar_base_empresarial(base,2016)
  agrupacion_2017 <- agrupar_base_empresarial(base,2017)
  agrupacion_2018 <- agrupar_base_empresarial(base,2018)
  agrupacion_2019 <- agrupar_base_empresarial(base,2019)
  base_agrupada<-base %>% 
    ungroup() %>% 
    select(ues,servicio,-anno,-id_empresa)%>% 
    group_by(ues,servicio)%>%
    filter(row_number()==1) %>% 
    left_join(agrupacion_2015,by = c("ues"="ues","servicio"="servicio")) %>% 
    left_join(agrupacion_2016,by = c("ues"="ues","servicio"="servicio")) %>% 
    left_join(agrupacion_2017,by = c("ues"="ues","servicio"="servicio")) %>% 
    left_join(agrupacion_2018,by = c("ues"="ues","servicio"="servicio")) %>% 
    left_join(agrupacion_2019,by = c("ues"="ues","servicio"="servicio")) 
  rm(agrupacion_2015)
  rm(agrupacion_2016)
  rm(agrupacion_2017)
  rm(agrupacion_2018)
  rm(agrupacion_2019)
  return(base_agrupada)
}
#Totalizar la base de datos por mes o por anno individual----
totalizar_base<-function(base,agrupacion){
  if(agrupacion=="anno"){
    agrupacion <- base%>%
      group_by(id_empresa,anno,ues,servicio)%>%
      summarise(afiliados= n_distinct(id_persona), usos = n(), consumo=sum(consumo),transacciones=sum(transacciones))  
    base_final<-moldear_base(agrupacion)
  }else if(agrupacion=="mes"){
    agrupacion <- base%>%
      group_by(id_empresa,anno,mes,ues,servicio)%>%
      summarise(afiliados= n_distinct(id_persona), usos = n(), consumo=sum(consumo),transacciones=sum(transacciones))  
    base_final<-agrupacion
  }
  
  return(base_final)
}
#Totalizar la base de datos por mes o por anno empresarial----
totalizar_base_empresarial<-function(base,agrupacion){
  if(agrupacion=="anno"){
    agrupacion <- base%>%
      group_by(id_empresa,anno,ues,servicio)%>%
      summarise(consumo=sum(consumo),transacciones=sum(transacciones))  
    base_final<-moldear_base_empresarial(agrupacion)
  }else if(agrupacion=="mes"){
    agrupacion <- base%>%
      group_by(id_empresa,anno,mes,ues,servicio)%>%
      summarise(consumo=sum(consumo),transacciones=sum(transacciones))  
    base_final<-agrupacion
  }
  
  return(base_final)
}
#head_anual individual----
head_anual = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2,'Servicio'),
      th(colspan = 2, '2015'),
      th(colspan = 2, '2016'),
      th(colspan = 2, '2017'),
      th(colspan = 2, '2018'),
      th(colspan = 2, '2019')
    ),
    tr(
      lapply(rep(c("Afiliados", "Consumo"),5 ), th)
    )
  )
))
#head_anual empresarial----
head_anual_empresarial = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2,'Servicio'),
      th(colspan = 2, '2015'),
      th(colspan = 2, '2016'),
      th(colspan = 2, '2017'),
      th(colspan = 2, '2018'),
      th(colspan = 2, '2019')
    ),
    tr(
      lapply(rep(c("Consumo", "transacion"),5 ), th)
    )
  )
))
#Evento de javascript para la accion click----
evento_click_credito_individual = JS("table.on('click.dt', 'td', function() {
                                     var data = table.row(this).data();
                                     Shiny.onInputChange('click_credito_individual',data);
                                     $('#detalle_individual_credito').modal('show');
                                     
                                     });")
evento_click_credito_empresarial = JS("table.on('click.dt', 'td', function() {
                                      var data = table.row(this).data();
                                      Shiny.onInputChange('click_credito_empresarial',data);
                                      $('#detalle_empresarial_credito').modal('show');
                                      
});")
evento_click_educacion_individual = JS("table.on('click.dt', 'td', function() {
                                       var data = table.row(this).data();
                                       Shiny.onInputChange('click_educacion_individual',data);
                                       $('#detalle_individual_educacion').modal('show');
});")
evento_click_educacion_empresarial = JS("table.on('click.dt', 'td', function() {
                                        var data = table.row(this).data();
                                        Shiny.onInputChange('click_educacion_empresarial',data);
                                        $('#detalle_empresarial_educacion').modal('show');
});")

evento_click_mercadeo_individual = JS("table.on('click.dt', 'td', function() {
                                      var data = table.row(this).data();
                                      Shiny.onInputChange('click_mercadeo_individual',data);
                                      $('#detalle_individual_mercadeo').modal('show');
                                      });")
evento_click_mercadeo_empresarial = JS("table.on('click.dt', 'td', function() {
                                       var data = table.row(this).data();
                                       Shiny.onInputChange('click_mercadeo_empresarial',data);
                                       $('#detalle_empresarial_mercadeo').modal('show');
});")
evento_click_recreacion_individual = JS("table.on('click.dt', 'td', function() {
                                        var data = table.row(this).data();
                                        Shiny.onInputChange('click_recreacion_individual',data);
                                        $('#detalle_individual_recreacion').modal('show');
});")
evento_click_recreacion_empresarial = JS("table.on('click.dt', 'td', function() {
                                         var data = table.row(this).data();
                                         Shiny.onInputChange('click_recreacion_empresarial',data);
                                         $('#detalle_empresarial_recreacion').modal('show');
});")
evento_click_salud_individual = JS("table.on('click.dt', 'td', function() {
                                   var data = table.row(this).data();
                                   Shiny.onInputChange('click_salud_individual',data);
                                   $('#detalle_individual_salud').modal('show');
});")
evento_click_salud_empresarial = JS("table.on('click.dt', 'td', function() {
                                    var data = table.row(this).data();
                                    Shiny.onInputChange('click_salud_empresarial',data);
                                    $('#detalle_empresarial_salud').modal('show');
});")
evento_click_vivienda_individual = JS("table.on('click.dt', 'td', function() {
                                      var data = table.row(this).data();
                                      Shiny.onInputChange('click_vivienda_individual',data);
                                      $('#detalle_individual_vivienda').modal('show');
});")


#Vector de nombres de las columnas----
columnas_individual_moneda<-c("consumo2015","consumo2016","consumo2017","consumo2018","consumo2019")
columnas_individual_numero<-c("afiliados2015","afiliados2016","afiliados2017","afiliados2018","afiliados2019")
columnas_empresarial_moneda<-c("consumo2015","consumo2016","consumo2017","consumo2018","consumo2019")
columnas_empresarial_numero<-c("transaccion2015","transaccion2016","transaccion2017","transaccion2018","transaccion2019")

columnas_detalle_individual_numero<-c("afiliados","usos","transacciones")
columnas_detalle_individual_moneda<-c("consumo")
columnas_detalle_empresarial_numero<-c("transacciones")
columnas_detalle_empresarial_moneda<-c("consumo")

#parametros de la tabla----
parametros_table_consumo_individual<-list(
  pageLength = 10, 
  dom = 't', 
  scrolly=TRUE,
  lengthChange = FALSE,
  autowidth = TRUE,
  scrollX=TRUE,
  scrollY="230px",
  paging=F,
  fixedHeader=TRUE,
  fixedColumns = list(leftColumns = 1, rightColumns = 0)
)

parametros_table_consumo_individual_detalle<-list(
  # pageLength = 10, 
  # dom = 't', 
  scrolly=TRUE,
  # lengthChange = FALSE,
  # autowidth = TRUE,
  scrollX=TRUE,
  scrollY="230px",
  # paging=F,
  fixedHeader=TRUE,
  fixedColumns = list(leftColumns = 1, rightColumns = 0),
  buttons = list("copy", list(
    extend = "collection"
    , buttons = c("csv", "excel", "pdf")
    , text = "Download"
  ) )
)

consumo_empresarial <- readRDS(file = "data/consumo_empresarial.rds")
consumo_individual<-readRDS(file = "data/consumo_individual.rds")
informacion_empresa<-readRDS(file = "data/informacion_empresas.rds")
