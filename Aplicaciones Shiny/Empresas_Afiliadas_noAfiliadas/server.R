shinyServer(function(input, output) {
        
        
        
        
        #Filtro de empresa
        
        noafiliadas  <-   shiny::reactive({noafiliadas <- no_afiliadas %>% 
                dplyr::filter(id_empresa==input$razonsocial2|razon_social==input$razonsocial2)})
        
        afiliadas    <-  shiny::reactive({afiliadas<- emp_afiliadas %>% 
                dplyr::filter(id_empresa==input$razonsocial|razon_social==input$razonsocial)})
        
        
        # construccion de matriz de distancia -------------------------------------
        
        #Empresas afiliadas-infraestructura y convenios
        matrix_alm_si_full     <- shiny::reactive({
                distancia   <- function(matrix_a,infra=T){
                        store <- matrix(0, nrow = dim(matrix_a)[1], 
                                        ncol = ifelse(infra==T,dim(infraestructuras)[1],dim(convenios)[1]))
                        total <- dim(matrix_a)[1]
                        pb    <- txtProgressBar(min = 0, max = total, style = 3,char = "##")
                        for (i in 1:dim(matrix_a)[1]) {
                                
                                Sys.sleep(0.1)
                                for (j in 1:ifelse(infra==T,dim(infraestructuras)[1],dim(convenios)[1])) {
                                        if(infra==T){
                                                store[i,j] <- geosphere::distHaversine(c(matrix_a$CY[i],matrix_a$CX[i]), c(infraestructuras$CY[j],infraestructuras$CX[j]))/1000
                                        }else{
                                                store[i,j] <- geosphere::distHaversine(c(matrix_a$CY[i],matrix_a$CX[i]), c(convenios$CY[j],convenios$CX[j]))/1000      
                                                
                                        }
                                }
                                setTxtProgressBar(pb, i)
                        }
                        close(pb)
                        
                        store <- store %>% data.frame()
                        
                        row.names(store) <- matrix_a[,1]
                        
                        if(infra==T){
                                colnames(store)  <- infraestructuras$NOMBRE
                        }else{
                                colnames(store)  <- convenios$RSCOD     
                        }
                        
                        return(store)
                }
                matrix_alm_si_full <- distancia(matrix_a = afiliadas(),infra = T)
        })
        
        matrix_convenio_afi    <- reactive({
                distancia   <- function(matrix_a,infra=T){
                        store <- matrix(0, nrow = dim(matrix_a)[1], 
                                        ncol = ifelse(infra==T,dim(infraestructuras)[1],dim(convenios)[1]))
                        total <- dim(matrix_a)[1]
                        pb    <- txtProgressBar(min = 0, max = total, style = 3,char = "##")
                        for (i in 1:dim(matrix_a)[1]) {
                                
                                Sys.sleep(0.1)
                                for (j in 1:ifelse(infra==T,dim(infraestructuras)[1],dim(convenios)[1])) {
                                        if(infra==T){
                                                store[i,j] <- geosphere::distHaversine(c(matrix_a$CY[i],matrix_a$CX[i]), c(infraestructuras$CY[j],infraestructuras$CX[j]))/1000
                                        }else{
                                                store[i,j] <- geosphere::distHaversine(c(matrix_a$CY[i],matrix_a$CX[i]), c(convenios$CY[j],convenios$CX[j]))/1000      
                                                
                                        }
                                }
                                setTxtProgressBar(pb, i)
                        }
                        close(pb)
                        
                        store <- store %>% data.frame()
                        
                        row.names(store) <- matrix_a[,1]
                        
                        if(infra==T){
                                colnames(store)  <- infraestructuras$NOMBRE
                        }else{
                                colnames(store)  <- convenios$RSCOD     
                        }
                        
                        return(store)
                }
                matrix_convenio_afi <- distancia(matrix_a =afiliadas(),infra = F )
        })
        
        #Empresas no afiliadas-infraestructura y convenios
        matrix_alm_no_full     <- shiny::reactive({
                distancia   <- function(matrix_a,infra=T){
                        store <- matrix(0, nrow = dim(matrix_a)[1], 
                                        ncol = ifelse(infra==T,dim(infraestructuras)[1],dim(convenios)[1]))
                        total <- dim(matrix_a)[1]
                        pb    <- txtProgressBar(min = 0, max = total, style = 3,char = "##")
                        for (i in 1:dim(matrix_a)[1]) {
                                
                                Sys.sleep(0.1)
                                for (j in 1:ifelse(infra==T,dim(infraestructuras)[1],dim(convenios)[1])) {
                                        if(infra==T){
                                                store[i,j] <- geosphere::distHaversine(c(matrix_a$CY[i],matrix_a$CX[i]), c(infraestructuras$CY[j],infraestructuras$CX[j]))/1000
                                        }else{
                                                store[i,j] <- geosphere::distHaversine(c(matrix_a$CY[i],matrix_a$CX[i]), c(convenios$CY[j],convenios$CX[j]))/1000      
                                                
                                        }
                                }
                                setTxtProgressBar(pb, i)
                        }
                        close(pb)
                        
                        store <- store %>% data.frame()
                        
                        row.names(store) <- matrix_a[,1]
                        
                        if(infra==T){
                                colnames(store)  <- infraestructuras$NOMBRE
                        }else{
                                colnames(store)  <- convenios$RSCOD     
                        }
                        
                        return(store)
                }
                matrix_alm_no_full <- distancia(matrix_a = noafiliadas(),infra = T)
        })
        
        matrix_convenio_no_afi <- shiny::reactive({
                distancia   <- function(matrix_a,infra=T){
                        store <- matrix(0, nrow = dim(matrix_a)[1],
                                        ncol = ifelse(infra==T,dim(infraestructuras)[1],dim(convenios)[1]))
                        total <- dim(matrix_a)[1]
                        pb    <- txtProgressBar(min = 0, max = total, style = 3,char = "##")
                        for (i in 1:dim(matrix_a)[1]) {

                                Sys.sleep(0.1)
                                for (j in 1:ifelse(infra==T,dim(infraestructuras)[1],dim(convenios)[1])) {
                                        if(infra==T){
                                                store[i,j] <- geosphere::distHaversine(c(matrix_a$CY[i],matrix_a$CX[i]), c(infraestructuras$CY[j],infraestructuras$CX[j]))/1000
                                        }else{
                                                store[i,j] <- geosphere::distHaversine(c(matrix_a$CY[i],matrix_a$CX[i]), c(convenios$CY[j],convenios$CX[j]))/1000

                                        }
                                }
                                setTxtProgressBar(pb, i)
                        }
                        close(pb)

                        store <- store %>% data.frame()

                        row.names(store) <- matrix_a[,1]

                        if(infra==T){
                                colnames(store)  <- infraestructuras$NOMBRE
                        }else{
                                colnames(store)  <- convenios$RSCOD
                        }

                        return(store)
                }
                matrix_convenio_no_afi <- distancia(matrix_a = noafiliadas(),infra = F)
        })
        
        # Empresas Afiliadas ------------------------------------------------------
        
        output$title           <- renderText({"EMPRESAS AFILIADAS"})
        

        tabla                  <- reactive({
                infra.names <- levels(as.factor(infraestructuras$TIPO))
                afiliados_piramide   <- function(matrix,radio= input$distancia ,empresas=1){

                        resultado <- NULL

                        for(kk in 1:empresas){

                                index   <- which(matrix[kk,]<= radio)

                                dt     <- as.data.frame(table(infraestructuras$TIPO[index]))

                                index.2 <- which(levels(as.factor(infraestructuras$TIPO))%in%dt$Var1)

                                if(length(index)==0){
                                        dt2 <- data.frame(Var1=levels(as.factor(infraestructuras$TIPO)),Freq=0)

                                }else{
                                        dt2 <- data.frame(Var1=levels(as.factor(infraestructuras$TIPO))[-index.2],Freq=0)

                                }

                                conteo  <- rbind(dt,dt2)
                                conteo  <- spread(conteo, Var1, Freq)
                                conteo  <- conteo[ , order(names(conteo))]

                                resultado  <- rbind(resultado,conteo)

                        }
                        rownames(resultado) <-  rownames(matrix)[1:empresas]
                        resultado$TOTAL     <-  rowSums(resultado)

                        return(resultado)

                }
                tabla <- afiliados_piramide(matrix= matrix_alm_si_full(),radio = input$distancia)
                tabla <- tabla %>% mutate(empresas=row.names(tabla)) 
                tabla <- tabla %>%left_join(select(afiliadas(),razon_social,id_empresa,piramide_1,piramide_2),
                                            by = c("empresas"="id_empresa")) %>% #dplyr::filter(CSC!=0) %>%
                        select(empresas,razon_social,piramide_1,piramide_2,CSC:TOTAL)

        })
        tabla2                 <- reactive({
                
                prospecto <- function(matrix,radio=input$distancia,empresas=1){
                        
                        dt <-NULL
                        for(kk in 1:empresas){
                                
                                index                  <- which(matrix[kk,]<= radio)
                                infra                  <- names(matrix)[index]
                                prospecto              <- rownames(matrix)[kk]
                                
                                if(length(index)==0){
                                        consolidado          <- data.frame(prospecto,infraestructura=NA)
                                        un_list              <- rbind.data.frame(0)
                                        
                                }else{
                                        consolidado          <- data.frame(prospecto,infraestructura=infra)
                                        dist                 <- c(matrix[kk, index])
                                        un_list              <- rbind.data.frame(dist)
                                        
                                }
                                colnames(un_list)    <- "Distancia"
                                
                                consolidado$distancia <- un_list$Distancia
                                row_order <- order(consolidado$distancia, decreasing = FALSE)
                                consolidado <- consolidado[row_order, , drop = FALSE]
                                
                                if(length(index)==0){
                                        consolidado$orden_de_cercania <- 1
                                }else{
                                        consolidado$orden_de_cercania <- 1:length(index)
                                }
                                dt          <- rbind(dt,consolidado)
                                dt$prospecto       <- as.character(dt$prospecto) 
                                dt$infraestructura <- as.character(dt$infraestructura) 
                        }
                        return(dt)
                }
                tabla2    <- prospecto (matrix= matrix_alm_si_full(),radio = input$distancia)

                tabla2    <- tabla2 %>%left_join(select(afiliadas(),razon_social,id_empresa,piramide_1,piramide_2,CX,CY),
                                                 by = c("prospecto"="id_empresa"))
                
                tabla2 <- tabla2 %>% left_join(select(infraestructuras,NOMBRE,TIPO),by=c("infraestructura"="NOMBRE"))
                tabla2 <- tabla2 %>% select(-distancia) %>% dplyr::filter(infraestructura!=0) %>% 
                        select(prospecto,razon_social,piramide_1,piramide_2,infraestructura,TIPO,orden_de_cercania,CX,CY)
                
                
        })
        tabla3                 <- reactive({
                
                prospecto <- function(matrix,radio=input$distancia,empresas=1){
                        
                        dt <-NULL
                        for(kk in 1:empresas){
                                
                                index                  <- which(matrix[kk,]<= radio)
                                infra                  <- names(matrix)[index]
                                prospecto              <- rownames(matrix)[kk]
                                
                                if(length(index)==0){
                                        consolidado          <- data.frame(prospecto,infraestructura=NA)
                                        un_list              <- rbind.data.frame(0)
                                        
                                }else{
                                        consolidado          <- data.frame(prospecto,infraestructura=infra)
                                        dist                 <- c(matrix[kk, index])
                                        un_list              <- rbind.data.frame(dist)
                                        
                                }
                                colnames(un_list)    <- "Distancia"
                                
                                consolidado$distancia <- un_list$Distancia
                                row_order <- order(consolidado$distancia, decreasing = FALSE)
                                consolidado <- consolidado[row_order, , drop = FALSE]
                                
                                if(length(index)==0){
                                        consolidado$orden_de_cercania <- 1
                                }else{
                                        consolidado$orden_de_cercania <- 1:length(index)
                                }
                                dt          <- rbind(dt,consolidado)
                                dt$prospecto       <- as.character(dt$prospecto) 
                                dt$infraestructura <- as.character(dt$infraestructura) 
                        }
                        return(dt)
                }
                tabla3 <- prospecto (matrix= matrix_convenio_afi(),radio = input$distancia)
                tabla3 <- tabla3 %>%left_join(dplyr::select(afiliadas(),razon_social,id_empresa,piramide_1,piramide_2,CX,CY),
                                              by = c("prospecto"="id_empresa"))
                
                tabla3 <- tabla3 %>% left_join(select(convenios,RSCOD,CATEGORIA),by=c("infraestructura"="RSCOD"))
                tabla3 <- tabla3 %>% dplyr::select(-distancia) %>% dplyr::filter(infraestructura!=0) %>% 
                        dplyr::select(prospecto,razon_social,piramide_1,piramide_2,infraestructura,CATEGORIA,orden_de_cercania,CX,CY) %>% 
                        dplyr::distinct()
                
                
        }) 
        tabla4                 <- reactive({
                
                afiliados_piramide   <- function(matrix,radio= input$distancia ,empresas=1){
                        
                        resultado <- NULL
                        
                        for(kk in 1:empresas){
                                
                                index   <- which(matrix[kk,]<= radio)
                                
                                dt     <- as.data.frame(table(convenios$CATEGORIA[index]))
                                
                                index.2 <- which(levels(as.factor(convenios$CATEGORIA))%in%dt$Var1)
                                
                                if(length(index)==0){
                                        dt2 <- data.frame(Var1=levels(as.factor(convenios$CATEGORIA)),Freq=0)
                                        
                                }else{
                                        dt2 <- data.frame(Var1=levels(as.factor(convenios$CATEGORIA))[-index.2],Freq=0)
                                        
                                }
                                
                                conteo  <- rbind(dt,dt2)
                                conteo  <- spread(conteo, Var1, Freq)
                                conteo  <- conteo[ , order(names(conteo))]
                                
                                resultado  <- rbind(resultado,conteo)
                                
                        }
                        rownames(resultado) <-  rownames(matrix)[1:empresas]
                        resultado$TOTAL     <-  rowSums(resultado)
                        
                        return(resultado)
                        
                }
                tabla <- afiliados_piramide(matrix= matrix_convenio_afi(),radio = input$distancia)
                tabla <- tabla %>% dplyr::mutate(empresas=row.names(tabla))
                tabla <- tabla %>%dplyr::left_join(select(afiliadas(),razon_social,id_empresa,piramide_1,piramide_2),
                                            by = c("empresas"="id_empresa"))  %>% 
                        dplyr::select(empresas,razon_social,piramide_1,piramide_2,`ALIMENTOS Y BEBIDAS`:TOTAL)
                
        })
        
        ##salidas de las tablas
        output$tabla     <-  DT::renderDataTable({
                tabla() %>% dplyr::filter(razon_social==input$razonsocial|empresas==input$razonsocial) %>%
                        DT::datatable(


                                options = list(scrollX="600px",scrollY="80px",pageLength = 5,paging = T,
                                               initComplete = JS(
                                                       "function(settings, json) {",
                                                       "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                                                       "}")),
                                colnames = c("NIT", "RAZON SOCIAL","PIRAMIDE 1","PIRAMIDE 2","CENTROS DE SERVICIO","DROGUERIA",
                                             "HOTEL","INSTITUTO","PARQUE","RECREACION","SALUD","SERVICIO","SUPERMERCADO","VIVIENDA",
                                             "TOTAL")
                        )

        })
        output$csc       <-  DT::renderDataTable({
                
                tabla2() %>%select(-CX,-CY) %>% dplyr::filter(razon_social==input$razonsocial|prospecto==input$razonsocial) %>% DT::datatable(options = list(pageLength = 10,paging = T,
                                                                                                                                                        initComplete = JS(
                                                                                                                                                                "function(settings, json) {",
                                                                                                                                                                "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                                                                                                                                                                "}")),
                                                                                                                                         colnames = c("NIT", "RAZON SOCIAL","PIRAMIDE 1","PIRAMIDE 2","INFRAESTRUCTURA","TIPO","ORDEN DE CERCANIA") 
                )
                
        })
        output$tabla3    <-  DT::renderDataTable({
                tabla3()    %>%select(-CX,-CY) %>% dplyr::filter(razon_social==input$razonsocial|prospecto==input$razonsocial) %>% DT::datatable(options = list(pageLength = 10,paging = T,
                                                                                                                                                           initComplete = JS(
                                                                                                                                                                   "function(settings, json) {",
                                                                                                                                                                   "$(this.api().table().header()).css({'background-color': '#E82C23', 'color': '#fff'});",
                                                                                                                                                                   "}")),
                                                                                                                                            colnames = c("NIT", "RAZON SOCIAL","PIRAMIDE 1","PIRAMIDE 2","CONVENIO","CATEGORIA","ORDEN DE CERCANIA") 
                                                                                                                                            
                )                   
                
        })
        output$tabla4    <-  DT::renderDataTable({
                tabla4() %>% dplyr::filter(razon_social==input$razonsocial|empresas==input$razonsocial) %>%
                        rename(NIT=empresas,piramide_1=piramide_1,piramide_2=piramide_2) %>%
                        DT::datatable(


                                options = list(scrollX="600px",scrollY="80px",pageLength = 5,paging = T,
                                               initComplete = JS(
                                                       "function(settings, json) {",
                                                       "$(this.api().table().header()).css({'background-color': '#E82C23', 'color': '#fff'});",
                                                       "}")))
        })
        output$csc2      <-  DT::renderDataTable({
                
                tabla2() %>%left_join(select(infraestructuras,NOMBRE,CX,CY),by=c("infraestructura"="NOMBRE")) %>%  
                        DT::datatable(options = list(pageLength = 5,paging = T,
                                                     initComplete = JS(
                                                             "function(settings, json) {",
                                                             "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                                                             "}")))
                
        })
         

        ## Salidas mapas
        
        #ICONOS PARA INFRAESTRUCTURA 
        leafIconsCS   <- icons(iconUrl = "Colsubsidio.png",   
                               iconWidth = 28, iconHeight = 45,
                               iconAnchorX = 16, iconAnchorY = 40)
        
        leafIconsCS2  <- icons(iconUrl = "factory.png",   
                               iconWidth = 28, iconHeight = 45,
                               iconAnchorX = 16, iconAnchorY = 40)
        
        leafIconsED   <- icons(iconUrl = "Educacion.png",   
                               iconWidth = 28, iconHeight = 45,
                               iconAnchorX = 16, iconAnchorY = 40)
        
        leafIconsSP   <- icons(iconUrl="Supermercados.png",   
                               iconWidth = 28, iconHeight = 45,
                               iconAnchorX = 16, iconAnchorY = 40)
        
        leafIconsDR   <- icons(iconUrl="Farmacias.png",
                               iconWidth = 28, iconHeight = 45,
                               iconAnchorX = 16, iconAnchorY = 40)
        
        leafIconsRYT  <- icons(iconUrl = "Recreacion.png",   
                               iconWidth = 28, iconHeight = 45,
                               iconAnchorX = 16, iconAnchorY = 40)
        
        leafIconsSL   <- icons(iconUrl = "Salud.png",
                               iconWidth = 28, iconHeight = 45,
                               iconAnchorX = 16, iconAnchorY = 40)
        
        leafIconsVV   <- icons(iconUrl = "Vivienda.png",
                               iconWidth = 28, iconHeight = 45,
                               iconAnchorX = 16, iconAnchorY = 40)
        
        leafIconsCUBO <- icons(iconUrl = "ElCubo.png",
                               iconWidth = 28, iconHeight = 45,
                               iconAnchorX = 16, iconAnchorY = 40)
        
        #ICONOS CONVENIOS
        
        icon.ali     <- makeAwesomeIcon(icon = "glyphicon glyphicon-cutlery", markerColor = 'red', iconColor = 'black')
        icon.alm     <- makeAwesomeIcon(icon = "glyphicon glyphicon-shopping-cart", markerColor = 'green', iconColor = 'black')
        icon.calz    <- makeAwesomeIcon(icon = "glyphicon glyphicon-ice-lolly", markerColor = "purple", iconColor = 'black')
        icon.depor   <- makeAwesomeIcon(icon = "glyphicon glyphicon-knight", markerColor = "pink", iconColor = 'black')
        icon.diver   <- makeAwesomeIcon(icon = "glyphicon glyphicon-sunglasses", markerColor = "white", iconColor = 'black')
        icon.educcon <- makeAwesomeIcon(icon = "glyphicon glyphicon-education", markerColor = "gray", iconColor = 'black')
        icon.electro <- makeAwesomeIcon(icon = "glyphicon glyphicon-blackboard", markerColor = "beige", iconColor = 'black')
        icon.espe    <- makeAwesomeIcon(icon = "glyphicon glyphicon-search", markerColor = "darkred", iconColor = 'black')
        icon.hogar   <- makeAwesomeIcon(icon = "glyphicon glyphicon-home", markerColor = "lightred", iconColor = 'black')
        icon.masco   <- makeAwesomeIcon(icon = "fa-paw",markerColor = "lightgreen",iconColor = 'black',library = "fa")
        icon.sabie   <- makeAwesomeIcon(icon = "fa-plus-square",markerColor = "darkblue",iconColor = 'black',library = "fa")
        icon.segu    <- makeAwesomeIcon(icon = "fa-certificate",markerColor = "lightblue",iconColor = 'black',library = "fa")
        icon.exe     <- makeAwesomeIcon(icon = "fa-pagelines",markerColor = "darkpurple",iconColor = 'black',library = "fa")
        icon.tecn    <- makeAwesomeIcon(icon = "glyphicon glyphicon-phone",markerColor = "cadetblue",iconColor = 'black')
        icon.turis   <- makeAwesomeIcon(icon = "glyphicon glyphicon-plane",markerColor = "white",iconColor = 'black')
        icon.vehi    <- makeAwesomeIcon(icon = "fa-paper-plane-o",markerColor = "lightgray",iconColor = 'black',
                                        library = "fa")
        icon.vest    <- makeAwesomeIcon(icon = "fa-female",markerColor = "darkgreen",iconColor = 'black',
                                        library = "fa")
        
        output$map              <-  renderLeaflet({
                
                
                coord <- tabla2() %>%left_join(select(infraestructuras,NOMBRE,CX,CY),by=c("infraestructura"="NOMBRE")) %>%
                        dplyr::filter(razon_social==input$razonsocial|prospecto==input$razonsocial)
                
                
                SUPERMERCADO     <- coord  %>% dplyr::filter(TIPO=="SUPERMERCADO")
                CLUB             <- coord  %>% dplyr::filter(TIPO=="CLUB")
                SALUD            <- coord  %>% dplyr::filter(TIPO=="SALUD")
                VIVIENDA         <- coord  %>% dplyr::filter(TIPO=="VIVIENDA")
                DROGUERIA        <- coord  %>% dplyr::filter(TIPO=="DROGUERIA")
                EDUCACION        <- coord  %>% dplyr::filter(TIPO=="COLEGIO"|TIPO=="INSTITUTO")
                CSC              <- coord  %>% dplyr::filter(TIPO=="CSC"|TIPO=="SERVICIO")
                RECREACION       <- coord  %>% dplyr::filter(TIPO=="HOTEL"|TIPO=="PARQUE"|TIPO=="RECREACION")
                
                map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 9, maxZoom = 50)) %>%
                        #addProviderTiles(providers$Stamen.TonerLite) %>%
                        setView(lat=4.6477908, lng = -74.108419, zoom = 11) %>% addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google')
                map <-  addCircles(map,data= coord[1,], lat =~CY.x , popup=~razon_social,lng =~CX.x,radius = input$distancia*1000) %>% 
                        addMarkers(data=coord[1,], lng =~CX.x, lat =~CY.x, icon= leafIconsCS2, popup = ~as.character(razon_social),label = ~as.character(paste(razon_social)))
                
                if (dim(SUPERMERCADO)[1]!=0) {
                        map <- addMarkers(map, data=SUPERMERCADO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                          icon = leafIconsSP,group = "Supermercados")}
                
                if (dim(CLUB)[1]!=0) {
                        map <- addMarkers(map,data=CLUB, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                          icon = leafIconsCUBO,group = "Clubes")}
                
                if (dim(SALUD)[1]!=0) {
                        map <- addMarkers(map,data= SALUD, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                          icon = leafIconsSL,group = "Salud")}
                
                if (dim(VIVIENDA)[1]!=0) {
                        map <- addMarkers(data= VIVIENDA, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                          icon = leafIconsVV,group = "Vivienda")}
                
                if (dim(DROGUERIA)[1]!=0) {
                        map <- addMarkers(map,data= DROGUERIA, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                          icon = leafIconsDR,group = "Drogueria")}
                
                if (dim(EDUCACION)[1]!=0) {
                        map <- addMarkers(map,data= EDUCACION, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                          icon = leafIconsED,group = "Educacion")}
                
                if (dim(CSC)[1]!=0){
                        map <- addMarkers(map,data= CSC, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                          icon = leafIconsCS,group = "Centros de Servicio")}
                
                if (dim(RECREACION)[1]!=0){
                        map <- addMarkers(map,data= RECREACION, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                          icon = leafIconsRYT,group = "Recreacion")}
                
                map <- map %>% addLayersControl(overlayGroups = c("Supermercados","Clubes","Salud", "Vivienda","Drogueria","Educacion","Centros de Servicio","Recreacion" 
                ), options = layersControlOptions(collapsed = T), position = "bottomright") %>% 
                        hideGroup(group = c("Supermercados","Clubes","Salud", "Vivienda","Drogueria","Educacion","Centros de Servicio","Recreacion" 
                        ))
                
                map
        })
        output$map.convenio.afi <-  renderLeaflet({
                coord2 <- tabla3() %>%left_join(select(convenios,RSCOD,CX,CY),by=c("infraestructura"="RSCOD")) %>%
                        dplyr::filter(razon_social==input$razonsocial|prospecto==input$razonsocial)

                # Convenios

                ALIMENTOS_BEBIDAS          <- coord2  %>% dplyr::filter(CATEGORIA=="ALIMENTOS Y BEBIDAS")
                ALMACENES_DEPARTAMENTO     <- coord2  %>% dplyr::filter(CATEGORIA=="ALMACENES POR DEPARTAMENTO")
                CALZADO                    <- coord2  %>% dplyr::filter(CATEGORIA=="CALZADO")
                DEPORTES                   <- coord2  %>% dplyr::filter(CATEGORIA=="DEPORTES")
                DIVERSION                  <- coord2  %>% dplyr::filter(CATEGORIA=="DIVERSION Y ENTRETENIMIENTO")
                EDUCACION_CON              <- coord2  %>% dplyr::filter(CATEGORIA=="EDUCACION")
                ELECTRO                    <- coord2  %>% dplyr::filter(CATEGORIA=="ELECTRODOMESTICOS")
                ESPECIALIZADOS             <- coord2  %>% dplyr::filter(CATEGORIA=="ESPECIALIZADOS")
                HOGAR                      <- coord2  %>% dplyr::filter(CATEGORIA=="HOGAR Y DECORACION")
                MASCOTAS                   <- coord2  %>% dplyr::filter(CATEGORIA=="MASCOTAS")
                SALUD_BIENESTAR            <- coord2  %>% dplyr::filter(CATEGORIA=="SALUD Y BIENESTAR")
                SEGUROS                    <- coord2  %>% dplyr::filter(CATEGORIA=="SEGUROS")
                EXEQUIALES                 <- coord2  %>% dplyr::filter(CATEGORIA=="SERVICIOS EXCEQUIALES")
                TECNOLOGIA                 <- coord2  %>% dplyr::filter(CATEGORIA=="TECNOLOGIA")
                TURISMO                    <- coord2  %>% dplyr::filter(CATEGORIA=="TURISMO")
                VEHICULOS                  <- coord2  %>% dplyr::filter(CATEGORIA=="VEHICULOS Y MOTOS")
                VESTUARIO                  <- coord2  %>% dplyr::filter(CATEGORIA=="VESTUARIO")

                map3 <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 9, maxZoom = 50)) %>%
                        #addProviderTiles(providers$Stamen.TonerLite) %>%
                        setView(lat=4.6477908, lng = -74.108419, zoom = 11) %>% addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google')
                map3 <-  addCircles(map3,data= coord2[1,], lat =~CY.y, popup=~razon_social,lng =~CX.y,radius = input$distancia*1000,color = "red") %>%
                        addMarkers(data=coord2[1,], lng =~CX.y, lat =~CY.y, icon= leafIconsCS2, popup = ~as.character(razon_social),label = ~as.character(paste(razon_social)))



                if (dim(ALIMENTOS_BEBIDAS)[1]!=0){
                        map3 <- addMarkers(map3, data=ALIMENTOS_BEBIDAS, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.ali,group = "Alimentos y bebidas")}

                if (dim(ALMACENES_DEPARTAMENTO)[1]!=0){
                        map3 <- addMarkers(map3,data=ALMACENES_DEPARTAMENTO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.alm,group = "Almacenes por dpto")}

                if (dim(CALZADO )[1]!=0) {
                        map3 <- addMarkers(map3,data=CALZADO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.calz,group = "Calzado")}

                if (dim(DEPORTES )[1]!=0) {
                        map3 <- addMarkers(map3,data=DEPORTES, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.depor,group = "Deportes")}

                if (dim(DIVERSION)[1]!=0) {
                        map3 <- addMarkers(map3,data=DIVERSION, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.diver,group = "Diversion")}

                if (dim(EDUCACION_CON)[1]!=0) {
                        map3 <- addMarkers(map3,data=EDUCACION_CON, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.educcon,group = "Educacion")}

                if (dim(ELECTRO)[1]!=0) {
                        map3 <- addMarkers(map3,data=ELECTRO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.electro,group = "Electrodomesticos")}

                if (dim(ESPECIALIZADOS)[1]!=0) {
                        map3 <- addMarkers(map3,data=ESPECIALIZADOS, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.espe,group = "Especializados")}

                if (dim(HOGAR)[1]!=0) {
                        map3 <- addMarkers(map3,data=HOGAR, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.hogar,group = "Hogar")}

                if (dim(MASCOTAS)[1]!=0) {
                        map3 <- addMarkers(map3,data=MASCOTAS, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.masco,group = "Mascotas")}

                if (dim(SALUD_BIENESTAR)[1]!=0) {
                        map3 <- addMarkers(map3,data=SALUD_BIENESTAR, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.sabie,group = "Salud y bienestar")}

                if (dim(SEGUROS)[1]!=0) {
                        map3 <- addMarkers(map3,data=SEGUROS, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.segu,group = "Seguros")}

                if (dim(EXEQUIALES)[1]!=0) {
                        map3 <- addMarkers(map3,data=EXEQUIALES, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.exe,group = "Exequiales")}

                if (dim(TECNOLOGIA)[1]!=0) {
                        map3 <- addMarkers(map3,data=TECNOLOGIA, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.tecn,group = "Tecnologia")}

                if (dim(TURISMO)[1]!=0) {
                        map3 <- addMarkers(map3,data=TURISMO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.turis,group = "Turismo")}

                if (dim(VEHICULOS)[1]!=0) {
                        map3 <- addMarkers(map3,data=VEHICULOS, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.vehi,group = "Vehiculos")}

                if (dim(VESTUARIO)[1]!=0) {
                        map3 <- addMarkers(map3,data=VESTUARIO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                           icon = icon.vehi,group = "Vestuario")}


                map3 <- map3 %>% addLayersControl(overlayGroups = c("Alimentos y bebidas", "Almacenes por dpto","Calzado","Deportes","Diversion",
                                                                    "Educacion","Electrodomesticos","Especializados","Hogar","Mascotas","Salud y bienestar",
                                                                    "Seguros","Exequiales", "Tecnologia","Turismo","Vehiculos","Vestuario"
                ), options = layersControlOptions(collapsed = T), position = "bottomright") %>%
                        hideGroup(group = c("Alimentos y bebidas", "Almacenes por dpto","Calzado","Deportes","Diversion",
                                            "Educacion","Electrodomesticos","Especializados","Hogar","Mascotas","Salud y bienestar",
                                            "Seguros","Exequiales", "Tecnologia","Turismo","Vehiculos","Vestuario"

                        ))

                map3
        })

        # Empresas no afiliadas ---------------------------------------------------
        
        #reactivos
        
        tabla_no                   <-  shiny::reactive({

                afiliados_piramide   <- function(matrix,radio= input$distancia2,empresas=1){

                        resultado <- NULL

                        for(kk in 1:empresas){

                                index   <- which(matrix[kk,]<= radio)

                                dt      <- as.data.frame(table(infraestructuras$TIPO[index]))

                                index.2 <- which(levels(as.factor(infraestructuras$TIPO))%in%dt$Var1)

                                if(length(index)==0){
                                        dt2 <- data.frame(Var1=levels(as.factor(infraestructuras$TIPO)),Freq=0)

                                }else{
                                        dt2 <- data.frame(Var1=levels(as.factor(infraestructuras$TIPO))[-index.2],Freq=0)

                                }

                                conteo  <- rbind(dt,dt2)
                                conteo  <- spread(conteo, Var1, Freq)
                                conteo  <- conteo[ , order(names(conteo))]

                                resultado  <- rbind(resultado,conteo)

                        }
                        rownames(resultado) <-  rownames(matrix)[1:empresas]
                        resultado$TOTAL     <-  rowSums(resultado)

                        return(resultado)

                }
                tabla_no <- afiliados_piramide(matrix= matrix_alm_no_full(),radio = input$distancia2,empresas=1)
                tabla_no <- tabla_no %>% dplyr::mutate(empresas=row.names(tabla_no))
                tabla_no <- tabla_no %>% dplyr::left_join(dplyr::select(noafiliadas(),razon_social,id_empresa,piramide_1,piramide_2),
                                                  by = c("empresas"="id_empresa")) %>%
                        dplyr::select(empresas,razon_social,piramide_1,piramide_2,CLUB:TOTAL)
        })
        tabla2.no                  <-  shiny::reactive({

                prospecto <- function(matrix,radio=input$distancia2,empresas=1){

                        dt <-NULL
                        for(kk in 1:empresas){

                                index                  <- which(matrix[kk,]<= radio)
                                infra                  <- names(matrix)[index]
                                prospecto              <- rownames(matrix)[kk]

                                if(length(index)==0){
                                        consolidado          <- data.frame(prospecto,infraestructura=NA)
                                        un_list              <- rbind.data.frame(0)

                                }else{
                                        consolidado          <- data.frame(prospecto,infraestructura=infra)
                                        dist                 <- c(matrix[kk, index])
                                        un_list              <- rbind.data.frame(dist)

                                }
                                colnames(un_list)    <- "Distancia"

                                consolidado$distancia <- un_list$Distancia
                                row_order <- order(consolidado$distancia, decreasing = FALSE)
                                consolidado <- consolidado[row_order, , drop = FALSE]

                                if(length(index)==0){
                                        consolidado$orden_de_cercania <- 1
                                }else{
                                        consolidado$orden_de_cercania <- 1:length(index)
                                }
                                dt          <- rbind(dt,consolidado)
                                dt$prospecto       <- as.character(dt$prospecto)
                                dt$infraestructura <- as.character(dt$infraestructura)
                        }
                        return(dt)
                }


                tabla2_no <- prospecto (matrix= matrix_alm_no_full(),radio = input$distancia2)
                tabla2_no <- tabla2_no %>%left_join(select(noafiliadas(),razon_social,id_empresa,piramide_1,piramide_2,CX,CY),
                                                    by = c("prospecto"="id_empresa"))
                tabla2_no <- tabla2_no %>% left_join(select(infraestructuras,NOMBRE,TIPO),by=c("infraestructura"="NOMBRE"))
                tabla2_no <- tabla2_no %>% select(-distancia) %>% dplyr::filter(infraestructura!=0) %>%
                        select(prospecto,razon_social,piramide_1,piramide_2,infraestructura,TIPO,orden_de_cercania,CX,CY)


        })
        tabla3.no                  <-  shiny::reactive({

                afiliados_piramide   <- function(matrix,radio= input$distancia2,empresas=1){

                        resultado <- NULL

                        for(kk in 1:empresas){

                                index   <- which(matrix[kk,]<= radio)

                                dt     <- as.data.frame(table(convenios$CATEGORIA[index]))

                                index.2 <- which(levels(as.factor(convenios$CATEGORIA))%in%dt$Var1)

                                if(length(index)==0){
                                        dt2 <- data.frame(Var1=levels(as.factor(convenios$CATEGORIA)),Freq=0)

                                }else{
                                        dt2 <- data.frame(Var1=levels(as.factor(convenios$CATEGORIA))[-index.2],Freq=0)

                                }

                                conteo  <- rbind(dt,dt2)
                                conteo  <- spread(conteo, Var1, Freq)
                                conteo  <- conteo[ , order(names(conteo))]

                                resultado  <- rbind(resultado,conteo)

                        }
                        rownames(resultado) <-  rownames(matrix)[1:empresas]
                        resultado$TOTAL     <-  rowSums(resultado)

                        return(resultado)

                }
                tabla_no <- afiliados_piramide(matrix= matrix_convenio_no_afi(),radio = input$distancia2)
                tabla_no <- tabla_no%>% dplyr::mutate(empresas=row.names(tabla_no))
                tabla_no <- tabla_no %>%dplyr::left_join(dplyr::select(noafiliadas(),razon_social,id_empresa,piramide_1,piramide_2),
                                                  by = c("empresas"="id_empresa"))%>%
                        dplyr::select(empresas,razon_social,piramide_1,piramide_2,`ALIMENTOS Y BEBIDAS`:TOTAL)
        })
        tabla4.no                  <-  shiny::reactive({

                prospecto <- function(matrix,radio=input$distancia2,empresas=1){

                        dt <-NULL
                        for(kk in 1:empresas){

                                index                  <- which(matrix[kk,]<= radio)
                                infra                  <- names(matrix)[index]
                                prospecto              <- rownames(matrix)[kk]

                                if(length(index)==0){
                                        consolidado          <- data.frame(prospecto,infraestructura=NA)
                                        un_list              <- rbind.data.frame(0)

                                }else{
                                        consolidado          <- data.frame(prospecto,infraestructura=infra)
                                        dist                 <- c(matrix[kk, index])
                                        un_list              <- rbind.data.frame(dist)

                                }
                                colnames(un_list)    <- "Distancia"

                                consolidado$distancia <- un_list$Distancia
                                row_order <- order(consolidado$distancia, decreasing = FALSE)
                                consolidado <- consolidado[row_order, , drop = FALSE]

                                if(length(index)==0){
                                        consolidado$orden_de_cercania <- 1
                                }else{
                                        consolidado$orden_de_cercania <- 1:length(index)
                                }
                                dt          <- rbind(dt,consolidado)
                                dt$prospecto       <- as.character(dt$prospecto)
                                dt$infraestructura <- as.character(dt$infraestructura)
                        }
                        return(dt)
                }


                tabla4_no <- prospecto (matrix= matrix_convenio_no_afi(),radio = input$distancia2)
                tabla4_no <- tabla4_no %>%left_join(select(noafiliadas(),razon_social,id_empresa,piramide_1,piramide_2,CX,CY),
                                                    by = c("prospecto"="id_empresa"))

                tabla4_no <- tabla4_no %>% left_join(select(convenios,RSCOD,CATEGORIA),by=c("infraestructura"="RSCOD"))
                tabla4_no <- tabla4_no %>% select(-distancia) %>% dplyr::filter(infraestructura!=0) %>%
                        select(prospecto,razon_social,piramide_1,piramide_2,infraestructura,CATEGORIA,orden_de_cercania,CX,CY) %>%
                        distinct()





        })
        # 
        # Salidas tablas empresas no afiliadas
        output$tablano             <-  renderDataTable({
                tabla_no() %>% dplyr::filter(empresas==input$razonsocial2|razon_social==input$razonsocial2) %>%
                        DT::datatable(options = list(scrollX="600px",scrollY="90px",pageLength = 5,paging = T,
                                                     initComplete = JS(
                                                             "function(settings, json) {",
                                                             "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                                                             "}")),
                                       colnames = c("NIT", "RAZON SOCIAL","PIRAMIDE 1","PIRAMIDE 2","CENTROS DE SERVICIO","DROGUERIA",
                                                    "HOTEL","INSTITUTO","PARQUE","RECREACION","SALUD","SERVICIO","SUPERMERCADO","VIVIENDA",
                                                    "TOTAL")
                        )

        })
        output$csc.no              <-  DT::renderDataTable({

                tabla2.no() %>%select(-CX,-CY) %>% dplyr::filter(razon_social==input$razonsocial2|prospecto==input$razonsocial2) %>%  DT::datatable(options = list(pageLength = 10,paging = T,
                                                                                                                                                       initComplete = JS(
                                                                                                                                                               "function(settings, json) {",
                                                                                                                                                               "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                                                                                                                                                               "}")),
                                                                                                                                        colnames = c("NIT", "RAZON SOCIAL","PIRAMIDE 1","PIRAMIDE 2","INFRAESTRUCTURA","TIPO","ORDEN DE CERCANIA")
                                                                                                                                        )

        })
        output$tabla3.no           <-  DT::renderDataTable({
                tabla3.no() %>% dplyr::filter(razon_social==input$razonsocial2|empresas==input$razonsocial2) %>%
                        rename(NIT=empresas) %>%
                        DT::datatable(


                                options = list(scrollX="600px",scrollY="80px",pageLength = 5,paging = T,
                                               initComplete = JS(
                                                       "function(settings, json) {",
                                                       "$(this.api().table().header()).css({'background-color': '#E82C23', 'color': '#fff'});",
                                                       "}")))


        })
        output$tabla4.no           <-  DT::renderDataTable({
                tabla4.no() %>%select(-CX,-CY) %>% dplyr::filter(razon_social==input$razonsocial2|prospecto==input$razonsocial2) %>% DT::datatable(options = list(pageLength = 5,paging = T,
                                                                                                                                                      initComplete = JS(
                                                                                                                                                              "function(settings, json) {",
                                                                                                                                                              "$(this.api().table().header()).css({'background-color': '#E82C23', 'color': '#fff'});",
                                                                                                                                                              "}")),
                                                                                                                                       colnames = c("NIT", "RAZON SOCIAL","PIRAMIDE 1","PIRAMIDE 2","CONVENIO","CATEGORIA","ORDEN DE CERCANIA")

                )
        })
        

        ## Salidas mapas no afiliados
        
        output$map.no              <-  renderLeaflet({

                coord <- tabla2.no() %>%left_join(select(infraestructuras,NOMBRE,CX,CY),by=c("infraestructura"="NOMBRE")) %>%
                        dplyr::filter(razon_social==input$razonsocial2|prospecto==input$razonsocial2)

                SUPERMERCADO     <- coord  %>% dplyr::filter(TIPO=="SUPERMERCADO")
                CLUB             <- coord  %>% dplyr::filter(TIPO=="CLUB")
                SALUD            <- coord  %>% dplyr::filter(TIPO=="SALUD")
                VIVIENDA         <- coord  %>% dplyr::filter(TIPO=="VIVIENDA")
                DROGUERIA        <- coord  %>% dplyr::filter(TIPO=="DROGUERIA")
                EDUCACION        <- coord  %>% dplyr::filter(TIPO=="COLEGIO"|TIPO=="INSTITUTO")
                CSC              <- coord  %>% dplyr::filter(TIPO=="CSC"|TIPO=="SERVICIO")
                RECREACION       <- coord  %>% dplyr::filter(TIPO=="HOTEL"|TIPO=="PARQUE"|TIPO=="RECREACIËN")


                map.no <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 9, maxZoom = 50)) %>%
                        #addProviderTiles(providers$Stamen.TonerLite) %>%
                        setView(lat=4.6477908, lng = -74.108419, zoom = 11) %>% addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google')
                map.no <- addCircles(map.no,data= coord[1,], lat =~CY.x , popup=~razon_social,lng =~CX.x,radius = input$distancia*1000,color = "red") %>%
                        addMarkers(data=coord[1,], lng =~CX.x, lat =~CY.x, icon= leafIconsCS2, popup = ~as.character(razon_social),label = ~as.character(paste(razon_social)))

                if (dim(SUPERMERCADO)[1]!=0) {
                        map.no <- addMarkers(map.no, data=SUPERMERCADO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = leafIconsSP,group = "Supermercados")}

                if (dim(CLUB)[1]!=0) {
                        map.no <- addMarkers(map.no,data=CLUB, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = leafIconsCUBO,group = "Clubes")}

                if (dim(SALUD)[1]!=0) {
                        map.no <- addMarkers(map.no,data= SALUD, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = leafIconsSL,group = "Salud")}

                if (dim(VIVIENDA)[1]!=0) {
                        map.no <- addMarkers(map.no,data= VIVIENDA, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = leafIconsVV,group = "Vivienda")}

                if (dim(DROGUERIA)[1]!=0) {
                        map.no <- addMarkers(map.no,data= DROGUERIA, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = leafIconsDR,group = "Drogueria")}

                if (dim(EDUCACION)[1]!=0) {
                        map.no <- addMarkers(map.no,data= EDUCACION, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = leafIconsED,group = "Educacion")}

                if (dim(CSC)[1]!=0){
                        map.no <- addMarkers(map.no,data= CSC, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = leafIconsCS,group = "Centros de Servicio")}

                if (dim(RECREACION)[1]!=0){
                        map.no <- addMarkers(map.no,data= RECREACION, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = leafIconsRYT,group = "Recreacion")}
                map.no

                map.no <- map.no %>% addLayersControl(overlayGroups = c("Supermercados","Clubes","Salud", "Vivienda","Drogueria","Educacion","Centros de Servicio","Recreación"
                ), options = layersControlOptions(collapsed = T), position = "bottomright") %>%
                        hideGroup(group = c("Supermercados","Clubes","Salud", "Vivienda","Drogueria","Educacion","Centros de Servicio","Recreación"
                        ))

                map.no




        })
        output$map.convenio.no_afi <-  renderLeaflet({
                coord3 <- tabla4.no() %>%left_join(select(convenios,RSCOD,CX,CY),by=c("infraestructura"="RSCOD")) %>%
                        dplyr::filter(razon_social==input$razonsocial2|prospecto==input$razonsocial2)

                # Convenios

                ALIMENTOS_BEBIDAS          <- coord3  %>% dplyr::filter(CATEGORIA=="ALIMENTOS Y BEBIDAS")
                ALMACENES_DEPARTAMENTO     <- coord3  %>% dplyr::filter(CATEGORIA=="ALMACENES POR DEPARTAMENTO")
                CALZADO                    <- coord3  %>% dplyr::filter(CATEGORIA=="CALZADO")
                DEPORTES                   <- coord3  %>% dplyr::filter(CATEGORIA=="DEPORTES")
                DIVERSION                  <- coord3  %>% dplyr::filter(CATEGORIA=="DIVERSION Y ENTRETENIMIENTO")
                EDUCACION_CON              <- coord3  %>% dplyr::filter(CATEGORIA=="EDUCACION")
                ELECTRO                    <- coord3  %>% dplyr::filter(CATEGORIA=="ELECTRODOMESTICOS")
                ESPECIALIZADOS             <- coord3  %>% dplyr::filter(CATEGORIA=="ESPECIALIZADOS")
                HOGAR                      <- coord3  %>% dplyr::filter(CATEGORIA=="HOGAR Y DECORACION")
                MASCOTAS                   <- coord3  %>% dplyr::filter(CATEGORIA=="MASCOTAS")
                SALUD_BIENESTAR            <- coord3  %>% dplyr::filter(CATEGORIA=="SALUD Y BIENESTAR")
                SEGUROS                    <- coord3  %>% dplyr::filter(CATEGORIA=="SEGUROS")
                EXEQUIALES                 <- coord3  %>% dplyr::filter(CATEGORIA=="SERVICIOS EXCEQUIALES")
                TECNOLOGIA                 <- coord3  %>% dplyr::filter(CATEGORIA=="TECNOLOGIA")
                TURISMO                    <- coord3  %>% dplyr::filter(CATEGORIA=="TURISMO")
                VEHICULOS                  <- coord3  %>% dplyr::filter(CATEGORIA=="VEHICULOS Y MOTOS")
                VESTUARIO                  <- coord3  %>% dplyr::filter(CATEGORIA=="VESTUARIO")

                map3no <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 9, maxZoom = 50)) %>%
                        #addProviderTiles(providers$Stamen.TonerLite) %>%
                        setView(lat=4.6477908, lng = -74.108419, zoom = 11) %>% addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google')
                map3no <-  addCircles(map3no,data= coord3[1,], lat =~CY.y, popup=~razon_social,lng =~CX.y,radius = input$distancia*1000,color = "red") %>%
                        addMarkers(data=coord3[1,], lng =~CX.y, lat =~CY.y, icon= leafIconsCS2, popup = ~as.character(razon_social),label = ~as.character(paste(razon_social)))



                if (dim(ALIMENTOS_BEBIDAS)[1]!=0){
                        map3no <- addMarkers(map3no, data=ALIMENTOS_BEBIDAS, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.ali,group = "Alimentos y bebidas")}

                if (dim(ALMACENES_DEPARTAMENTO)[1]!=0){
                        map3no <- addMarkers(map3no,data=ALMACENES_DEPARTAMENTO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.alm,group = "Almacenes por dpto")}

                if (dim(CALZADO )[1]!=0) {
                        map3no <- addMarkers(map3no,data=CALZADO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.calz,group = "Calzado")}

                if (dim(DEPORTES )[1]!=0) {
                        map3no <- addMarkers(map3no,data=DEPORTES, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.depor,group = "Deportes")}

                if (dim(DIVERSION)[1]!=0) {
                        map3no <- addMarkers(map3no,data=DIVERSION, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.diver,group = "Diversion")}

                if (dim(EDUCACION_CON)[1]!=0) {
                        map3no <- addMarkers(map3no,data=EDUCACION_CON, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.educcon,group = "Educacion")}

                if (dim(ELECTRO)[1]!=0) {
                        map3no <- addMarkers(map3no,data=ELECTRO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.electro,group = "Electrodomesticos")}

                if (dim(ESPECIALIZADOS)[1]!=0) {
                        map3no <- addMarkers(map3no,data=ESPECIALIZADOS, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.espe,group = "Especializados")}

                if (dim(HOGAR)[1]!=0) {
                        map3no <- addMarkers(map3no,data=HOGAR, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.hogar,group = "Hogar")}

                if (dim(MASCOTAS)[1]!=0) {
                        map3no <- addMarkers(map3no,data=MASCOTAS, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.masco,group = "Mascotas")}

                if (dim(SALUD_BIENESTAR)[1]!=0) {
                        map3no <- addMarkers(map3no,data=SALUD_BIENESTAR, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.sabie,group = "Salud y bienestar")}

                if (dim(SEGUROS)[1]!=0) {
                        map3no <- addMarkers(map3no,data=SEGUROS, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.segu,group = "Seguros")}

                if (dim(EXEQUIALES)[1]!=0) {
                        map3no <- addMarkers(map3no,data=EXEQUIALES, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.exe,group = "Exequiales")}

                if (dim(TECNOLOGIA)[1]!=0) {
                        map3no <- addMarkers(map3no,data=TECNOLOGIA, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.tecn,group = "Tecnologia")}

                if (dim(TURISMO)[1]!=0) {
                        map3no <- addMarkers(map3no,data=TURISMO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.turis,group = "Turismo")}

                if (dim(VEHICULOS)[1]!=0) {
                        map3no <- addMarkers(map3no,data=VEHICULOS, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.vehi,group = "Vehiculos")}

                if (dim(VESTUARIO)[1]!=0) {
                        map3no <- addMarkers(map3no,data=VESTUARIO, lng =~CX.y, lat =~CY.y, popup = ~as.character(infraestructura),label = ~as.character(paste(infraestructura)),
                                             icon = icon.vehi,group = "Vestuario")}


                map3no <- map3no %>% addLayersControl(overlayGroups = c("Alimentos y bebidas", "Almacenes por dpto","Calzado           ","Deportes","Diversion",
                                                                        "Educacion","Electrodomesticos","Especializados","Hogar","Mascotas","Salud y bienestar",
                                                                        "Seguros","Exequiales", "Tecnologia","Turismo","Vehiculos","Vestuario"
                ), options = layersControlOptions(collapsed = T), position = "bottomright") %>%
                        hideGroup(group = c("Alimentos y bebidas", "Almacenes por dpto","Calzado","Deportes","Diversion",
                                            "Educacion","Electrodomesticos","Especializados","Hogar","Mascotas","Salud y bienestar",
                                            "Seguros","Exequiales", "Tecnologia","Turismo","Vehiculos","Vestuario"

                        ))

                map3no
        })


        output$prueba    <-  DT::renderDataTable({

                 tabla2() %>%
                         DT::datatable()

         })
        output$default <- renderText({ "todo ok" })
          

        
        
        
})
