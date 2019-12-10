library(tictoc)

### Modelo Lineal -----

tic("Extraccion de Estadisticas")
Resultado <- data.frame(
  prod_id=numeric(),
  Regional=character(),
  Elasticidad=character(),
  Betta=numeric(),
  PrecioMinimo=numeric(),
  PrecioMaximo=numeric(),
  PrecioOptimo=numeric(),
  stringsAsFactors = F
)

prods <- unique(data$prod_id)

for(i in 1:length(prods)){
  print(paste("Iteracion", i, "de", length(prods)))
  tmp1 <- data %>% filter(prod_id==prods[i])
  regs <- unique(tmp1$Regional)
    for(j in 1:length(regs)){
      tmp2 <- tmp1 %>% filter(Regional==regs[j])
      if(nrow(tmp2)>3){
        tryCatch({
          optimos <- optimize_one_product_lmer(data = tmp2, factor_spend = 2)
          PrecioOptimo= optimos$opt$maximum
          b <- abs(summary(optimos$model)$coefficients[which(rownames(summary(optimos$model)$coefficients) == "BASE_PRICE"), 1])
          Elasticidad=ifelse(b < 1, "Inelastica", "Elastica")
          Resultado <- bind_rows(
            Resultado,
            data.frame(prod_id=prods[i],
                       Regional=regs[j],
                       Elasticidad=Elasticidad,
                       Betta=b,
                       PrecioMinimo=min(tmp2$PRICE),
                       PrecioMaximo=round((max(tmp2$BASE_PRICE) * 1.3),0),
                       PrecioOptimo=PrecioOptimo,
                       stringsAsFactors = F)
            )
            }, error=function(e){})
      }
    }
  }
toc()

### Pruebas de Tukey ----

tic("Extraccion de Tukey")
Tukey <- data.frame(
  prod_id=numeric(),
  Regional=character(),
  Dif=character(),
  Dif_Cant=numeric(),
  stringsAsFactors = F
)

prods <- unique(data$prod_id)

for(i in 1:length(prods)){
  print(paste("Iteracion", i, "de", length(prods)))
  tmp1 <- data %>% filter(prod_id==prods[i])
  regs <- unique(tmp1$Regional)
  for(j in 1:length(regs)){
    tmp2 <- tmp1 %>% filter(Regional==regs[j]) %>% 
      select(UNITS, FEATURE) %>% 
      mutate(FEATURE=as.factor(FEATURE))
    tryCatch({
      m1 <- lm(UNITS~FEATURE, data = tmp2)
      hsd <- HSD.test(m1, "FEATURE")
      Dif=ifelse(n_distinct(hsd$groups$groups)==2,"Diferente", "Indiferente")
      Cant=hsd$means[2,1]-hsd$means[1,1]
      Tukey <- bind_rows(
        Tukey,
        data.frame(prod_id=prods[i],
                   Regional=regs[j],
                   Dif=Dif,
                   Dif_Cant=Cant,
                   stringsAsFactors = F)
        )
        }, error=function(e){})
  }
}
toc()


### Consolidacion ----
Resultados <- data %>% 
  select(1:13) %>% 
  distinct() %>% 
  left_join(Resultado, by=c("prod_id","Regional")) %>% 
  left_join(Tukey, by=c("prod_id","Regional"))

### Exportacion ----
saveRDS(Resultados, "DashoardElasticidades/data/Resultados.rds")
rm(list = ls(all=T))
