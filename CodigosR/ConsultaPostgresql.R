library(DBI)
library(RPostgreSQL)

db_driver <- dbDriver("PostgreSQL")

db_connection <- dbConnect(db_driver, 
                           dbname = "DATA_BASE_NAME",
                           host = "HOST", 
                           port = 5432,
                           password="PASSWORD",
                           user = "USERNAME")

# ---- Consultas ejemplo ---
Consulta1 <- dbGetQuery(db_connection,'SELECT *	FROM "SUPERMERCADO"."SUPERMERCADO_CONSUMO" limit 10;')
dbSendQuery(db_connection,'DELETE FROM "SUPERMERCADO"."SUPERMERCADO_CONSUMO"')
qry = 'INSERT into "SUPERMERCADO"."SUPERMERCADO_CONSUMO" ("ID_CATEGORIA", "DESC_CATEGORIA", "ID_PRODUCTO", "DESC_PRODUCTO", "ID_PROVEEDOR", "DESC_PROVEEDOR", "ID_TRANSACCION", "CONSUMO", "TRANSACCION", "SUCURSAL", "FECHA", "HORA", "DESC_CATEGORIA2", "DOC_CLIENTE", "DESCUENTO", "ID_PERSONA") VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16);'
for (x in 1:nrow(Consumo)) {
  rs <- dbSendQuery(db_connection, qry, c(Consumo$ID_CATEGORIA[x], Consumo$DESC_CATEGORIA[x], Consumo$ID_PRODUCTO[x], Consumo$DESC_PRODUCTO[x], Consumo$ID_PROVEEDOR[x], Consumo$DESC_PROVEEDOR[x], Consumo$ID_TRANSACCION[x], Consumo$CONSUMO[x], Consumo$TRANSACCION[x], Consumo$SUCURSAL[x],Consumo$FECHA[x], Consumo$HORA[x], Consumo$DESC_CATEGORIA2[x] , Consumo$DOC_CLIENTE[x], Consumo$DESCUENTO[x] , Consumo$ID_PERSONA[x]))
  dbClearResult(rs)
}

dbDisconnect(db_connection)
dbUnloadDriver(db_driver)