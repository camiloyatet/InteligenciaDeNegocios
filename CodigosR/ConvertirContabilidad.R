# Convierte una cadena de carateres con formato contabilidad en número

convertir.contabilidad <- function(x){
  if(grepl("\\(.*\\)", x)){
    as.numeric(paste0("-", gsub("\\(|\\)", "", gsub("[\\$, ]", "", x))))
  } else {
    as.numeric(gsub("[\\$, ]", "", x))
  }
}
