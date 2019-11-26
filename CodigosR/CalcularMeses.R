# Calcula el numero de meses entre dos fechas

Calcuar.Meses <- function(from, to) {
  sd <- as.POSIXlt(from)
  ed <- as.POSIXlt(to)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}
