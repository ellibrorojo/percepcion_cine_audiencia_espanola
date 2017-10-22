getPibAño <- function(tablaPib, año)
{
  return(tablaPib$Valor[which(tablaPib$Año == año)])
}