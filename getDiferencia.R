getDiferencia <- function(puntuacion, relPib, referencia)
{
  cociente <- relPib/puntuacion
  retorno  <- round(cociente-referencia, digits=3)
  
  return(retorno)
}