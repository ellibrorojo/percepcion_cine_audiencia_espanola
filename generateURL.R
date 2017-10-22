generateURL <- function (año)
{
  añoActual <- as.numeric(format(Sys.Date(), "%Y"))
  
  if (añoActual-año >= 4)
    url <- as.character(gsub('%s',as.character(año),"http://www.mecd.gob.es/cultura-mecd/areas-cultura/cine/mc/cdc/anos-anteriores/ano-%s/cine-peliculas-recaudacion.html"))
  else
    url <- as.character(gsub('%s',as.character(año),"http://www.mecd.gob.es/cultura-mecd/areas-cultura/cine/mc/cdc/ano-%s/c/cine-peliculas-recaudacion.html"))
  
  if (año == 2013)
    url <- "http://www.mecd.gob.es/cultura-mecd/areas-cultura/cine/mc/cdc/anos-anteriores/ano-2013/c/cine-peliculas-recaudacion.html"
  
  return(url)
}