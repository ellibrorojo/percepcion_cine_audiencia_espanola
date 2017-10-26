lookupPaises <- function(diccionarioEs, diccionarioEn, pais)
{
  country <- diccionarioEn$Nombre[which(diccionarioEn$Código == diccionarioEs$Código[which(tolower(diccionarioEs$Nombre) == tolower(pais))])]
  
  if (country == "United States of America")
    country <- "United States"
  else
  if (country == "United Kingdom of Great Britain and Northern Ireland")
    country <- "United Kingdom"
  
  return(country)
}