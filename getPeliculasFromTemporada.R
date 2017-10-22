getPeliculasFromTemporada <- function(año)
{
  url <- generateURL(año)
  table <- url %>% read_html() %>%  html_nodes(xpath='//*[@id=\"info\"]/div[2]/table') %>% html_table()
  
  retorno <- as.data.frame(table[1])
  retorno$Año <- año
  retorno$Recaudación <- gsub("\\.", "", retorno$Recaudación)
  retorno$Recaudación <- gsub(",", ".", retorno$Recaudación)
  retorno$RelPib <- round(as.numeric(retorno$Recaudación)/(getPibAño(getTablaPib(), año)*100000), digits=2)
  retorno$Recaudación <- as.numeric(retorno$Recaudación)
  
  return(retorno)
}