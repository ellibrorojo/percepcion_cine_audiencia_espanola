getTablaPib <- function()
{
  tabla <- as.data.frame("https://www.datosmacro.com/pib/espana" %>% read_html() %>%  html_nodes(xpath='//*[@id="node-41"]/div[5]/div[2]/div[2]/table') %>% html_table())
  tabla <- tabla[,-c(3,4,5)]
  colnames(tabla) <- c("Año", "Valor")
  tabla <- tabla[which(tabla$Año >= 2002),]
  tabla$Valor <- gsub('\u20AC', "", tabla$Valor)
  tabla$Valor <- as.numeric(as.character(tabla$Valor))
  
  return(tabla)
}