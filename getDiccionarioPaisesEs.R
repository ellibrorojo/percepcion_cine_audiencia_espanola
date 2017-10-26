getDiccionarioPaisesEs <- function()
{
  tableEs <- "https://es.wikipedia.org/wiki/ISO_3166-1" %>% read_html() %>%  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>% html_table()
  tableEs <- as.data.frame(tableEs)
  tableEs <- tableEs[-c(2,3,4,6)]
  colnames(tableEs) <- c("Nombre", "Código")
  
  return(tableEs)
}