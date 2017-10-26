getDiccionarioPaisesEn <- function()
{
  tableEn <- "https://en.wikipedia.org/wiki/ISO_3166-1" %>% read_html() %>%  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>% html_table()
  tableEn <- as.data.frame(tableEn)
  tableEn <- tableEn[-c(2,3,5,6)]
  colnames(tableEn) <- c("Nombre", "Código")

  return(tableEn)
}