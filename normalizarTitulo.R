normalizarTitulo <- function(titulo)
{
  titulo <- gsub("DISNEY'S "                                       , "", titulo, ignore.case=T)
  titulo <- gsub("DISNEY/PIXAR "                                   , "", titulo, ignore.case=T)
  titulo <- gsub("DREAMWORKS "                                     , "", titulo, ignore.case=T)
  titulo <- gsub(" de Eric Toledano y Olivier Nakache"             , "", titulo, ignore.case=T)
  titulo <- gsub(" ¿Quien quiere ser millonario?"                  , "", titulo, ignore.case=T)
  titulo <- gsub(":SALVAR LA TIERRA"                               , "", titulo, ignore.case=T)
  titulo <- gsub(" UNO MAS ENTRA EN JUEGO"                         , "", titulo, ignore.case=T)
  titulo <- gsub("X 2"                                             , "X2", titulo, ignore.case=T)
  titulo <- gsub("\\(Cambiando el pasado\\)"                           , "", titulo, ignore.case=T)
  titulo <- gsub(" \\(SORCERER'S APPRENTICE\\)"                    , "", titulo, ignore.case=T)
  titulo <- gsub("ª"                                               , "", titulo, ignore.case=T)
  titulo <- gsub(" \\-UN FILM DE ROB MARSHALL\\-"                  , "", titulo, ignore.case=T)
  titulo <- gsub("\\(ANGELS AND DEMONS\\)"                         , "", titulo, ignore.case=T)
  
  

  return(titulo)
}