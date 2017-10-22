isCorrectMovie <- function(elem4, año)
{
  matrix <- NULL
  
  if (length(elem4) > 0)
  {
    elementosEliminar <- c(which(grepl("results.vote_count", names(elem4))), 
                           which(grepl("results.id", names(elem4))),
                           which(grepl("results.video", names(elem4))), 
                           which(grepl("results.adult", names(elem4))), 
                           which(grepl("results.overview", names(elem4))), 
                           which(grepl("genre_id", names(elem4))),
                           which(grepl("path", names(elem4)))
                          )
    
    elem4 <- elem4[-elementosEliminar]
    
    x6 <- c(seq(1,length(elem4)+1,6))
    
    matrix <- NULL
    for (i in 1:as.numeric(length(x6)-1))
      matrix <- rbind(matrix, as.data.frame(t(elem4[x6[i]:as.numeric(x6[i+1]-1)])))
    
    fechasVacias<-which(is.element(matrix$results.release_date, c("")))
    matrix$results.release_date <- as.character(matrix$results.release_date)
    matrix$results.release_date[fechasVacias] <- "9999-12-31"
    
    rowPuntuacionCero <- which(matrix$results.vote_average == 0)
    if (length(rowPuntuacionCero) > 0)
      matrix <- matrix[-rowPuntuacionCero,]
    
    releaseYear <- year(matrix$results.release_date)
    rowAñoCorrecto <- which(año == releaseYear)
    if (length(rowAñoCorrecto) == 0)
      rowAñoCorrecto <- which(año == releaseYear+1)
    if (length(rowAñoCorrecto) == 0)
      rowAñoCorrecto <- which(año == releaseYear+2)
    
    matrix <- matrix[rowAñoCorrecto,]
  }
  
  if (!is.null(matrix) && nrow(matrix) > 0)
    return(TRUE)
  else
    return(FALSE)
}