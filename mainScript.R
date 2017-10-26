#MAIN SCRIPT

  rm(list=ls())
  
  path <- "F:/Box Sync/MDS/S1 - Tipología y ciclo de vida de los datos/Práctica Web Scrapping/wrk/Proyecto"
  #path <- "C:/Users/javie/OneDrive/Documentos/backup v1.1"

  source(paste(path,'/generateURL.R', sep=""))
  source(paste(path,'/getPeliculasFromTemporada.R', sep=""))
  source(paste(path,'/getRatingPelicula.R', sep=""))
  source(paste(path,'/normalizarTitulo.R', sep=""))
  source(paste(path,'/isCorrectMovie.R', sep=""))
  source(paste(path,'/getPibAño.R', sep=""))
  source(paste(path,'/getTablaPib.R', sep=""))
  source(paste(path,'/getDiferencia.R', sep=""))
  
  #remove(path)
  
  library(RJSONIO)
  library(rvest)
  library(doBy)
  library(lubridate)
  library(wordcloud)
  library(RCurl)
  library(plyr)
  
  nombresColumnas <- c("Posicion", "Largometraje", "Nacionalidad", "Distribuidora",	"Recaudación", "Año", "RelPib")

  peliculas <- NULL
  
  for(currentAño in 2002:(as.numeric(format(Sys.Date(), "%Y"))-1))
  {
    writeLines(as.character(currentAño))
    currentPeliculas <- getPeliculasFromTemporada(currentAño)
    writeLines("Peliculas retrieved")
    colnames(currentPeliculas) <- nombresColumnas
    peliculas <- rbind(peliculas, currentPeliculas)
  }
  
  remove(currentPeliculas)
  remove(currentAño)
  remove(nombresColumnas)
  
  globalConnections = 0
  
  pb <- winProgressBar(title="Cargando puntuaciones", min=0, max=nrow(peliculas), width=375)
  peliculas$Puntuacion <- 0
  
  for (currentRow in 1:nrow(peliculas))
  {
    progreso <- format(currentRow*100/nrow(peliculas), digits=3)
    
    writeLines("-------------------------------------------------------------------------")
    display <- paste(as.character(currentRow), "/", nrow(peliculas), " (", progreso, " %) ", as.character(peliculas$Largometraje[currentRow])," (", as.character(peliculas$Año[currentRow]), ")", sep="")
    
    retorno <- getRatingPelicula(normalizarTitulo(peliculas$Largometraje[currentRow]), peliculas$Año[currentRow], globalConnections)
    rating <- unlist(strsplit(retorno, "&"))[1]
    globalConnections <- globalConnections + as.numeric(unlist(strsplit(retorno, "&"))[2])
    peliculas$Puntuacion[currentRow] <- rating
    
    writeLines(display)
    setWinProgressBar(pb, currentRow, title=display)
  }
  close(pb)
  
  writeLines(paste("Total Connections established:", globalConnections))
  
  peliculas$Puntuacion <- as.numeric(as.character(peliculas$Puntuacion))
  peliculasSinPuntuacion <- peliculas[which(peliculas$Puntuacion == 0),]
  
  model <- lm(RelPib~Puntuacion+Año+Nacionalidad, data=peliculas)
  peliculas$TasteInd <- peliculas$RelPib - as.numeric(format(predict(model, peliculas), digits=3))
  
  peliculas <- orderBy(~TasteInd, peliculas)
  peliculas<- peliculas[seq(dim(peliculas)[1],1),]

  write.csv2(peliculas, paste(path, "/tabla.csv", sep=""), quote=FALSE, row.names = FALSE)
  
  #adicionalmente, por explotar estas técnicas, buscaremos en Twitter menciones a los nombres de las 10 primeras películas
  
  t <- NULL
  t <- as.data.frame(NULL)
  t$text <- NULL
  
  for (i in 1:10)
  {
    writeLines(paste("i", as.character(i)))
    t <- twListToDF(searchTwitter(lowerAndTrim(peliculas$Largometraje[i]), n=50))
    
    text <- ""
    
    for(j in 1:length(t))
    {
      writeLines(paste("j", as.character(j)))
      text <- paste(text, t$text[j], sep="")
      text <- gsub("\\n", "", text)
    }
    
    #text <- gsub(lowerAndTrim(peliculas$Largometraje[i]), "", tolower(text))
    
    png(paste("worldcloud_", peliculas$Largometraje[i], ".png", sep=""), width=1280,height=800)
    
    wordcloud(text)
    dev.off()
  }
  
  # Y también obtendremos las coordenadas que el servicio geocode nos devuelve para los distintos países de la lista
  
  paisesLista <- as.character(levels(as.factor(peliculas$Nacionalidad)))
  diccionarioEs <- getDiccionarioPaisesEs()
  diccionarioEn <- getDiccionarioPaisesEn()
  paisesGPS <- NULL
  paisesGPS$Nombre <- 0
  paisesGPS$Latitud <- 0
  paisesGPS$Longitud <- 0
  
  for (i in 1:length(paisesLista))
  {
    ubicacion             <- geoCode(lookupPaises(diccionarioEs, diccionarioEn, paisesLista[i]))
    paisesGPS$Nombre[i]   <- paisesLista[i]
    paisesGPS$Latitud[i]  <- ubicacion[1]
    paisesGPS$Longitud[i] <- ubicacion[2]
  }

  paisesGPS <- as.data.frame(paisesGPS)
  
  rm(list=setdiff(ls(), c("peliculas", "peliculasSinPuntuacion", "paisesGPS")))