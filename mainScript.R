#MAIN SCRIPT

library(twitteR)
api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""
setup_twitter_oauth(api_key,api_secret)

rm(list=ls())

path <- "F:/Box Sync/MDS/S1 - Tipología y ciclo de vida de los datos/Práctica Web Scrapping/wrk/Proyecto/stable"
#path <- "C:/Users/javie/OneDrive/Documentos/backup v1.1"

source(paste(path,'/generateURL.R', sep=""))
source(paste(path,'/getPeliculasFromTemporada.R', sep=""))
source(paste(path,'/getRatingPelicula.R', sep=""))
source(paste(path,'/normalizarTitulo.R', sep=""))
source(paste(path,'/isCorrectMovie.R', sep=""))
source(paste(path,'/getPibAño.R', sep=""))
source(paste(path,'/getTablaPib.R', sep=""))
source(paste(path,'/getDiferencia.R', sep=""))
source(paste(path,'/getDiccionarioPaisesEn.R', sep=""))
source(paste(path,'/getDiccionarioPaisesEs.R', sep=""))
source(paste(path,'/lookupPaises.R', sep=""))
source(paste(path,'/geoCode.R', sep=""))
source(paste(path,'/lowerAndTrim.R', sep=""))

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
#peliculasSinPuntuacion <- peliculas[which(peliculas$Puntuacion == 0),]

model <- lm(RelPib~Puntuacion+Año+Nacionalidad, data=peliculas)
peliculas$TasteInd <- peliculas$RelPib - as.numeric(format(predict(model, peliculas), digits=3))

peliculas <- orderBy(~TasteInd, peliculas)
peliculas<- peliculas[seq(dim(peliculas)[1],1),]

#adicionalmente, por explotar estas técnicas, buscaremos en Twitter menciones a los nombres de las 10 primeras películas

t <- NULL
t <- as.data.frame(NULL)
t$text <- NULL

for (i in 1:5)
{
  t <- twListToDF(searchTwitter(lowerAndTrim(peliculas$Largometraje[i]), n=50))
  
  text <- ""
  
  for(j in 1:length(t))
  {
    text <- paste(text, t$text[j], sep="")
    text <- gsub("\\n", "", text)
  }
  
  png(paste(path, "/worldcloud_", peliculas$Largometraje[i], ".png", sep=""), width=1280,height=800)
  
  wordcloud(text)
  dev.off()
}

# Y también obtendremos las coordenadas que el servicio geocode nos devuelve para los distintos países de la lista

paisesLista <- as.character(levels(as.factor(peliculas$Nacionalidad)))
diccionarioEs <- getDiccionarioPaisesEs()
diccionarioEn <- getDiccionarioPaisesEn()
Paises_Latitud_Longitud <- NULL
Paises_Latitud_Longitud$Nombre <- 0
Paises_Latitud_Longitud$Latitud <- 0
Paises_Latitud_Longitud$Longitud <- 0

for (i in 1:length(paisesLista))
{
  ubicacion             <- geoCode(lookupPaises(diccionarioEs, diccionarioEn, paisesLista[i]))
  Paises_Latitud_Longitud$Nombre[i]   <- paisesLista[i]
  Paises_Latitud_Longitud$Latitud[i]  <- ubicacion[1]
  Paises_Latitud_Longitud$Longitud[i] <- ubicacion[2]
}

Paises_Latitud_Longitud <- as.data.frame(Paises_Latitud_Longitud)
Paises_Latitud_Longitud$Latitud <- round(as.double(as.character(Paises_Latitud_Longitud$Latitud)), digits=5)
Paises_Latitud_Longitud$Longitud <- round(as.double(as.character(Paises_Latitud_Longitud$Longitud)), digits=5)


write.csv2(Paises_Latitud_Longitud, paste(path, "/Paises_Latitud_Longitud.csv", sep=""), quote=FALSE, row.names = FALSE)

Percepcion_audiencia_cine_espanola <- peliculas

write.csv2(Percepcion_audiencia_cine_espanola, paste(path, "/Percepcion_audiencia_cine_espanola.csv", sep=""), quote=FALSE, row.names = FALSE)

rm(list=setdiff(ls(), c("Percepcion_audiencia_cine_espanola", "Paises_Latitud_Longitud")))
