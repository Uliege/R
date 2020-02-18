library(jsonlite)
library(sqldf)
library(aspace)
library(VIM)
library(dplyr)
library(ggmap)
detach(package:rjson)

#Especifica directorio donde estan los JSON#
urlData = "D:/G/GitHub/Uliege/R/TimeLineAnalysis/data/"
urlRawData = "D:/G/GitHub/Uliege/R/TimeLineAnalysis/rawdata/"

#Nombre del archivo json a deserializar#
dataTimeLine = "tld1.json"
dataMaps = "SmartGPSMaps.RData"

rawMapsFile <- paste(urlRawData,dataMaps,sep = "")

#Funcion para cargar directorio con archivo y deserializar#
cargar<-function(dir, arch){
  ruta <- file.path(dir, arch)
  dataFile <- fromJSON(ruta)
}

#Carga el Mapa
load(file=rawMapsFile)

#Guardamos el resultado de la funcion en una variable#
dataFile <- cargar(urlData,dataTimeLine)
rm(urlData)
rm(dataTimeLine)

###############################################################################
#https://www.chipoglesby.com/2018/03/2018-analyzing-google-location-historyII/
#https://shiring.github.io/maps/2016/12/30/Standortverlauf_post

#fecha
timestampMs <- as.numeric(dataFile$locations$timestampMs) #Almacena el número que reprsenta la fecha
dateLocation <- as.POSIXct(timestampMs/1000, origin="1970-01-01") #Para mostrar la fecha con la hora
rm(timestampMs)

#location
latitude <- dataFile$locations$latitudeE7/1e7  #1e7 = 10000000 eje y coordenadas
longitude <- dataFile$locations$longitudeE7/1e7 #1e7 = 10000000 eje x coordenadas

#precisión (m) - < 800 es alta y > 5000 es baja.
accuracy <- dataFile$locations$accuracy

#precisión de la ubicación vertical del dispositivo (m)
verticalAccuracy <- dataFile$locations$verticalAccuracy

#altitud - en msnm
altitude <- dataFile$locations$altitude

#velocidad - m/s
velocity <- dataFile$locations$velocity

#dirección en la que viaja el dispositivo.
heading <- dataFile$locations$heading

#actividades
activities <- dataFile$locations$activity

dateTimeLine = NULL
act1TimeLine = NULL
con1TimeLine = NULL
act2TimeLine = NULL
con2TimeLine = NULL
i<-0
for (i in 1:length(activities)) {
  if(is.null(activities[[i]])){
    dateActivity <- NA
    activity1 <- NA
    confidence1 <- NA
    activity2 <- NA
    confidence2 <- NA
  }else{
    timestampMs <- as.numeric(activities[[i]]$timestampMs[[1]])
    dateActivity <- as.POSIXct(timestampMs/1000, origin="1970-01-01")
    activity1 <- activities[[i]]$activity[[1]]$type[[1]]
    confidence1 <- activities[[i]]$activity[[1]]$confidence[[1]]
    if(length(activities[[i]]$activity[[1]]$type)==1){
      activity2 <- NA
      confidence2 <- NA
    }else{
      activity2 <- activities[[i]]$activity[[1]]$type[[2]]
      confidence2 <- activities[[i]]$activity[[1]]$confidence[[2]]  
    }
  }
  dateTimeLine <- append(dateTimeLine, dateActivity)
  act1TimeLine <- append(act1TimeLine,activity1)
  con1TimeLine <- append(con1TimeLine,confidence1)
  act2TimeLine <- append(act2TimeLine,activity2)
  con2TimeLine <- append(con2TimeLine,confidence2)
  
}

rm(activities)
rm(dataFile)
rm(dateActivity)
rm(activity1)
rm(confidence1)
rm(activity2)
rm(confidence2)
rm(i, timestampMs)

#Data frame Total
dataSource <- data.frame(dateLocation, latitude, longitude, accuracy, verticalAccuracy, altitude, velocity, heading, 
                         dateTimeLine, act1TimeLine, con1TimeLine, act2TimeLine, con2TimeLine)

print("Coordinates range of Quito")
latMin = -0.400294
latMax =  0.026711
lonLef = -78.591624
lonRig = -78.270936

dataQuito <- subset(dataSource, between(latitude,latMin,latMax))
dataQuito <- subset(dataQuito, between(longitude,lonLef,lonRig))

rm(latMin, latMax, lonLef, lonRig)

dataClean <- subset(dataQuito,!is.na(dataQuito$latitude))
dataClean <- na.omit(dataClean)
  
str(dataSource)
summary(dataSource)

str(dataQuito)
summary(dataQuito)

str(dataClean)
summary(dataClean)

aggr(dataSource, col = c("yellow", "green", "gray"), only.miss = TRUE, sortVars = TRUE, numbers = TRUE)

aggr(dataQuito, col = c("yellow", "green", "gray"), only.miss = TRUE, sortVars = TRUE, numbers = TRUE)

coordenadas <- dataQuito[,2:3]

calc_sde(id=1, filename="SDE_Output.txt", centre.xy=NULL, calccentre=TRUE, 
         weighted=FALSE, weights=NULL, points=coordenadas, verbose=FALSE)


plot_sde(plotnew=TRUE, plotSDEaxes=FALSE, plotweightedpts=FALSE, 
         weightedpts.col='black', weightedpts.pch=19, plotpoints=TRUE, 
         points.col='green', points.pch=1, plotcentre=TRUE, centre.col='black', 
         centre.pch=19, titletxt="Title", xaxis="Easting (m)", 
         yaxis="Northing (m)", sde.col='black', sde.lwd=2, jpeg=FALSE)


coordenadasLongLat <- dataQuito[,3:2]

calc_sde(id=1, filename="SDE_Output.txt", centre.xy=NULL, calccentre=TRUE, 
         weighted=FALSE, weights=NULL, points=coordenadasLongLat, verbose=FALSE)


plot_sde(plotnew=TRUE, plotSDEaxes=FALSE, plotweightedpts=FALSE, 
         weightedpts.col='black', weightedpts.pch=19, plotpoints=TRUE, 
         points.col='green', points.pch=1, plotcentre=TRUE, centre.col='black', 
         centre.pch=19, titletxt="Title", xaxis="Easting (m)", 
         yaxis="Northing (m)", sde.col='black', sde.lwd=2, jpeg=FALSE)



ggmap(roadMapQuito) + 
  geom_point(data = dataQuito,
             aes(x = dataQuito$longitude[], y = dataQuito$latitude[]), 
             alpha = .5, 
             color="darkred", 
             size = .0001) +
  ggtitle("GPS Point Track") + 
  xlab("Longitude") +
  ylab("Latitude")



ggmap(roadMapQuito) + 
  geom_point(data = dataSource,
             aes(x = dataSource$longitude[], y = dataSource$latitude[]), 
             alpha = .5, 
             color="darkred", 
             size = .0001) +
  ggtitle("GPS Point Track") + 
  xlab("Longitude") +
  ylab("Latitude")

ggmap(roadMapQuito) + 
  geom_point(data = dataClean,
             aes(x = dataClean$longitude[], y = dataClean$latitude[]), 
             alpha = .5, 
             color="darkred", 
             size = .0001) +
  ggtitle("GPS Point Track") + 
  xlab("Longitude") +
  ylab("Latitude")


dataSQL <- sqldf("select * from totalFrame where YEAR(DATE(dateLocation)) = 2019")


aspace


