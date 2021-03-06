#Script para generar archivos RData desde los archivos json

library(jsonlite)
library(dplyr)
library(lubridate)

#Funcion para cargar directorio con archivo y deserializar#
cargar<-function(dir, arch){
  ruta <- file.path(dir, arch)
  dataFile <- fromJSON(ruta)
}


#Directorios (url) de trabajo del proyecto
urlDefault = getwd()
urlData = paste(urlDefault,"/data/",sep = "")
urlFigures = paste(urlDefault,"/figures/",sep = "")
urlModels = paste(urlDefault,"/models/",sep = "")
urlRawData = paste(urlDefault,"/rawdata/",sep = "")
urlReports = paste(urlDefault,"/reports/",sep = "")
urlOutput = paste(urlDefault,"/output/",sep = "")
urlExcelData = paste(urlDefault,"/exceldata/",sep = "")
urlJsonData = paste(urlDefault,"/data/",sep = "")

jsonName = "tld"

#Archivo para almacenar el log de ejecuci�n
fileLog = "log.txt"
#Conexi�n para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(text = as.character(Sys.time()), con = con)

# data de Quito
latMin = -0.400294
latMax =  0.026711
lonLef = -78.591624
lonRig = -78.270936

#Proceso repetitivo
#for(k in 0:160) {
for(k in 161:269) {  
  k=0
  msg = k
  print(msg)
  writeLines(text = as.character(msg), con = con)
  #Proceso deserializar archivo JSON#
  jsonNumber = k
  fileJsonData = paste(jsonName,jsonNumber,".json",sep="")
  dataFile <- cargar(urlJsonData,fileJsonData)
  
  if(length(dataFile$locations) == 0){
    msg = paste("   El archivo ",k," no tiene informaci�n",sep = "")
    print(msg)
    writeLines(text = msg, con = con)
  }else if(length(dataFile$locations) == 9){

    ###############################################################################
    #https://www.chipoglesby.com/2018/03/2018-analyzing-google-location-historyII/
    #https://shiring.github.io/maps/2016/12/30/Standortverlauf_post
    
    #fecha
    timestampMs <- as.numeric(dataFile$locations$timestampMs) #Almacena el n�mero que reprsenta la fecha
    dateTimeLine <- as.POSIXct(timestampMs/1000, origin="1970-01-01") #Para mostrar la fecha con la hora
    
    #location (expressed in degreess decimals)
    latitude <- dataFile$locations$latitudeE7/1e7  #1e7 = 10000000 eje y coordenadas
    longitude <- dataFile$locations$longitudeE7/1e7 #1e7 = 10000000 eje x coordenadas
    
    #precisi�n (m) - < 800 es alta y > 5000 es baja.
    accuracy <- dataFile$locations$accuracy
    
    #precisi�n de la ubicaci�n vertical del dispositivo (m)
    verticalAccuracy <- dataFile$locations$verticalAccuracy
    
    #altitud - en msnm
    altitude <- dataFile$locations$altitude
    
    #velocidad - m/s
    velocity <- dataFile$locations$velocity
    
    #direcci�n en la que viaja el dispositivo.
    heading <- dataFile$locations$heading
    
    #Creamos data frame con todos los datos
    dataSource <- data.frame(dateTimeLine, latitude, longitude, accuracy, verticalAccuracy, altitude, velocity, heading)
    
    #Filtramos datos para UIO
    dataQuito <- subset(dataSource, between(latitude,latMin,latMax))
    dataQuito <- subset(dataQuito, between(longitude,lonLef,lonRig))
    
    #Guardamos dataSource, dataQuito y activities en Raw Data
    fileRawData = paste(jsonName,jsonNumber,".RData",sep="")
    urlSaveRawData = paste(urlRawData,fileRawData,sep="")
    save(dataSource, dataQuito, file=urlSaveRawData)
   
  }
}

#Borrar variables que ya no se utilizar�n
rm(dataFile, accuracy, altitude, dateTimeLine, fileJsonData, fileRawData, heading, 
   latitude, longitude, timestampMs, urlRawData, urlSaveRawData, velocity, verticalAccuracy)

#Hora fin del procesamiento
writeLines(text = as.character(Sys.time()), con = con)
#Cierra conexi�n al archivo
close(con)
#limpia consola
shell("cls")
#Borra las variables del ws
rm(list=ls())
print("Fin procesamiento Historial de Ubicaciones... ")





#Para procesar los registros de actividad - por separado por el tiempo de ejecuci�n

for(k in 161:269) {  
  #k=161
  msg = k
  print(msg)
  writeLines(text = as.character(msg), con = con)
  #Proceso deserializar archivo JSON#
  jsonNumber = k
  fileJsonData = paste(jsonName,jsonNumber,".json",sep="")
  dataFile <- cargar(urlJsonData,fileJsonData)
  
  if(length(dataFile$locations) == 0){
    msg = paste("   El archivo ",k," no tiene informaci�n",sep = "")
    print(msg)
    writeLines(text = msg, con = con)
  }else if(length(dataFile$locations) == 9){
    
    #actividades
    activities <- dataFile$locations$activity
    
    # En segundo grupo de datos, se guarda TODO el objeto que contiene las actividades
    dataActivity = NULL
    for (i in 1:length(activities)) {
      acti = activities[[i]]
      dataActivity = rbind(dataActivity, acti)
    }
    
    fileRawActivity = paste(jsonName,jsonNumber,"Activity.RData",sep="")
    urlSaveRawData = paste(urlRawData,fileRawActivity,sep="")
    save(dataActivity, file=urlSaveRawData)
  }
}





#################################################
#Para incluir los nuevos campos del data set
################################################

jsonName = "tld"

# data de Quito
latMin = -0.400294
latMax =  0.026711
lonLef = -78.591624
lonRig = -78.270936

for(k in 161:269) {  
  k=0
  msg = k
  print(msg)
  writeLines(text = as.character(msg), con = con)
  #Proceso deserializar archivo JSON#
  jsonNumber = k
  fileJsonData = paste(jsonName,jsonNumber,".json",sep="")
  dataFile <- cargar(urlJsonData,fileJsonData)
  
  if(length(dataFile$locations) == 0){
    msg = paste("   El archivo ",k," no tiene informaci�n",sep = "")
    print(msg)
    writeLines(text = msg, con = con)
  }else if(length(dataFile$locations) >= 9){
    
    ###############################################################################
    #https://www.chipoglesby.com/2018/03/2018-analyzing-google-location-historyII/
    #https://shiring.github.io/maps/2016/12/30/Standortverlauf_post
    
    #fecha
    timestampMs <- as.numeric(dataFile$locations$timestampMs) #Almacena el n�mero que reprsenta la fecha
    dateTimeLine <- as.POSIXct(timestampMs/1000, origin="1970-01-01") #Para mostrar la fecha con la hora
    dateTimeLineEc <- dateTimeLine - 5*60*60  
    
    
    #location (expressed in degreess decimals)
    latitude <- dataFile$locations$latitudeE7/1e7  #1e7 = 10000000 eje y coordenadas
    longitude <- dataFile$locations$longitudeE7/1e7 #1e7 = 10000000 eje x coordenadas
    
    #precisi�n (m) - < 800 es alta y > 5000 es baja.
    accuracy <- dataFile$locations$accuracy
    
    #altitud - en msnm
    altitude <- dataFile$locations$altitude
    
    #precisi�n de la ubicaci�n vertical del dispositivo (m)
    verticalAccuracy <- dataFile$locations$verticalAccuracy
    
    #New fields
    source <- dataFile$locations$source
    deviceTag <- dataFile$locations$deviceTag
    platform <- dataFile$locations$platform
    platformType <- dataFile$locations$platformType
    
    #velocidad - m/s
    velocity <- dataFile$locations$velocity
    
    #direcci�n en la que viaja el dispositivo.
    heading <- dataFile$locations$heading
    
    #Creamos data frame con todos los datos
    dataSource <- data.frame(dateTimeLine, dateTimeLineEc, latitude, longitude, accuracy, altitude, verticalAccuracy, 
                             source, deviceTag, platform, platformType, velocity, heading)
    
    #Filtramos datos para UIO
    dataQuito <- subset(dataSource, between(latitude,latMin,latMax))
    dataQuito <- subset(dataQuito, between(longitude,lonLef,lonRig))
    
    #Guardamos dataSource, dataQuito y activities en Raw Data
    #fileRawData = paste(jsonName,jsonNumber,".RData",sep="")
    #urlSaveRawData = paste(urlRawData,fileRawData,sep="")
    #save(dataSource, dataQuito, file=urlSaveRawData)
    
  }
}


dataQuito$source = as.factor(dataQuito$source)

dataQuito$platform = as.factor(dataQuito$platform)

dataQuito$platformType = as.factor(dataQuito$platformType)

summary(dataQuito)

dataGPS = filter(dataQuito, source == 'GPS')

summary(dataGPS)



