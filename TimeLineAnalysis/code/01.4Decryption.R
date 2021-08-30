#Script para validar los archivos json de los estudiantes

library(jsonlite)
library(dplyr)
library(lubridate)

#Funcion para cargar directorio con archivo y deserializar#
cargar<-function(dir, arch){
  ruta <- file.path(dir, arch)
  dataFile <- fromJSON(ruta)
}


#Directorios (url) de trabajo del proyecto
#urlDefault = getwd()
urlDefault = 'D:/G/GitHub/Uliege/R/TimeLineAnalysis'
urlData = paste(urlDefault,"/data/",sep = "")
urlFigures = paste(urlDefault,"/figures/",sep = "")
urlModels = paste(urlDefault,"/models/",sep = "")
urlRawData = paste(urlDefault,"/dataRaw/",sep = "")
urlReports = paste(urlDefault,"/reports/",sep = "")
urlOutput = paste(urlDefault,"/output/",sep = "")
urlExcelData = paste(urlDefault,"/dataExcel/",sep = "")
urlJsonData = paste(urlDefault,"/data/",sep = "")

# data de Quito
latMin = -0.400294
latMax =  0.026711
lonLef = -78.591624
lonRig = -78.270936

#File name
jsonName = "tld"

iniFile = 469
finFile = 473

for(i in iniFile:finFile) {
  #i=319
  print(i)
  jsonNumber = i
  fileJsonData = paste(jsonName,jsonNumber,".json",sep="")
  dataFile <- cargar(urlJsonData,fileJsonData)
  
  if(length(dataFile$locations) == 0){
    print(paste("   El archivo ",i," no tiene informaci?n",sep = ""))
  }else if(length(dataFile$locations) >= 9){
    ###############################################################################
    #https://www.chipoglesby.com/2018/03/2018-analyzing-google-location-historyII/
    #https://shiring.github.io/maps/2016/12/30/Standortverlauf_post
    
    #fecha
    timestampMs <- as.numeric(dataFile$locations$timestampMs) #Almacena el n?mero que reprsenta la fecha
    dateTimeLine <- as.POSIXct(timestampMs/1000, origin="1970-01-01") #Para mostrar la fecha con la hora
    
    #location (expressed in degreess decimals)
    latitude <- dataFile$locations$latitudeE7/1e7  #1e7 = 10000000 eje y coordenadas
    longitude <- dataFile$locations$longitudeE7/1e7 #1e7 = 10000000 eje x coordenadas
    
    #precisi?n (m) - < 800 es alta y > 5000 es baja.
    accuracy <- dataFile$locations$accuracy
    if(is.null(accuracy)){
      accuracy=-99
    }
    
    #precisi?n de la ubicaci?n vertical del dispositivo (m)
    verticalAccuracy <- dataFile$locations$verticalAccuracy
    if(is.null(verticalAccuracy)){
      verticalAccuracy=-99
    }
    
    #altitud - en msnm
    altitude <- dataFile$locations$altitude
    if(is.null(altitude)){
      altitude=-99
    }
    
    #velocidad - m/s
    velocity <- dataFile$locations$velocity
    if(is.null(velocity)){
      velocity=-99
    }
    
    #direcci?n en la que viaja el dispositivo.
    heading <- dataFile$locations$heading
    if(is.null(heading)){
      heading=-99
    }
    
    #Creamos data frame con todos los datos
    dataSource <- data.frame(dateTimeLine, latitude, longitude, accuracy, verticalAccuracy, altitude, velocity, heading)
    
    #Filtramos datos para UIO
    dataQuito <- subset(dataSource, between(latitude,latMin,latMax))
    dataQuito <- subset(dataQuito, between(longitude,lonLef,lonRig))
    
    #Guardamos dataSource, dataQuito y activities en Raw Data
    fileRawData = paste(jsonName,jsonNumber,".RData",sep="")
    urlSaveRawData = paste(urlRawData,fileRawData,sep="")
    save(dataSource, dataQuito, file=urlSaveRawData)
    
    #Validaci?n de los datos que tienen el archivo
    print(paste('Archivo: ', i ))
    print(paste('N?mero de Registros: ', nrow(dataQuito)))
    inicio = dataQuito[1,1]
    print(paste('Fecha Menor: ', inicio))
    fin = dataQuito[nrow(dataQuito), 1]
    print(paste('Fecha Mayor: ', fin))
    periodo = fin -inicio
    print(paste('Per?odo: ', as.period(periodo)))
    print(paste('Duraci?n total: ',as.duration(periodo)))
    
  }else{
    
    print(paste("   El archivo ",i," tiene menos de 9 columnas",sep = ""))
    
    #fecha
    timestampMs <- as.numeric(dataFile$locations$timestampMs) #Almacena el n?mero que reprsenta la fecha
    dateTimeLine <- as.POSIXct(timestampMs/1000, origin="1970-01-01") #Para mostrar la fecha con la hora
    
    #location (expressed in degreess decimals)
    latitude <- dataFile$locations$latitudeE7/1e7  #1e7 = 10000000 eje y coordenadas
    longitude <- dataFile$locations$longitudeE7/1e7 #1e7 = 10000000 eje x coordenadas
    
    #Creamos data frame con todos los datos
    dataSource <- data.frame(dateTimeLine, latitude, longitude)
    
    #Filtramos datos para UIO
    dataQuito <- subset(dataSource, between(latitude,latMin,latMax))
    dataQuito <- subset(dataQuito, between(longitude,lonLef,lonRig))
    
    #Guardamos dataSource, dataQuito y activities en Raw Data
    fileRawData = paste(jsonName,jsonNumber,".RData",sep="")
    urlSaveRawData = paste(urlRawData,fileRawData,sep="")
    save(dataSource, dataQuito, file=urlSaveRawData)
    
    #Validaci?n de los datos que tienen el archivo
    print(paste('Archivo: ', i ))
    print(paste('N?mero de Registros: ', nrow(dataQuito)))
    inicio = dataQuito[1,1]
    print(paste('Fecha Menor: ', inicio))
    fin = dataQuito[nrow(dataQuito), 1]
    print(paste('Fecha Mayor: ', fin))
    periodo = fin -inicio
    print(paste('Per?odo: ', as.period(periodo)))
    print(paste('Duraci?n total: ',as.duration(periodo)))
  }
  
}

