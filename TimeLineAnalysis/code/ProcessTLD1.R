library(jsonlite)
library(sqldf)
library(aspace)
library(VIM)
library(dplyr)
library(ggmap)


#Funcion para cargar directorio con archivo y deserializar#
cargar<-function(dir, arch){
  ruta <- file.path(dir, arch)
  dataFile <- fromJSON(ruta)
}

#Obtener url del directorio de trabajo dek proyecto
urlDefault = getwd()

#Carga mapa de Quito para visualización
fileMaps = "SmartGPSMaps.RData"
urlRawData = paste(urlDefault,"/rawdata/",sep = "")
load(file=paste(urlRawData,fileMaps,sep = ""))
rm(fileMaps, urlRawData)

#Especifica directorio donde se almacenan los archivos JSON
urlJsonData = paste(urlDefault,"/data/",sep = "")
jsonName = "tld"

#Variables para la fecha de análisis
aYear=2019
aWeek=46 #November 11-17
aWeedDay=4 #Wednesday

#Variable para indicar el mínimo de registros necesarios para el procesamiento
minPoints = 10

#Directorio donde se almacenan los gráficos
urlFigures = paste(urlDefault,"/figures/",sep = "")

#Archivo para almacenar el log de ejecución
fileLog = "log.txt"
#Conexión para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(text = as.character(Sys.time()), con = con)

#Proceso repetitivo
for(k in 91:100) {
  #k=9
  msg = k
  print(msg)
  writeLines(text = as.character(msg), con = con)
  #Proceso deserializar archivo JSON#
  jsonNumber = k
  fileJsonData = paste(jsonName,jsonNumber,".json",sep="")
  dataFile <- cargar(urlJsonData,fileJsonData)
  
  if(length(dataFile$locations) == 0){
    msg = paste("   El archivo ",k," no tiene información",sep = "")
    print(msg)
    writeLines(text = msg, con = con)
  }else if(length(dataFile$locations) == 9){

    ###############################################################################
    #https://www.chipoglesby.com/2018/03/2018-analyzing-google-location-historyII/
    #https://shiring.github.io/maps/2016/12/30/Standortverlauf_post
    
    #fecha
    timestampMs <- as.numeric(dataFile$locations$timestampMs) #Almacena el número que reprsenta la fecha
    dateTimeLine <- as.POSIXct(timestampMs/1000, origin="1970-01-01") #Para mostrar la fecha con la hora
    
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
    
    dateActivity = NULL
    activity1 = NULL
    confidence1 = NULL
    activity2 = NULL
    confidence2 = NULL
    i<-0
    for (i in 1:length(activities)) {
      if(is.null(activities[[i]])){
        da <- NA
        act1 <- NA
        conf1 <- NA
        act2 <- NA
        conf2 <- NA
      }else{
        timestampMs <- as.numeric(activities[[i]]$timestampMs[[1]])
        da <- as.POSIXct(timestampMs/1000, origin="1970-01-01")
        act1 <- activities[[i]]$activity[[1]]$type[[1]]
        conf1 <- activities[[i]]$activity[[1]]$confidence[[1]]
        if(length(activities[[i]]$activity[[1]]$type)==1){
          act2 <- NA
          conf2 <- NA
        }else{
          act2 <- activities[[i]]$activity[[1]]$type[[2]]
          conf2 <- activities[[i]]$activity[[1]]$confidence[[2]]  
        }
      }
      dateActivity <- append(dateActivity, da)
      activity1 <- append(activity1,act1)
      confidence1 <- append(confidence1,conf1)
      activity2 <- append(activity2,act2)
      confidence2 <- append(confidence2,conf2)
      
    }
    
    #Data frame Total
    dataSource <- data.frame(dateTimeLine, latitude, longitude, accuracy, verticalAccuracy, altitude, velocity, heading, 
                             dateActivity, activity1, confidence1, activity2, confidence2)
    
    # para cuando es NULL la actividad del 1er registro 
    if(is.na(dataSource$dateActivity[1])){
      dataSource["dateActivity"] =  as.POSIXct(dataSource$dateActivity[], origin="1970-01-01")
    }
      
    # data de Quito
    latMin = -0.400294
    latMax =  0.026711
    lonLef = -78.591624
    lonRig = -78.270936
    
    dataQuito <- subset(dataSource, between(latitude,latMin,latMax))
    dataQuito <- subset(dataQuito, between(longitude,lonLef,lonRig))
    
    dataQuito$Y = year(dataQuito$dateTimeLine)
    dataQuito$M = month(dataQuito$dateTimeLine)
    dataQuito$D = mday(dataQuito$dateTimeLine)
    dataQuito$Wd = wday(dataQuito$dateTimeLine)
    dataQuito$W = week(dataQuito$dateTimeLine)
    dataQuito$Ho = hour(dataQuito$dateTimeLine)
    dataQuito$Mi = minute(dataQuito$dateTimeLine)
    dataQuito$Se = second(dataQuito$dateTimeLine)
    
    #Guardamos dataSource y dataQuito en Raw Data
    urlRawData = "D:/G/GitHub/Uliege/R/TimeLineAnalysis/rawdata/"
    fileRawData = paste(jsonName,jsonNumber,".RData",sep="")
    urlSaveRawData = paste(urlRawData,fileRawData,sep="")
    save(dataSource, dataQuito,file=urlSaveRawData)
    
    #Borrar variables que ya no se utilizarán
    rm(activities, dataFile, accuracy, act1, act2, activity1, activity2, altitude, confidence1, confidence2, conf1, conf2, da, dateActivity, 
       dateTimeLine, fileJsonData, fileRawData, heading, latitude, latMax, latMin, longitude, lonLef, lonRig, timestampMs, 
       urlRawData, urlSaveRawData, velocity, verticalAccuracy)
    
    #Análisis puntual
    dataQuitoYear = filter(dataQuito, dataQuito$Y == aYear)
    dataQuitoWeek = filter(dataQuito, dataQuito$Y == aYear & dataQuito$W == aWeek)
    dataQuitoWeekDay = filter(dataQuito, dataQuito$Y == aYear & dataQuito$W == aWeek & dataQuito$Wd == aWeedDay)
    
    if(nrow(dataQuitoWeek) < minPoints){
      ##No existen registros para la fecha indicada - next
      msg = paste("   El archivo ",k," tiene poca o no tiene información en la semana seleccionada",sep = "")
      print(msg)
      writeLines(text = msg, con = con)
    }else {
      #para que se almacenen los gráficos en el directorio figures
      setwd(urlFigures)
      
      #Datos Año
      nameChart = paste("MapYear-",jsonName,jsonNumber,".jpeg",sep = "")
      ggmap(roadMapQuito) + 
        geom_point(data = dataQuitoYear,
                   aes(x = dataQuitoYear$longitude[], y = dataQuitoYear$latitude[]), 
                   alpha = .5, 
                   color="darkred", 
                   size = .001) +
        ggtitle(paste("GPS Point Track Year - ",jsonName,jsonNumber,".json",sep = "")) + 
        xlab("Longitude") +
        ylab("Latitude")
      ggsave(filename = paste(urlFigures,nameChart,sep = ""))
      graphics.off()
      
      nameChart = paste("Year-",jsonName,jsonNumber,sep = "")
      calc_sde(id=nameChart, filename = paste("sdeloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
               calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoYear[,3:2], verbose=FALSE)
      write.table(sdeatt, sep = ",", file = paste("sdeatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
      plot_sde(plotnew=TRUE, plotSDEaxes=TRUE, 
               plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
               plotpoints=TRUE, points.col='green', points.pch=1, 
               plotcentre=TRUE, centre.col='red', centre.pch=19, 
               titletxt=paste("SDE Year ",jsonName,jsonNumber,".json",sep = ""), 
               xaxis="Longitude", yaxis="Latitude", sde.col='red', sde.lwd=2, 
               jpeg=TRUE)
      graphics.off()
      calc_sdd(id=nameChart, filename = paste("sddloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
               calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoYear[,3:2], verbose=FALSE)
      write.table(sddatt, sep = ",", file = paste("sddatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
      plot_sdd(plotnew=TRUE, plothv=TRUE,
               plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
               plotpoints=TRUE, points.col='green', points.pch=1, 
               plotcentre=TRUE, centre.col='red', centre.pch=19, 
               titletxt=paste("SDD Year ",jsonName,jsonNumber,".json",sep = ""),
               xaxis="Longitude", yaxis="Latitude", 
               jpeg = TRUE)
      graphics.off()
      
      #Datos Semana
      nameChart = paste("MapWeek-",jsonName,jsonNumber,".jpeg",sep = "")
      ggmap(roadMapQuito) + 
        geom_point(data = dataQuitoWeek,
                   aes(x = dataQuitoWeek$longitude[], y = dataQuitoWeek$latitude[]), 
                   alpha = .5, 
                   color="darkred", 
                   size = .0001) +
        ggtitle(paste("GPS Point Track Week - ",jsonName,jsonNumber,".json",sep = "")) + 
        xlab("Longitude") +
        ylab("Latitude")
      ggsave(filename = paste(urlFigures,nameChart,sep = ""))
      graphics.off()
      
      nameChart = paste("Week-",jsonName,jsonNumber,sep = "")
      calc_sde(id=nameChart, filename = paste("sdeloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
               calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoWeek[,3:2], verbose=FALSE)
      write.table(sdeatt, sep = ",", file = paste("sdeatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
      plot_sde(plotnew=TRUE, plotSDEaxes=TRUE, 
               plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
               plotpoints=TRUE, points.col='green', points.pch=1, 
               plotcentre=TRUE, centre.col='red', centre.pch=19, 
               titletxt=paste("SDE Week ",jsonName,jsonNumber,".json",sep = ""), 
               xaxis="Longitude", yaxis="Latitude", sde.col='red', sde.lwd=2, 
               jpeg=TRUE)
      graphics.off()
      calc_sdd(id=nameChart, filename = paste("sddloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
               calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoWeek[,3:2], verbose=FALSE)
      write.table(sddatt, sep = ",", file = paste("sddatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
      plot_sdd(plotnew=TRUE, plothv=TRUE,
               plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
               plotpoints=TRUE, points.col='green', points.pch=1, 
               plotcentre=TRUE, centre.col='red', centre.pch=19, 
               titletxt=paste("SDD Week ",jsonName,jsonNumber,".json",sep = ""),
               xaxis="Longitude", yaxis="Latitude",
               jpeg = TRUE)
      graphics.off()
      
      if(nrow(dataQuitoWeekDay) < minPoints){
        ##No existen registros para el día indicado
        msg = paste("   El archivo ",k," tiene poca o no tiene información en el día indicado",sep = "")
        print(msg)
        writeLines(text = msg, con = con)
      }else {
        #Datos Dia
        nameChart = paste("MapDay-",jsonName,jsonNumber,".jpeg",sep = "")
        ggmap(roadMapQuito) + 
          geom_point(data = dataQuitoWeekDay,
                     aes(x = dataQuitoWeekDay$longitude[], y = dataQuitoWeekDay$latitude[]), 
                     alpha = .5, 
                     color="darkred", 
                     size = .0001) +
          ggtitle(paste("GPS Point Track WeekDay - ",jsonName,jsonNumber,".json",sep = "")) + 
          xlab("Longitude") +
          ylab("Latitude")
        ggsave(filename = paste(urlFigures,nameChart,sep = ""))
        graphics.off()
        
        nameChart = paste("Day-",jsonName,jsonNumber,sep = "")
        calc_sde(id=nameChart, filename = paste("sdeloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
                 calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoWeekDay[,3:2], verbose=FALSE)
        write.table(sdeatt, sep = ",", file = paste("sdeatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
        plot_sde(plotnew=TRUE, plotSDEaxes=TRUE, 
                 plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
                 plotpoints=TRUE, points.col='green', points.pch=1, 
                 plotcentre=TRUE, centre.col='red', centre.pch=19, 
                 titletxt=paste("SDE WeekDay ",jsonName,jsonNumber,".json",sep = ""), 
                 xaxis="Longitude", yaxis="Latitude", sde.col='red', sde.lwd=2, 
                 jpeg=TRUE)
        graphics.off()
        calc_sdd(id=nameChart, filename = paste("sddloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
                 calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoWeekDay[,3:2], verbose=FALSE)
        write.table(sddatt, sep = ",", file = paste("sddatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
        plot_sdd(plotnew=TRUE, plothv=TRUE,
                 plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
                 plotpoints=TRUE, points.col='green', points.pch=1, 
                 plotcentre=TRUE, centre.col='red', centre.pch=19, 
                 titletxt=paste("SDD WeekDay ",jsonName,jsonNumber,".json",sep = ""),
                 xaxis="Longitude", yaxis="Latitude", 
                 jpeg = TRUE)
        graphics.off()
        
      }
      
      #Volver al directorio por defecto
      setwd(urlDefault)
      msg = paste("   Archivo ",k," OK!!",sep = "")
      print(msg)
      writeLines(text = msg, con = con)
    }
  }else{
    msg = paste("   El archivo ",k," tiene menos de 9 columnas",sep = "")
    print(msg)
    writeLines(text = msg, con = con)
    
    ###############################################################################
    #https://www.chipoglesby.com/2018/03/2018-analyzing-google-location-historyII/
    #https://shiring.github.io/maps/2016/12/30/Standortverlauf_post
    
    #fecha
    timestampMs <- as.numeric(dataFile$locations$timestampMs) #Almacena el número que reprsenta la fecha
    dateTimeLine <- as.POSIXct(timestampMs/1000, origin="1970-01-01") #Para mostrar la fecha con la hora
    
    #location
    latitude <- dataFile$locations$latitudeE7/1e7  #1e7 = 10000000 eje y coordenadas
    longitude <- dataFile$locations$longitudeE7/1e7 #1e7 = 10000000 eje x coordenadas
    
    #Data frame Total
    dataSource <- data.frame(dateTimeLine, latitude, longitude)
    
    # data de Quito
    latMin = -0.400294
    latMax =  0.026711
    lonLef = -78.591624
    lonRig = -78.270936
    
    dataQuito <- subset(dataSource, between(latitude,latMin,latMax))
    dataQuito <- subset(dataQuito, between(longitude,lonLef,lonRig))
    
    dataQuito$Y = year(dataQuito$dateTimeLine)
    dataQuito$M = month(dataQuito$dateTimeLine)
    dataQuito$D = mday(dataQuito$dateTimeLine)
    dataQuito$Wd = wday(dataQuito$dateTimeLine)
    dataQuito$W = week(dataQuito$dateTimeLine)
    dataQuito$Ho = hour(dataQuito$dateTimeLine)
    dataQuito$Mi = minute(dataQuito$dateTimeLine)
    dataQuito$Se = second(dataQuito$dateTimeLine)
    
    #Guardamos dataSource y dataQuito en Raw Data
    urlRawData = "D:/G/GitHub/Uliege/R/TimeLineAnalysis/rawdata/"
    fileRawData = paste(jsonName,jsonNumber,".RData",sep="")
    urlSaveRawData = paste(urlRawData,fileRawData,sep="")
    save(dataSource, dataQuito,file=urlSaveRawData)
    
    #Borrar variables que ya no se utilizarán
    rm(dataFile, dateTimeLine, fileJsonData, fileRawData, latitude, latMax, latMin, 
       longitude, lonLef, lonRig, timestampMs, urlRawData, urlSaveRawData)
    
    #Análisis puntual
    dataQuitoYear = filter(dataQuito, dataQuito$Y == aYear)
    dataQuitoWeek = filter(dataQuito, dataQuito$Y == aYear & dataQuito$W == aWeek)
    dataQuitoWeekDay = filter(dataQuito, dataQuito$Y == aYear & dataQuito$W == aWeek & dataQuito$Wd == aWeedDay)
    
    if(nrow(dataQuitoWeek) < minPoints){
      ##No existen registros para la fecha indicada - next
      msg = paste("   El archivo ",k," tiene poca o no tiene información en la semana seleccionada",sep = "")
      print(msg)
      writeLines(text = msg, con = con)
    }else {
      #para que se almacenen los gráficos en el directorio figures
      setwd(urlFigures)
      
      #Datos Año
      nameChart = paste("MapYear-",jsonName,jsonNumber,".jpeg",sep = "")
      ggmap(roadMapQuito) + 
        geom_point(data = dataQuitoYear,
                   aes(x = dataQuitoYear$longitude[], y = dataQuitoYear$latitude[]), 
                   alpha = .5, 
                   color="darkred", 
                   size = .001) +
        ggtitle(paste("GPS Point Track Year - ",jsonName,jsonNumber,".json",sep = "")) + 
        xlab("Longitude") +
        ylab("Latitude")
      ggsave(filename = paste(urlFigures,nameChart,sep = ""))
      graphics.off()
      
      nameChart = paste("Year-",jsonName,jsonNumber,sep = "")
      calc_sde(id=nameChart, filename = paste("sdeloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
               calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoYear[,3:2], verbose=FALSE)
      write.table(sdeatt, sep = ",", file = paste("sdeatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
      plot_sde(plotnew=TRUE, plotSDEaxes=TRUE, 
               plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
               plotpoints=TRUE, points.col='green', points.pch=1, 
               plotcentre=TRUE, centre.col='red', centre.pch=19, 
               titletxt=paste("SDE Year ",jsonName,jsonNumber,".json",sep = ""), 
               xaxis="Longitude", yaxis="Latitude", sde.col='red', sde.lwd=2, 
               jpeg=TRUE)
      graphics.off()
      calc_sdd(id=nameChart, filename = paste("sddloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
               calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoYear[,3:2], verbose=FALSE)
      write.table(sddatt, sep = ",", file = paste("sddatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
      plot_sdd(plotnew=TRUE, plothv=TRUE,
               plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
               plotpoints=TRUE, points.col='green', points.pch=1, 
               plotcentre=TRUE, centre.col='red', centre.pch=19, 
               titletxt=paste("SDD Year ",jsonName,jsonNumber,".json",sep = ""),
               xaxis="Longitude", yaxis="Latitude", 
               jpeg = TRUE)
      graphics.off()
      
      #Datos Semana
      nameChart = paste("MapWeek-",jsonName,jsonNumber,".jpeg",sep = "")
      ggmap(roadMapQuito) + 
        geom_point(data = dataQuitoWeek,
                   aes(x = dataQuitoWeek$longitude[], y = dataQuitoWeek$latitude[]), 
                   alpha = .5, 
                   color="darkred", 
                   size = .0001) +
        ggtitle(paste("GPS Point Track Week - ",jsonName,jsonNumber,".json",sep = "")) + 
        xlab("Longitude") +
        ylab("Latitude")
      ggsave(filename = paste(urlFigures,nameChart,sep = ""))
      graphics.off()
      
      nameChart = paste("Week-",jsonName,jsonNumber,sep = "")
      calc_sde(id=nameChart, filename = paste("sdeloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
               calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoWeek[,3:2], verbose=FALSE)
      write.table(sdeatt, sep = ",", file = paste("sdeatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
      plot_sde(plotnew=TRUE, plotSDEaxes=TRUE, 
               plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
               plotpoints=TRUE, points.col='green', points.pch=1, 
               plotcentre=TRUE, centre.col='red', centre.pch=19, 
               titletxt=paste("SDE Week ",jsonName,jsonNumber,".json",sep = ""), 
               xaxis="Longitude", yaxis="Latitude", sde.col='red', sde.lwd=2, 
               jpeg=TRUE)
      graphics.off()
      calc_sdd(id=nameChart, filename = paste("sddloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
               calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoWeek[,3:2], verbose=FALSE)
      write.table(sddatt, sep = ",", file = paste("sddatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
      plot_sdd(plotnew=TRUE, plothv=TRUE,
               plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
               plotpoints=TRUE, points.col='green', points.pch=1, 
               plotcentre=TRUE, centre.col='red', centre.pch=19, 
               titletxt=paste("SDD Week ",jsonName,jsonNumber,".json",sep = ""),
               xaxis="Longitude", yaxis="Latitude",
               jpeg = TRUE)
      graphics.off()
      
      if(nrow(dataQuitoWeekDay) < minPoints){
        ##No existen registros para el día indicado
        msg = paste("   El archivo ",k," tiene poca o no tiene información en el día indicado",sep = "")
        print(msg)
        writeLines(text = msg, con = con)
      }else {
        #Datos Dia
        nameChart = paste("MapDay-",jsonName,jsonNumber,".jpeg",sep = "")
        ggmap(roadMapQuito) + 
          geom_point(data = dataQuitoWeekDay,
                     aes(x = dataQuitoWeekDay$longitude[], y = dataQuitoWeekDay$latitude[]), 
                     alpha = .5, 
                     color="darkred", 
                     size = .0001) +
          ggtitle(paste("GPS Point Track WeekDay - ",jsonName,jsonNumber,".json",sep = "")) + 
          xlab("Longitude") +
          ylab("Latitude")
        ggsave(filename = paste(urlFigures,nameChart,sep = ""))
        graphics.off()
        
        nameChart = paste("Day-",jsonName,jsonNumber,sep = "")
        calc_sde(id=nameChart, filename = paste("sdeloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
                 calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoWeekDay[,3:2], verbose=FALSE)
        write.table(sdeatt, sep = ",", file = paste("sdeatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
        plot_sde(plotnew=TRUE, plotSDEaxes=TRUE, 
                 plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
                 plotpoints=TRUE, points.col='green', points.pch=1, 
                 plotcentre=TRUE, centre.col='red', centre.pch=19, 
                 titletxt=paste("SDE WeekDay ",jsonName,jsonNumber,".json",sep = ""), 
                 xaxis="Longitude", yaxis="Latitude", sde.col='red', sde.lwd=2, 
                 jpeg=TRUE)
        graphics.off()
        calc_sdd(id=nameChart, filename = paste("sddloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
                 calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoWeekDay[,3:2], verbose=FALSE)
        write.table(sddatt, sep = ",", file = paste("sddatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
        plot_sdd(plotnew=TRUE, plothv=TRUE,
                 plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
                 plotpoints=TRUE, points.col='green', points.pch=1, 
                 plotcentre=TRUE, centre.col='red', centre.pch=19, 
                 titletxt=paste("SDD WeekDay ",jsonName,jsonNumber,".json",sep = ""),
                 xaxis="Longitude", yaxis="Latitude", 
                 jpeg = TRUE)
        graphics.off()
        
      }
      
      #Volver al directorio por defecto
      setwd(urlDefault)
      msg = paste("   Archivo ",k," OK!!",sep = "")
      print(msg)
      writeLines(text = msg, con = con)
    }
  }
}
#Hora fin del procesamiento
writeLines(text = as.character(Sys.time()), con = con)
#Cierra conexión al archivo
close(con)
#limpia consola
shell("cls")
#Borra las variables del ws
rm(list=ls())
print("Fin procesamiento... ")
