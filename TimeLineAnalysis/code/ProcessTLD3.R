#Script para generar datos de SDE y SDD del paquete ASPACE desde los archivos raw (RData)

library(jsonlite)
library(aspace)
library(dplyr)
library(ggmap)

#Obtener url del directorio de trabajo dek proyecto
urlDefault = getwd()

#Directorio donde se encuentran los archivos raw
urlRawData = paste(urlDefault,"/rawdata/",sep = "")

#Directorio donde se almacenan los gráficos
urlFigures = paste(urlDefault,"/figures1/",sep = "")

#Carga mapa de Quito para visualización
fileMaps = "SmartGPSMaps.RData"
load(file=paste(urlRawData,fileMaps,sep = ""))

#Especifica el nombre de los archivos raw
rawName = "tld"

#Variables para la fecha de análisis - MAX 2019-11-18
aYear=2019
aWeek=46 #November 11-17
aWeedDay=4 #Wednesday

#Variable para indicar el mínimo de registros necesarios para el procesamiento
minPoints = 10

#Archivo para almacenar el log de ejecución
fileLog = "log1.txt"
#Conexión para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(text = as.character(Sys.time()), con = con)

#Proceso repetitivo
for(k in 141:143) {
  #k=7
  msg = k
  print(msg)
  writeLines(text = as.character(msg), con = con)
  #Leer el archivo raw#
  rawNumber = k
  fileRawData = paste(rawName,rawNumber,".RData",sep="")
  load(file=paste(urlRawData,fileRawData,sep = ""))
  
  #Análisis puntual
  dataQuitoYear = filter(dataQuito, dataQuito$Y == aYear)
  dataQuitoWeek = filter(dataQuito, dataQuito$Y == aYear & dataQuito$W == aWeek)
  dataQuitoWeekDay = filter(dataQuito, dataQuito$Y == aYear & dataQuito$W == aWeek & dataQuito$Wd == aWeedDay)
  
  if(nrow(dataQuitoWeek) < minPoints){
    ##No existen registros para la fecha indicada - next
    msg = paste("   El archivo ",k," tiene poca o no tiene información en la semana seleccionada",sep = "")
    print(msg)
    writeLines(text = msg, con = con)
    next()
  }else {
    #para que se almacenen los gráficos en el directorio figures
    setwd(urlFigures)
    
    #Datos Año
    nameChart = paste("MapYear-",rawName,rawNumber,".jpeg",sep = "")
    ggmap(roadMapQuito) + 
      geom_point(data = dataQuitoYear,
                 aes(x = dataQuitoYear$longitude[], y = dataQuitoYear$latitude[]), 
                 alpha = .5, 
                 color="darkred", 
                 size = .001) +
      ggtitle(paste("GPS Point Track Year - ",rawName,rawNumber,".json",sep = "")) + 
      xlab("Longitude") +
      ylab("Latitude")
    ggsave(filename = paste(urlFigures,nameChart,sep = ""))
    graphics.off()
    
    nameChart = paste("Year-",rawName,rawNumber,sep = "")
    calc_sde(id=nameChart, filename = paste("sdeloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
             calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoYear[,3:2], verbose=FALSE)
    write.table(sdeatt, sep = ",", file = paste("sdeatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
    plot_sde(plotnew=TRUE, plotSDEaxes=TRUE, 
             plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
             plotpoints=TRUE, points.col='green', points.pch=1, 
             plotcentre=TRUE, centre.col='red', centre.pch=19, 
             titletxt=paste("SDE Year ",rawName,rawNumber,".json",sep = ""), 
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
             titletxt=paste("SDD Year ",rawName,rawNumber,".json",sep = ""),
             xaxis="Longitude", yaxis="Latitude", 
             jpeg = TRUE)
    graphics.off()
    
    #Datos Semana
    nameChart = paste("MapWeek-",rawName,rawNumber,".jpeg",sep = "")
    ggmap(roadMapQuito) + 
      geom_point(data = dataQuitoWeek,
                 aes(x = dataQuitoWeek$longitude[], y = dataQuitoWeek$latitude[]), 
                 alpha = .5, 
                 color="darkred", 
                 size = .0001) +
      ggtitle(paste("GPS Point Track Week - ",rawName,rawNumber,".json",sep = "")) + 
      xlab("Longitude") +
      ylab("Latitude")
    ggsave(filename = paste(urlFigures,nameChart,sep = ""))
    graphics.off()
    
    nameChart = paste("Week-",rawName,rawNumber,sep = "")
    calc_sde(id=nameChart, filename = paste("sdeloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
             calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoWeek[,3:2], verbose=FALSE)
    write.table(sdeatt, sep = ",", file = paste("sdeatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
    plot_sde(plotnew=TRUE, plotSDEaxes=TRUE, 
             plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
             plotpoints=TRUE, points.col='green', points.pch=1, 
             plotcentre=TRUE, centre.col='red', centre.pch=19, 
             titletxt=paste("SDE Week ",rawName,rawNumber,".json",sep = ""), 
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
             titletxt=paste("SDD Week ",rawName,rawNumber,".json",sep = ""),
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
      nameChart = paste("MapDay-",rawName,rawNumber,".jpeg",sep = "")
      ggmap(roadMapQuito) + 
        geom_point(data = dataQuitoWeekDay,
                   aes(x = dataQuitoWeekDay$longitude[], y = dataQuitoWeekDay$latitude[]), 
                   alpha = .5, 
                   color="darkred", 
                   size = .0001) +
        ggtitle(paste("GPS Point Track WeekDay - ",rawName,rawNumber,".json",sep = "")) + 
        xlab("Longitude") +
        ylab("Latitude")
      ggsave(filename = paste(urlFigures,nameChart,sep = ""))
      graphics.off()
      
      nameChart = paste("Day-",rawName,rawNumber,sep = "")
      calc_sde(id=nameChart, filename = paste("sdeloc",nameChart,"_Output.txt",sep=""), centre.xy=NULL, 
               calccentre=TRUE, weighted=FALSE, weights=NULL, points=dataQuitoWeekDay[,3:2], verbose=FALSE)
      write.table(sdeatt, sep = ",", file = paste("sdeatt",nameChart,"_Output.txt",sep=""), col.names = TRUE)
      plot_sde(plotnew=TRUE, plotSDEaxes=TRUE, 
               plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
               plotpoints=TRUE, points.col='green', points.pch=1, 
               plotcentre=TRUE, centre.col='red', centre.pch=19, 
               titletxt=paste("SDE WeekDay ",rawName,rawNumber,".json",sep = ""), 
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
               titletxt=paste("SDD WeekDay ",rawName,rawNumber,".json",sep = ""),
               xaxis="Longitude", yaxis="Latitude", 
               jpeg = TRUE)
      graphics.off()
      
    }
    
    #Volver al directorio por defecto
    setwd(urlDefault)
    msg = paste("   Archivo ",k," OK!!",sep = "")
    print(msg)
    writeLines(text = msg, con = con)
    rm(dataSource, dataQuito, dataQuitoYear, dataQuitoWeek, dataQuitoWeekDay)
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
