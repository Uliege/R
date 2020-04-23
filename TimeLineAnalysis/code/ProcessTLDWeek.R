#Script para generar datos de SDE y SDD del paquete ASPACE desde los archivos raw (RData)

#library(jsonlite)
#library(aspace)
#library(dplyr)
#library(ggmap)
library(sqldf)
library(hrbrthemes)
library(SearchTrees)
library(rworldmap)


#Obtener url del directorio de trabajo dek proyecto
urlDefault = getwd()

#Directorio donde se encuentran los archivos raw
urlRawData = paste(urlDefault,"/rawdata/",sep = "")

#Directorio donde se almacenan los gráficos
#urlFigures = paste(urlDefault,"/figures1/",sep = "")

#Carga mapa de Quito para visualización
#fileMaps = "SmartGPSMaps.RData"
#load(file=paste(urlRawData,fileMaps,sep = ""))

#Especifica el nombre de los archivos raw
rawName = "tld"

#Variables para la fecha de análisis - MAX 2019-11-18
#aYear=2019
#aWeek=46 #November 11-17
#aWeedDay=4 #Wednesday

#Variable para indicar el mínimo de puntos por semana necesarios para el procesamiento
minSamplesWeek = 350
minSamples = minSamplesWeek * 4

#Variables donde se almacenarán los datos 
dataCluster = NULL
dataSDE = NULL
dataSDD = NULL
dataBOX = NULL


#Archivo para almacenar el log de ejecución
#fileLog = "logW.txt"
#Conexión para escibir en el archivo
#con <- file(fileLog, open="a")
#Hora de inicio del proceso
#writeLines(text = as.character(Sys.time()), con = con)

#Proceso repetitivo
for(k in 0:160) {
  k=0
  #msg = k
  #print(msg)
  #writeLines(text = as.character(msg), con = con)
  
  #Leer el archivo raw#
  rawNumber = k
  fileRawData = paste(rawName,rawNumber,".RData",sep="")
  load(file=paste(urlRawData,fileRawData,sep = ""))
  
  if(nrow(dataQuito) > minSamples){
    
    #Variable para contar registros
    dataQuito$contador = 1
    
    #Sacamos años y semanas a procesar que tengan mínimo número de regs
    Y_W = dataQuito %>% 
      group_by(Y,W) %>% 
      summarise(total = sum(contador)) %>%
      filter(total >= minSamplesWeek)
      
    
    
    
    
    query = "select distinct(Y) from dataQuito "
    xYears = sqldf(query)
    
    str(dataQuito)
    
    
    dataQuito %>% 
      group_by(Y,W) %>% 
      summarise(total = sum(contador))
    
    
    for(i in 1:nrow(xYears)){
      #i=1
      #Sacamos semanas de cada año
      xYear = xYears[i,]
      query = paste("select distinct(W) from dataQuito where Y =", xYear)
      xWeeks = sqldf(query)
      
      
      atencion_ciudadano %>% 
        filter(AÑO == 2014) %>% 
        group_by(BARRIO) %>% 
        summarise(total = sum(total))
      
      
      print(xYear)
      
      for(j in 1:nrow(xWeeks)){
        #j=1
        xWeek = xWeeks[j,]
        dataWeek = filter(dataQuito, Y == xYear & W == xWeek)
        if(nrow(dataWeek) >= minPuntosWeek){
          print(xWeek)
          
          #Datos del Cluster
          xCoord = select(dataWeek, 2:3)
          numCenters = 3
          numStart = 50
          km_clusters <- kmeans(x = xCoord, centers = numCenters, nstart = numStart)
          ye = NULL
          we = NULL
          for (i in 1:numCenters){
            ye = append(ye, xYear)
            we = append(we, xWeek)
          }
          cl= as.data.frame(km_clusters$centers)
          cl[,'year'] = xYear
          cl[,'week'] = xWeek
          cl[,'id'] = paste(rawName,rawNumber,sep="")
          dataCluster = rbind(dataCluster, cl)
        }
        
      }
      
    }
    
  }
  
}
  
  
  ggplot(dataCluster, aes(x=longitude, y=latitude)) + geom_point()
  
  ggmap(roadMapQuito) + 
    geom_point(data = dataCluster,
               aes(x = dataCluster$longitude[], y = dataCluster$latitude[]), 
               alpha = .5, 
               color="darkred", 
               size = .0001) +
    ggtitle("") + 
    xlab("Longitude") +
    ylab("Latitude")
  
  library(ggmap)
  
  table(dataCluster$id)
  ggplot(dataCluster, aes(id)) + geom_histogram(stat = 'count')
  
  
  query = "select id, count(*) from dataCluster group by id"
  dd = sqldf(query)
  
  dd159 = filter(dataCluster, id == 'tld159' & year == 2017)
  ggplot(dd159, aes(year)) + geom_histogram(stat = 'count')
  
  ggplot(dd159, aes(x=longitude, y=latitude)) + geom_point()
  
  ggmap(roadMapQuito) + 
    geom_point(data = dd159,
               aes(x = longitude[], y = latitude[]), 
               alpha = .5, 
               color="darkred", 
               size = .0001) +
    ggtitle("") + 
    xlab("Longitude") +
    ylab("Latitude")
  
  
  dd0 = filter(dataCluster, id == 'tld0')
  ggplot(dd0, aes(year)) + geom_histogram(stat = 'count')
  
  ggplot(dd0, aes(x=longitude, y=latitude)) + geom_point()
  
  ggmap(roadMapQuito) + 
    geom_point(data = dd0,
               aes(x = longitude[], y = latitude[]), 
               alpha = .5, 
               color="darkred", 
               size = .0001) +
    ggtitle("") + 
    xlab("Longitude") +
    ylab("Latitude")
  
  
  
  
  
  
  
  hist(dataQuito$Y)
  hist(dataQuito$W)
  
  ggplot(dataQuito, aes(x=longitude, y=latitude)) + geom_point()
  
  
  table(dataQuito$Y)
  ggplot(dataQuito, aes(Y)) + geom_histogram(stat = 'count')
  
  table(dataQuito$M)
  ggplot(dataQuito, aes(M)) + geom_histogram(stat = 'count')
  
  query = "select * from dataQuito where Y=2018"
  dataQuito2018 = sqldf(query)
  ggplot(dataQuito2018, aes(M)) + geom_histogram(stat = 'count')
  
  query = "select * from dataQuito where Y=2019"
  dataQuito2019 = sqldf(query)
  ggplot(dataQuito2019, aes(M)) + geom_histogram(stat = 'count')
  ggplot(dataQuito2019, aes(W)) + geom_histogram(stat = 'count')

  query = "select distinct(W) from dataQuito2019"
  dataWeek2019 = sqldf(query)
  
  
  library(dplyr)
  library(ggplot2)  
  
  #Semana 6
  
  data6Week2019 = filter(dataQuito2019, W == 6)
  
  tm = select(data6Week2019, 2:3)
  
  ggplot(tm, aes(x=longitude, y=latitude)) + geom_point()
  
  nrow(tm)
  numCenters = 3
  numStart = 50
  
  km_clusters <- kmeans(x = tm, centers = numCenters, nstart = numStart)
  
  km_clusters$centers
  
  
  #Semana 11
  
  xYear = 2019
  xWeek=11
  
  dataWeek = filter(dataQuito2019, Y == xYear & W == xWeek)
  
  xCoord = select(dataWeek, 2:3)
  
  ggplot(xCoord, aes(x=longitude, y=latitude)) + geom_point()
  
  nrow(xCoord)
  numCenters = 3
  numStart = 50
  
  km_clusters <- kmeans(x = tm, centers = numCenters, nstart = numStart)
  
  ye = NULL
  we = NULL
  for (i in 1:numCenters){
    ye = append(ye, xYear)
    we = append(we, xWeek)
  }
  
  ye
  we
  aa= as.data.frame(km_clusters$centers)
  aa[,'year'] = xYear
  aa[,'week'] = xWeek
 
  
  
  
  
  
  
  
  
  query = "select * from dataQuito where Y=2019 and W between 39 and 44"
  Y19W39a44 = sqldf(query)
  ggplot(Y19W39a44, aes(W)) + geom_histogram(stat = 'count')
  
  ggplot(Y19W39a44, aes(x=longitude, y=latitude)) + geom_point()
  
  query = "select longitude, latitude from Y19W39a44"
  tm = sqldf(query)
  
  set.seed(101)
  
  nrow(tm)
  numCenters = 3
  numStart = 50
  
  km_clusters <- kmeans(x = tm, centers = numCenters, nstart = numStart)
  
  km_clusters$centers
  
  
  query = "select longitude, latitude from Y19W39a44"
  tm = sqldf(query)
  
  
  
  
  
  
  anios1 = sqldf("select distinct(a.Y), a.M, a.W, a.D, count(a.latitude) as GPS from dataQuito a group by a.Y, a.W order by 1,2,3,4")
  
  anios2 = sqldf("select a.Y, a.M, a.W, a.D, count(a.latitude) as GPS from dataQuito a group by a.Y, a.M, a.W, a.D order by 1,2,3,4")
  
  semanas = sqldf("select distinct(a.W), a.Y, count(a.latitude) as GPS from dataQuito a group by a.W order by a.Y, a.W")
  
  dias = sqldf("select distinct(a.D), a.M, a.W, a.Y, count(a.latitude) as GPS from dataQuito a group by a.D order by a.Y, a.M, a.D")
  
  sqldf("select sum(GPS) from dias")
  
  sqldf("select sum(GPS) from semanas")
  
  
  
  dataResumen = sqldf("select a.Y, a.M, a.W, a.D, count(a.latitude) as GPS from dataQuito a group by a.Y, a.M, a.W, a.D order by 1,2,3,4")
  
  aaa = sqldf("select * from dataQuito a where a.Y = 2019 and a.W = 46")
  
  
  
  
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
