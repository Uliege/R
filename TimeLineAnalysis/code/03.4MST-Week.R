#https://cran.r-project.org/web/packages/emstreeR/emstreeR.pdf
#https://github.com/cran/emstreeR
#Script para generar datos del MST (Minnimun Spanning Tree) del paquete EMSTREER desde los archivos CSV que vienen como resultado de la limpieza en python
#Los datos vienen del script 02.1CleanDataSkmob.py donde se incorporó el nuevo campo de la semana, las métricas de los viajes, los clusters y la ubicación de la casa
#Se incluye la transformación de coordenadas. De grados decimales (GPS) a proyección en metros (ArcGIS)
#CÁLCULO SEMANAL

#Delete  work space variables (Caution!!)
rm(list=ls())
shell("cls")

library(emstreeR)
library(dplyr)
library(sf)
library(stringr)
library(tidyr)
library(lubridate)

#Directorios (url) de trabajo del proyecto
urlProyecto = 'D:/G/GitHub/Uliege/R/TimeLineAnalysis'
#urlProyecto = getwd()
urlData = paste(urlProyecto,"/data/",sep = "")
urlDownloads = "C:/Users/gmonc/Downloads/"
urlExcelData = paste(urlProyecto,"/dataExcel/",sep = "")
urlFigures = paste(urlProyecto,"/figures/",sep = "")
urlModels = paste(urlProyecto,"/models/",sep = "")
urlOutput = paste(urlProyecto,"/output/",sep = "")
urlRawData = paste(urlProyecto,"/dataRaw/",sep = "")
urlReports = paste(urlProyecto,"/reports/",sep = "")
urlShapeData = paste(urlProyecto,"/dataShape/",sep = "")
urlLogs = paste(urlProyecto,"/logs/",sep = "")

#Archivo para almacenar el log de ejecución (cuando el script se ejecuta en un servidor)
fileLog = paste(urlLogs,"log-03.4-MST-Week ",trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
#Conexión para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(paste("Process start Time: ", as.character(Sys.time())), con = con)

#Variables to store results
#MST
#Weekly
MSTWeek = NULL
MSTWeekSummary = NULL

##############################################

#Read files generated in python

###############################################################################
# 2 files process
###############################################################################

#pName = "tldTotal 0-173"
#pName = "tldTotal 271-308"
pName = "tldTotal 314-332"
idFile = pName
excelFile = paste(urlExcelData,'POI/',idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints1 = read.csv(excelFile)
}

#pName = "tldTotal 174-269"
#pName = "tldTotal 309-313"
pName = "tldTotal 333-337"
idFile = pName
excelFile = paste(urlExcelData,'POI/',idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints2 = read.csv(excelFile)
}

activityPoints = rbind(activityPoints1, activityPoints2)
rm(activityPoints1)
rm(activityPoints2)

###############################################################################
# 1 file process
###############################################################################

#pName = "tldTotal 338-348"
#pName = "tldTotal 349-389"
#pName = "tldTotal 390-397"
#pName = "tldTotal 398-419"
#pName = "tldTotal 420-445"
#pName = "tldTotal 446-459"
#pName = "tldTotal 460-468"
pName = "tldTotal 469-473"
idFile = pName
excelFile = paste(urlExcelData,'POI/',idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints = read.csv(excelFile)
}

#Change name columns
names(activityPoints)[3] = 'lon' 
names(activityPoints)[11] = 'idPerson'

#Transform coordinates (lon, lat) from GPS degrees decimal (4362) to Ecuador projection in meters (32617) 
coord = select(activityPoints, lon, lat)
coord = coord %>% st_as_sf(coords = c("lon", "lat"))
coord = st_set_crs(coord, 4326)
coord = st_transform(coord, 32617)
coordDf = data.frame(st_coordinates(coord[,1]))
activityPoints$lon = coordDf$X
activityPoints$lat = coordDf$Y
rm(coord)
rm(coordDf)

activityPoints$idDay <- substr(activityPoints$datetime, 1, 10)

#barplot(table(activityPoints$idPerson))

personas = unique(activityPoints$idPerson)
#personas = personas[1:10]

for(p in personas){
  #print(p)
  #p="tld0"
  writeLines("**************************************************************************", con = con)
  writeLines(paste("Processing File: ", p), con = con)
  writeLines(paste("Start Time: ", as.character(Sys.time())), con = con)
  #Select data for each individual
  activityPointsAnalysis = activityPoints %>%
    filter(idPerson == p) %>%
    select(idPerson, idWeek, idDay, lon, lat, cluster)
  
  #Control to validate at least 1 Activity points
  if(nrow(activityPointsAnalysis)>0){
  
    #############################
    # Calculate MST per Week
    #############################
    
    #Identify weeks to calculate MST. (Each week have at least 3 Activity GPS points)
    weeks = activityPointsAnalysis %>% 
      group_by(idWeek, idPerson) %>%
      count(idWeek, sort = TRUE) %>%
      filter(n > 2) %>% 
      arrange(idWeek) 
    
    if(nrow(weeks) > 0){
      for(i in 1:nrow(weeks)){
        #i=1
        #Weekly variables
        eachWeek = weeks[i,]
        
        #Filter data to calculate MST weekly
        dataWeek = activityPointsAnalysis %>%
          filter(idWeek == eachWeek$idWeek)
        
        #Select coordinates to calculations
        coord = select(dataWeek, lon, lat)
        
        tryCatch(
          {
            #Compute MST (Minimum Spanning Tree)
            mstOut <- ComputeMST(coord)
            mstOut$idFile = eachWeek$idPerson
            mstOut$idWeek = eachWeek$idWeek
            MSTWeek = rbind(MSTWeek, mstOut)
            
            mstOutSummary = data.frame(idFile = eachWeek$idPerson, idWeek = eachWeek$idWeek, activityPoints = eachWeek$n, 
                                          mstDistance = sum(mstOut$distance))
            MSTWeekSummary = rbind(MSTWeekSummary, mstOutSummary)
            
            #Finish try-catch
          },
          warning = function(w) {
            writeLines(paste("  Warning WEEK!!",eachWeek$idWeek,"no MST calculation",eachWeek$n,"points"), con = con)
          },
          error = function(e) {
            writeLines(paste("  ERROR WEEK!!",eachWeek$idWeek,"no MST calculation",eachWeek$n,"points"), con = con)
          }
        )
      }
      msg = paste(eachWeek$idPerson, ":", nrow(weeks), "weeks, rest the Warnings!!")
      writeLines(msg, con = con)
      print(msg)
    }else{
      msg = paste(p, ":", "NO weeks to process")
      writeLines(msg, con = con)
      print(msg)
    }
    
  }else{
    msg = paste(p, ":", "NO activity points")
    writeLines(msg, con = con)
    print(msg)
  }
}

 
msg = paste("Fin del proceso")
print(msg)
writeLines(paste("End Time: ", as.character(Sys.time())), con = con)

#Cerramos conexión del archivo LOG
close(con)

#Export results
fileRawFinal = paste("MSTWeekTotal",trimws(str_replace_all(as.character(Sys.Date()),":","")),".RData",sep="")
urlSaveRawData = paste(urlRawData,'MST/',fileRawFinal,sep="")
save(MSTWeek, MSTWeekSummary, file=urlSaveRawData)

fileExcelFinal = paste("MSTWeek",trimws(str_replace_all(as.character(Sys.Date()),":","")),".csv",sep="")
urlSaveExcelData = paste(urlExcelData,'MST/',fileExcelFinal,sep="")
write.csv(MSTWeek,urlSaveExcelData)

fileExcelFinal = paste("MSTWeekSummary",trimws(str_replace_all(as.character(Sys.Date()),":","")),".csv",sep="")
urlSaveExcelData = paste(urlExcelData,'MST/',fileExcelFinal,sep="")
write.csv(MSTWeekSummary,urlSaveExcelData)

#Eliminamos variables
rm(activityPoints)
rm(activityPointsAnalysis)
rm(coord)
rm(dataWeek)
rm(eachWeek)
rm(weeks)
rm(mstOut)
rm(mstOutSummary)
rm(MSTWeek)
rm(MSTWeekSummary)

shell("cls")
print("Fin procesamiento... ")
