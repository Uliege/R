#Script para generar datos SDE, SDD y BOX del paquete ASPACE desde los archivos CSV que vienen como resultado de la limpieza en python
#Se incluye la transformación de coordenadas. De grados decimales (GPS) a proyección en metros (ArcGIS)

#Delete  work space variables (Caution!!)
rm(list=ls())
shell("cls")

library(aspace)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)

#Directorios (url) de trabajo del proyecto
#urlProyecto = 'D:/G/GitHub/Uliege/R/TimeLineAnalysis/'
urlProyecto = getwd()
urlData = paste(urlProyecto,"/data/",sep = "")
urlDownloads = "C:/Users/gmonc/Downloads/"
urlExcelData = paste(urlProyecto,"/exceldata/",sep = "")
urlFigures = paste(urlProyecto,"/figures/",sep = "")
urlModels = paste(urlProyecto,"/models/",sep = "")
urlOutput = paste(urlProyecto,"/output/",sep = "")
urlRawData = paste(urlProyecto,"/rawdata/",sep = "")
urlReports = paste(urlProyecto,"/reports/",sep = "")
urlShapeData = paste(urlProyecto,"/shapeData/",sep = "")
urlLogs = paste(urlProyecto,"/logs/",sep = "")

#Archivo para almacenar el log de ejecución (noProcWeeksrol cuando el script se ejecuta en un servidor)
fileLog = paste(urlLogs,"log",trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
#Conexión para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(paste("Start Time: ", as.character(Sys.time())), con = con)

#File's name Pattern
pName = "tld"

#Variables to store results
tDataSDEatt = NULL
tDataSDEloc = NULL
tDataSDDatt = NULL
tDataSDDloc = NULL
tDataBOXatt = NULL
tDataBOXloc = NULL

##############################################

#Loop process
#Start in 1 because 0-file is substituted with 161-file
for(k in 1:269) {
  #Change here to process individual files
  #k = 63
  #read csv file generated in python
  idFile = paste(pName,k,sep = "")
  excelFile = paste(urlExcelData,idFile,'.csv',sep="")
  if(file.exists(excelFile)){
    writeLines(paste("File: ", idFile), con = con)
    activityPoints = read.csv(excelFile)
    #Change name columns
    names(activityPoints)[4] = 'lon' 
    names(activityPoints)[8] = 'id' 
    #select columns for the analysis
    activityPoints = select(activityPoints, id, datetime, lon, lat)
    #Transform coordinates (lon, lat) from GPS degrees decimal (4362) to Ecuador projection in meters (32617) 
    coord = select(activityPoints, lon, lat)
    coord = coord %>% st_as_sf(coords = c("lon", "lat"))
    coord = st_set_crs(coord, 4326)
    coord = st_transform(coord, 32617)
    coordDf = data.frame(st_coordinates(coord[,1]))
    rm(coord)
    activityPoints$lon = coordDf$X
    activityPoints$lat = coordDf$Y
    rm(coordDf)
    #Add new columns for the analysis
    activityPoints$datetime <- ymd_hms(activityPoints$datetime)
    activityPoints$Y = year(activityPoints$datetime)
    activityPoints$W = week(activityPoints$datetime)
    activityPoints$idWeek = paste(activityPoints$Y,activityPoints$W,sep = "") 
    activityPoints$counter = 1
    
    #Identify weeks to calculate SDE.
    weeks = activityPoints %>% 
      group_by(idWeek, Y, W) %>% 
      summarise(numPoints = sum(counter)) %>%
      filter(numPoints > 2)
    
    if(nrow(weeks) > 0){
      for(i in 1:nrow(weeks)){
        #i=4
        #Weekly variables
        eachWeek = weeks[i,]
        numWeek = as.numeric(eachWeek$idWeek)
        numPoints = eachWeek$numPoints
        
        #Filter data to calculate SDE weekly
        dataWeek = activityPoints %>%
          filter(idWeek == numWeek)
        
        #Select coordinates to calculations
        coord = select(dataWeek, lon, lat)
        
        tryCatch(
          {
            #Compute SDE - Standard Deviation Ellipse
            calc_sde(id = numWeek, centre.xy=NULL, calccentre=TRUE, 
                     weighted=FALSE, weights=NULL, points=coord, 
                     verbose=FALSE, filename = paste(urlOutput,"SDE.txt",sep=""))
            
            #Add columns to SDE attribute result
            sdeatt$idFile = idFile
            sdeatt$year = eachWeek$Y
            sdeatt$week = eachWeek$W
            sdeatt$numPoints = eachWeek$numPoints
            
            #Add columns to SDE locations result
            sdeloc$idFile = idFile
            
            #Join SDE results
            tDataSDEatt = rbind(tDataSDEatt, sdeatt)
            tDataSDEloc = rbind(tDataSDEloc, sdeloc)
            
            #Compute SDD - Standard Distance Deviation (Standard Distance)
            calc_sdd(id = numWeek, centre.xy=NULL, calccentre=TRUE, 
                     weighted=FALSE, weights=NULL, points=coord, 
                     verbose=FALSE, filename = paste(urlOutput,"SDD.txt",sep=""))
            
            #Add columns to SDD attribute result
            sddatt$idFile = idFile
            sddatt$year = eachWeek$Y
            sddatt$week = eachWeek$W
            sddatt$numPoints = eachWeek$numPoints
            
            #Add columns to SDD locations result
            sddloc$idFile = idFile
            
            #Join SDD results
            tDataSDDatt = rbind(tDataSDDatt, sddatt)
            tDataSDDloc = rbind(tDataSDDloc, sddloc)
            
            #Compute BOX - Standard Deviation Box
            calc_box(id = numWeek, centre.xy=NULL, calccentre=TRUE, 
                     weighted=FALSE, weights=NULL, points=coord, 
                     verbose=FALSE, filename = paste(urlOutput,"BOX.txt",sep=""))
            
            #Add columns to BOX attribute result
            boxatt$idFile = idFile
            boxatt$year = eachWeek$Y
            boxatt$week = eachWeek$W
            boxatt$numPoints = eachWeek$numPoints
            
            #Add columns to BOX locations result
            boxloc$idFile = idFile
            
            #Join BOX results
            tDataBOXatt = rbind(tDataBOXatt, boxatt)
            tDataBOXloc = rbind(tDataBOXloc, boxloc) 
            
            #Finish try-catch
          },
        warning = function(w) {
          writeLines(paste("Warning!!","week",numWeek,"no SDE calculation",numPoints,"points"), con = con)
          },
        error = function(e) {
          writeLines(paste("ERROR!!","week",numWeek,"no SDE calculation",numPoints,"points"), con = con)
          }
        )
      }
      msg = paste(idFile, ":", nrow(weeks), "weeks, rest the Warnings!!")
      writeLines(msg, con = con)
      print(msg)
    }else{
      msg = paste(idFile, ":", "NO weeks to process")
      writeLines(msg, con = con)
      print(msg)
    }
  } 
}
msg = paste("Fin del proceso")
print(msg)
writeLines(paste("End Time: ", as.character(Sys.time())), con = con)

#Cerramos conexión del archivo LOG
close(con)

#Eliminamos variables
rm(activityPoints)
rm(boxatt)
rm(boxloc)
rm(coord)
rm(dataWeek)
rm(eachWeek)
rm(r.BOX)
rm(r.SDD)
rm(r.SDE)
rm(sddatt)
rm(sddloc)
rm(sdeatt)
rm(sdeloc)
rm(weeks)

#Export results
fileRawFinal = paste("aspaceTotal",trimws(str_replace_all(as.character(Sys.Date()),":","")),".RData",sep="")
urlSaveRawData = paste(urlRawData,fileRawFinal,sep="")
save(tDataBOXatt, tDataBOXloc, tDataSDDatt, tDataSDDloc, tDataSDEatt, tDataSDEloc, file=urlSaveRawData)

fileRawFinal = paste("tDataSDE",trimws(str_replace_all(as.character(Sys.Date()),":","")),".RData",sep="")
urlSaveRawData = paste(urlRawData,fileRawFinal,sep="")
save(tDataSDEatt, tDataSDEloc,file=urlSaveRawData)

shell("cls")
print("Fin procesamiento... ")