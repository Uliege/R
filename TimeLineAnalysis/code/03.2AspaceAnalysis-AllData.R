#Script para generar datos SDE, SDD y BOX del paquete ASPACE desde los archivos CSV que vienen como resultado de la limpieza en python
#Se incluye la transformación de coordenadas. De grados decimales (GPS) a proyección en metros (ArcGIS)
#Se incluye el mes para conformar los grupos
#Para el ANOVA se toma en cuenta lo siguiente:
#GRUPO: Año + Mes (El individuo debe tener al menos 3 meses con datos)
#MUESTRA o SUJETO: Semana (En cada mes, deben existir al menos 3 semanas con datos)
#RESUMEN: En total el inidividuo debe tener al menos 9 semanas de datos, distribuidas en al menos 3 meses

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

#Archivo para almacenar el log de ejecución (cuando el script se ejecuta en un servidor)
fileLog = paste(urlLogs,"log-03.2-ymw ",trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
#Conexión para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(paste("Start Time: ", as.character(Sys.time())), con = con)

#File's name Pattern
pName = "tld"

#Variables to store results
ymwDataSDEatt = NULL
ymwDataSDEloc = NULL
ymwDataSDDatt = NULL
ymwDataSDDloc = NULL
ymwDataBOXatt = NULL
ymwDataBOXloc = NULL

##############################################

#Loop process
#Start in 1 because 0-file is substituted with 161-file
for(k in 1:269) {
  #Change here to process individual files
  k = 161
  #read csv file generated in python
  idFile = paste(pName,k,sep = "")
  excelFile = paste(urlExcelData,idFile,'.csv',sep="")
  if(file.exists(excelFile)){
    writeLines(paste("File: ", idFile), con = con)
    ymwActivityPoints = read.csv(excelFile)
    #Change name columns
    names(ymwActivityPoints)[3] = 'lat1'
    names(ymwActivityPoints)[4] = 'lon1' 
    names(ymwActivityPoints)[8] = 'idPerson'
    names(ymwActivityPoints)[9] = 'idDate'
    #Transform coordinates (lon, lat) from GPS degrees decimal (4362) to Ecuador projection in meters (32617) 
    coord = select(ymwActivityPoints, lon1, lat1)
    coord = coord %>% st_as_sf(coords = c("lon1", "lat1"))
    coord = st_set_crs(coord, 4326)
    coord = st_transform(coord, 32617)
    coordDf = data.frame(st_coordinates(coord[,1]))
    rm(coord)
    ymwActivityPoints$lon = coordDf$X
    ymwActivityPoints$lat = coordDf$Y
    rm(coordDf)
    
    #Add new columns for the analysis
    ymwActivityPoints$datetime <- ymd_hms(ymwActivityPoints$datetime)
    ymwActivityPoints$weekYear = isoweek(ymwActivityPoints$datetime)
    ymwActivityPoints[month(ymwActivityPoints$datetime)==1 & ymwActivityPoints$weekYear>48, 'weekYear'] = 1
    
    ymwActivityPoints$weekYearChar = '0'
    for(i in 1:length(ymwActivityPoints$weekYear)){
      #i=1
      wy = ymwActivityPoints$weekYear[i]
      if(wy<10){
        ymwActivityPoints$weekYearChar[i] = paste('0',wy,sep = '')
      }else{
        ymwActivityPoints$weekYearChar[i] = wy
      }
    }
    ymwActivityPoints$weekYear = ymwActivityPoints$weekYearChar
    ymwActivityPoints$weekMonth = ceiling(day(ymwActivityPoints$datetime) / 7)
    ymwActivityPoints$weekMonthChar = '0'
    for(i in 1:length(ymwActivityPoints$weekMonth)){
      #i=1
      wm = ymwActivityPoints$weekMonth[i]
      if(wm<10){
        ymwActivityPoints$weekMonthChar[i] = paste('0',wm,sep = '')
      }else{
        ymwActivityPoints$weekMonthChar[i] = wm
      }
    }
    ymwActivityPoints$weekMonth = ymwActivityPoints$weekMonthChar
    ymwActivityPoints$idGroup = gsub("-", "", substring(ymwActivityPoints$idDate, first = 1, last = 7)) 
    
    if(k<10){
      ymwActivityPoints$idPerson = paste(pName, '00', k, sep = '')
    }else if(k<100){
      ymwActivityPoints$idPerson = paste(pName, '0', k, sep = '')
    }
    
    #select columns for the analysis
    ymwActivityPoints = select(ymwActivityPoints, idDate, idPerson, idGroup, weekYear, weekMonth, lon, lat)
    
    #Identify weeks to calculate SDE. (Each week have at least 3 Activity GPS points)
    weeks = ymwActivityPoints %>% 
      group_by(idGroup, weekYear, idPerson) %>%
      count(idGroup, sort = TRUE) %>%
      filter(n > 2)
    
    weeks = weeks %>% arrange(idGroup, weekYear)
    
    if(nrow(weeks) > 0){
      for(i in 1:nrow(weeks)){
        #i=3
        #Weekly variables
        eachWeek = weeks[i,]
        
        #Filter data to calculate SDE weekly
        dataWeek = ymwActivityPoints %>%
          filter(idGroup == eachWeek$idGroup & weekYear == eachWeek$weekYear)
        
        weekMonth = max(dataWeek$weekMonth)
        
        #Select coordinates to calculations
        coord = select(dataWeek, lon, lat)
        
        tryCatch(
          {
            #Compute SDE - Standard Deviation Ellipse
            calc_sde(id = i, centre.xy=NULL, calccentre=TRUE, 
                     weighted=FALSE, weights=NULL, points=coord, 
                     verbose=FALSE, filename = paste(urlOutput,"SDE.txt",sep=""))
            
            #Add columns to SDE attribute result
            sdeatt$idFile = eachWeek$idPerson
            sdeatt$idGroup = eachWeek$idGroup
            sdeatt$weekYear = eachWeek$weekYear
            sdeatt$weekMonth = weekMonth
            sdeatt$activityPoints = eachWeek$n
            
            #Add columns to SDE locations result
            sdeloc$idFile = eachWeek$idPerson
            
            #Join SDE results
            ymwDataSDEatt = rbind(ymwDataSDEatt, sdeatt)
            ymwDataSDEloc = rbind(ymwDataSDEloc, sdeloc)
            
            #Compute SDD - Standard Distance Deviation (Standard Distance)
            calc_sdd(id = i, centre.xy=NULL, calccentre=TRUE, 
                     weighted=FALSE, weights=NULL, points=coord, 
                     verbose=FALSE, filename = paste(urlOutput,"SDD.txt",sep=""))
            
            #Add columns to SDD attribute result
            sddatt$idFile = eachWeek$idPerson
            sddatt$idGroup = eachWeek$idGroup
            sddatt$weekYear = eachWeek$weekYear
            sddatt$weekMonth = weekMonth
            sddatt$activityPoints = eachWeek$n
            
            
            #Add columns to SDD locations result
            sddloc$idFile = eachWeek$idPerson
            
            #Join SDD results
            ymwDataSDDatt = rbind(ymwDataSDDatt, sddatt)
            ymwDataSDDloc = rbind(ymwDataSDDloc, sddloc)
            
            #Compute BOX - Standard Deviation Box
            calc_box(id = i, centre.xy=NULL, calccentre=TRUE, 
                     weighted=FALSE, weights=NULL, points=coord, 
                     verbose=FALSE, filename = paste(urlOutput,"BOX.txt",sep=""))
            
            #Add columns to BOX attribute result
            boxatt$idFile = eachWeek$idPerson
            boxatt$idGroup = eachWeek$idGroup
            boxatt$weekYear = eachWeek$weekYear
            boxatt$weekMonth = weekMonth
            boxatt$activityPoints = eachWeek$n
            
            #Add columns to BOX locations result
            boxloc$idFile = eachWeek$idPerson
            
            #Join BOX results
            ymwDataBOXatt = rbind(ymwDataBOXatt, boxatt)
            ymwDataBOXloc = rbind(ymwDataBOXloc, boxloc) 
            
            #Finish try-catch
          },
        warning = function(w) {
          writeLines(paste("Warning!!",eachWeek$idGroup,eachWeek$weekYear,"no SDE calculation",eachWeek$n,"points"), con = con)
          },
        error = function(e) {
          writeLines(paste("Error!!",eachWeek$idGroup,eachWeek$weekYear,"no SDE calculation",eachWeek$n,"points"), con = con)
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
rm(ymwActivityPoints)
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
fileRawFinal = paste("ymwAspaceTotal",trimws(str_replace_all(as.character(Sys.Date()),":","")),".RData",sep="")
urlSaveRawData = paste(urlRawData,fileRawFinal,sep="")
save(ymwDataBOXatt, ymwDataBOXloc, ymwDataSDDatt, ymwDataSDDloc, ymwDataSDEatt, ymwDataSDEloc, file=urlSaveRawData)

fileRawFinal = paste("ymwDataSDE",trimws(str_replace_all(as.character(Sys.Date()),":","")),".RData",sep="")
urlSaveRawData = paste(urlRawData,fileRawFinal,sep="")
save(ymwDataSDEatt, ymwDataSDEloc,file=urlSaveRawData)

shell("cls")
print("Fin procesamiento... ")
