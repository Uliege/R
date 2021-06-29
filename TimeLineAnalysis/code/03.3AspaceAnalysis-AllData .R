#Script para generar datos SDE, SDD y BOX del paquete ASPACE desde los archivos CSV que vienen como resultado de la limpieza en python
#Se incluye la transformación de coordenadas. De grados decimales (GPS) a proyección en metros (ArcGIS)
#Se incluye el día de la semana para conformar los grupos (weekdays, weekends)
#Para el ANOVA se toma en cuenta lo siguiente:
#GRUPO: weekday, weekend (El individuo debe tener al menos 3 semanas con datos)
#MUESTRA o SUJETO: Semana (En cada semana, deben existir al menos 3 días con datos)
#RESUMEN: En total el individuo debe tener al menos 9 semanas de datos,

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
fileLog = paste(urlLogs,"log-03.3-wdwe ",trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
#Conexión para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(paste("Start Time: ", as.character(Sys.time())), con = con)

#File's name Pattern
pName = "tld"

#Variables to store results
wdweDataSDEatt = NULL
wdweDataSDEloc = NULL
wdweDataSDDatt = NULL
wdweDataSDDloc = NULL
wdweDataBOXatt = NULL
wdweDataBOXloc = NULL

##############################################

#Loop process
#Start in 1 because 0-file is substituted with 161-file
for(k in 1:269) {
  #Change here to process individual files
  #k = 161
  #read csv file generated in python
  idFile = paste(pName,k,sep = "")
  excelFile = paste(urlExcelData,idFile,'.csv',sep="")
  if(file.exists(excelFile)){
    writeLines(paste("File: ", idFile), con = con)
    wdweActivityPoints = read.csv(excelFile)
    #Change name columns
    names(wdweActivityPoints)[3] = 'lat1'
    names(wdweActivityPoints)[4] = 'lon1' 
    names(wdweActivityPoints)[8] = 'idPerson'
    names(wdweActivityPoints)[9] = 'idDate'
    #Transform coordinates (lon, lat) from GPS degrees decimal (4362) to Ecuador projection in meters (32617) 
    coord = select(wdweActivityPoints, lon1, lat1)
    coord = coord %>% st_as_sf(coords = c("lon1", "lat1"))
    coord = st_set_crs(coord, 4326)
    coord = st_transform(coord, 32617)
    coordDf = data.frame(st_coordinates(coord[,1]))
    rm(coord)
    wdweActivityPoints$lon = coordDf$X
    wdweActivityPoints$lat = coordDf$Y
    rm(coordDf)
    
    #Add new columns for the analysis
    wdweActivityPoints$datetime <- ymd_hms(wdweActivityPoints$datetime)
    wdweActivityPoints$weekYear = isoweek(wdweActivityPoints$datetime)
    wdweActivityPoints$weekYearChar = '0'
    for(i in 1:length(wdweActivityPoints$weekYear)){
      #i=1
      wy = wdweActivityPoints$weekYear[i]
      if(wy<10){
        wdweActivityPoints$weekYearChar[i] = paste('0',wy,sep = '')
      }else{
        wdweActivityPoints$weekYearChar[i] = wy
      }
    }
    wdweActivityPoints$weekYear = wdweActivityPoints$weekYearChar
    
    wdweActivityPoints$weekMonth = ceiling(day(wdweActivityPoints$datetime) / 7)
    wdweActivityPoints$weekMonthChar = '0'
    for(i in 1:length(wdweActivityPoints$weekMonth)){
      #i=1
      wm = wdweActivityPoints$weekMonth[i]
      if(wm<10){
        wdweActivityPoints$weekMonthChar[i] = paste('0',wm,sep = '')
      }else{
        wdweActivityPoints$weekMonthChar[i] = wm
      }
    }
    wdweActivityPoints$weekMonth = wdweActivityPoints$weekMonthChar
    
    
    wdweActivityPoints$dayWeek = wday(wdweActivityPoints$datetime, week_start = getOption("lubridate.week.start", 1))
    wdweActivityPoints =  wdweActivityPoints %>%
      mutate(typeWeek = ifelse(dayWeek <= 5, 1, 2))
    
    
    wdweActivityPoints$idGroup = gsub("-", "", substring(wdweActivityPoints$idDate, first = 1, last = 7)) 
    
    if(k<10){
      wdweActivityPoints$idPerson = paste(pName, '00', k, sep = '')
    }else if(k<100){
      wdweActivityPoints$idPerson = paste(pName, '0', k, sep = '')
    }
    
    #select columns for the analysis
    wdweActivityPoints = select(wdweActivityPoints, idDate, idPerson, idGroup, weekYear, weekMonth, typeWeek, lon, lat)
    
    #Identify weeks to calculate SDE. (Each week have at least 3 Activity GPS points)
    weeks = wdweActivityPoints %>% 
      group_by(idGroup, weekYear, idPerson, typeWeek) %>%
      count(idGroup, sort = TRUE) %>%
      filter(n > 2) %>% 
      arrange(idGroup, weekYear)
    
    if(nrow(weeks) > 0){
      for(i in 1:nrow(weeks)){
        #i=1
        #Weekly variables
        eachWeek = weeks[i,]
        
        #Filter data to calculate SDE weekly
        dataWeek = wdweActivityPoints %>%
          filter(idGroup == eachWeek$idGroup) %>% 
          filter(weekYear == eachWeek$weekYear) %>%
          filter(typeWeek == eachWeek$typeWeek)
        
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
            sdeatt$typeWeek = eachWeek$typeWeek
            sdeatt$activityPoints = eachWeek$n
            
            #Add columns to SDE locations result
            sdeloc$idFile = eachWeek$idPerson
            
            #Join SDE results
            wdweDataSDEatt = rbind(wdweDataSDEatt, sdeatt)
            wdweDataSDEloc = rbind(wdweDataSDEloc, sdeloc)
            
            #Compute SDD - Standard Distance Deviation (Standard Distance)
            calc_sdd(id = i, centre.xy=NULL, calccentre=TRUE, 
                     weighted=FALSE, weights=NULL, points=coord, 
                     verbose=FALSE, filename = paste(urlOutput,"SDD.txt",sep=""))
            
            #Add columns to SDD attribute result
            sddatt$idFile = eachWeek$idPerson
            sddatt$idGroup = eachWeek$idGroup
            sddatt$weekYear = eachWeek$weekYear
            sddatt$weekMonth = weekMonth
            sddatt$typeWeek = eachWeek$typeWeek
            sddatt$activityPoints = eachWeek$n
            
            
            #Add columns to SDD locations result
            sddloc$idFile = eachWeek$idPerson
            
            #Join SDD results
            wdweDataSDDatt = rbind(wdweDataSDDatt, sddatt)
            wdweDataSDDloc = rbind(wdweDataSDDloc, sddloc)
            
            #Compute BOX - Standard Deviation Box
            calc_box(id = i, centre.xy=NULL, calccentre=TRUE, 
                     weighted=FALSE, weights=NULL, points=coord, 
                     verbose=FALSE, filename = paste(urlOutput,"BOX.txt",sep=""))
            
            #Add columns to BOX attribute result
            boxatt$idFile = eachWeek$idPerson
            boxatt$idGroup = eachWeek$idGroup
            boxatt$weekYear = eachWeek$weekYear
            boxatt$weekMonth = weekMonth
            boxatt$typeWeek = eachWeek$typeWeek
            boxatt$activityPoints = eachWeek$n
            
            #Add columns to BOX locations result
            boxloc$idFile = eachWeek$idPerson
            
            #Join BOX results
            wdweDataBOXatt = rbind(wdweDataBOXatt, boxatt)
            wdweDataBOXloc = rbind(wdweDataBOXloc, boxloc) 
            
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
rm(wdweActivityPoints)
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
fileRawFinal = paste("wdweAspaceTotal",trimws(str_replace_all(as.character(Sys.Date()),":","")),".RData",sep="")
urlSaveRawData = paste(urlRawData,fileRawFinal,sep="")
save(wdweDataBOXatt, wdweDataBOXloc, wdweDataSDDatt, wdweDataSDDloc, wdweDataSDEatt, wdweDataSDEloc, file=urlSaveRawData)

fileRawFinal = paste("wdweDataSDE",trimws(str_replace_all(as.character(Sys.Date()),":","")),".RData",sep="")
urlSaveRawData = paste(urlRawData,fileRawFinal,sep="")
save(wdweDataSDEatt, wdweDataSDEloc,file=urlSaveRawData)

shell("cls")
print("Fin procesamiento... ")
