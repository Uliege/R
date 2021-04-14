#Script para generar datos de SDE y SDD del paquete ASPACE desde los archivos CSV que vienen como resultado de la limpieza en python

library(aspace)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)

#Directorios (url) de trabajo del proyecto
urlDefault = getwd()
urlData = paste(urlDefault,"/data/",sep = "")
urlFigures = paste(urlDefault,"/figures/",sep = "")
urlModels = paste(urlDefault,"/models/",sep = "")
urlRawData = paste(urlDefault,"/rawdata/",sep = "")
urlReports = paste(urlDefault,"/reports/",sep = "")
urlOutput = paste(urlDefault,"/output/",sep = "")

#Archivo para almacenar el log de ejecución
fileLog = paste("log",trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
#Conexión para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(text = as.character(Sys.time()), con = con)


#Lectuar de datos procesados en python
pathPython = 'D:/python/TimelineGPSData-master/my_data.csv'
dataPython = read.csv(pathPython)

dataPython$datetime <- ymd_hms(dataPython$datetime)

dataPython$Y = year(dataPython$datetime)
dataPython$M = month(dataPython$datetime)
dataPython$W = week(dataPython$datetime)
dataPython$contador = 1

#Especifica el nombre de los archivos raw
rawName = "tld"

#Variables para almacenar los resultados
xDataSDEatt = NULL
xDataSDEloc = NULL
xDataSDDatt = NULL
xDataSDDloc = NULL
xDataBOXatt = NULL
xDataBOXloc = NULL

Y_W = NULL

##############################################

#Proceso repetitivo
for(k in 0:160) {
  #k = 0
  
  #Para filtrar los datos
  idFile = paste(rawName,k,sep = "")
  fileRawData = paste(idFile,".RData",sep="")
  
  #Datos de cada persona
  dataFile = filter(dataPython, uid == idFile)
  
  #Obtener las semanas de cada año
  Y_W = dataFile %>% 
    group_by(Y,W) %>% 
    summarise(numPoints = sum(contador)) %>%
    filter(numPoints >= 0)
  
  #Contador semanas no procesadas
  cont = 0
  
  if(nrow(Y_W) > 0){
    
    #para que se almacenen los archivos de ASPACE en el directorio output
    setwd(urlOutput)
    
    for(i in 1:nrow(Y_W)){
      #i=1 
      
      #Variables de cada semana
      xYear = as.integer(Y_W[i,1])
      xWeek = as.integer(Y_W[i,2])
      xTotal = as.integer(Y_W[i,3])
      
      #Debe existir al menos 2 puntos para calcular ASPACE
      if(xTotal > 1){
        
        #Filtrar datos a procesar
        dataWeek = dataFile %>%
          filter(Y == xYear & W == xWeek)
      
        # latitude - longitude
        coord = select(dataWeek, 3:4)
      
        #Cálculo SDE - Standard Deviation Ellipse      
        nameOutput = paste(idFile,"-",xYear,"-",xWeek,"-",nrow(dataWeek),sep = "")
        calc_sde(id=nameOutput, filename = paste("SDEloc_",nameOutput,"_Output.txt",sep=""), centre.xy=NULL, 
                 calccentre=TRUE, weighted=FALSE, weights=NULL, points=coord, verbose=FALSE)
        
        #Concatenar resultado
        xDataSDEatt = rbind(xDataSDEatt, sdeatt)
        xDataSDEloc = rbind(xDataSDEloc, sdeloc)
        
        #Cálculo SDD - Standard Distance Deviation (Standard Distance)
        calc_sdd(id=nameOutput, filename = paste("SDDloc_",nameOutput,"_Output.txt",sep=""), centre.xy=NULL, 
                 calccentre=TRUE, weighted=FALSE, weights=NULL, points=coord, verbose=FALSE)
        #Concatenar resultado
        xDataSDDatt = rbind(xDataSDDatt, sddatt)
        xDataSDDloc = rbind(xDataSDDloc, sddloc)
        
        #Cálculo BOX - Standard Deviation Box
        calc_box(id=nameOutput, filename = paste("BOXloc_",nameOutput,"_Output.txt",sep=""), centre.xy=NULL, 
                 calccentre=TRUE, weighted=FALSE, weights=NULL, points=coord, verbose=FALSE)
        #Concatenar resultado
        xDataBOXatt = rbind(xDataBOXatt, boxatt)
        xDataBOXloc = rbind(xDataBOXloc, boxloc)
      }else{
        cont=cont+1
      }
      
    }
    msg = paste("Archivo ",idFile," - ",nrow(Y_W) - cont," semanas procesadas, ", cont," NO procesadas",sep = "")
    print(msg)
    writeLines(text = msg, con = con)
    writeLines(text = as.character(Sys.time()), con = con)
    #Volver al directorio por defecto
    setwd(urlDefault)
  }else{
    msg = paste("Archivo ",idFile," - 0 semanas procesados",sep = "")
    print(msg)
    writeLines(text = msg, con = con)
  }
}

#separar columnas
xDataSDEatt = separate(xDataSDEatt, id, c("id","Y","W","samples"))
xDataSDEloc = separate(xDataSDEloc, id, c("id","Y","W","samples"))
xDataSDDatt = separate(xDataSDDatt, id, c("id","Y","W","samples"))
xDataSDDloc = separate(xDataSDDloc, id, c("id","Y","W","samples"))
xDataBOXatt = separate(xDataBOXatt, id, c("id","Y","W","samples"))
xDataBOXloc = separate(xDataBOXloc, id, c("id","Y","W","samples"))

#Cerramos conexión del archivo LOG
close(con)

#Eliminamos variables
rm(dataWeek)
rm(coord)
rm(Y_W)
rm(boxatt)
rm(boxloc)
rm(r.BOX)
rm(r.SDD)
rm(r.SDE)
rm(sddatt)
rm(sddloc)
rm(sdeatt)
rm(sdeloc)
rm(dataFile)

