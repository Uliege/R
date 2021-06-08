#Script para generar datos SDE, SDD y BOX del paquete ASPACE desde los archivos CSV que vienen como resultado de la limpieza en python

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
urlExcelData = paste(urlDefault,"/exceldata/",sep = "")

#Archivo para almacenar el log de ejecución
fileLog = paste("log",trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
#Conexión para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(text = as.character(Sys.time()), con = con)


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
  #Cambiar aqui para procesar archivos individuales
  #k = 8
  #Lectura de datos procesados en python
  #Para filtrar los datos
  idFile = paste(rawName,k,sep = "")
  excelFile = paste(urlExcelData,idFile,'.csv',sep="")
  
  if(file.exists(excelFile)){
    dataPython = read.csv(excelFile)
    dataPython$datetime <- ymd_hms(dataPython$datetime)
    dataPython$Y = year(dataPython$datetime)
    dataPython$M = month(dataPython$datetime)
    dataPython$W = week(dataPython$datetime)
    dataPython$contador = 1
    
    #Datos de cada persona
    dataFile = filter(dataPython, uid == idFile)
    
    #Obtener las semanas de cada año
    Y_W = dataFile %>% 
      group_by(Y,M,W) %>% 
      summarise(numPoints = sum(contador)) %>%
      filter(numPoints >= 0)
    
    #Contador semanas no procesadas
    cont = 0
    
    if(nrow(Y_W) > 0){
      
      #para que se almacenen los archivos de ASPACE en el directorio output
      setwd(urlOutput)
      
      for(i in 1:nrow(Y_W)){
        #i=2 
        
        #Variables de cada semana
        xYear = as.integer(Y_W[i,1])
        xMonth = as.integer(Y_W[i,2])
        xWeek = as.integer(Y_W[i,3])
        xTotal = as.integer(Y_W[i,4])
        
        #Debe existir al menos 3 puntos para calcular ASPACE
        if(xTotal > 2){
          
          #Filtrar datos a procesar
          dataWeek = dataFile %>%
            filter(Y == xYear & W == xWeek)
        
          # latitude - longitude
          coord = select(dataWeek, 3:4)
        
          #Cálculo SDE - Standard Deviation Ellipse      
          nameOutput = paste(idFile,"-",xYear,"-",xMonth,"-",xWeek,"-",xTotal,sep = "")
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
}

#separar columnas
xDataSDEatt = separate(xDataSDEatt, id, c("id","Y","M","W","samples"))
xDataSDEloc = separate(xDataSDEloc, id, c("id","Y","M","W","samples"))
xDataSDDatt = separate(xDataSDDatt, id, c("id","Y","M","W","samples"))
xDataSDDloc = separate(xDataSDDloc, id, c("id","Y","M","W","samples"))
xDataBOXatt = separate(xDataBOXatt, id, c("id","Y","M","W","samples"))
xDataBOXloc = separate(xDataBOXloc, id, c("id","Y","M","W","samples"))

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
rm(dataPython)

fileRawFinal = paste("aspace",trimws(str_replace_all(as.character(Sys.Date()),":","")),".RData",sep="")
urlSaveRawData = paste(urlRawData,fileRawFinal,sep="")
save(xDataBOXatt, xDataBOXloc, xDataSDDatt, xDataSDDloc, xDataSDEatt, xDataSDEloc,file=urlSaveRawData)

fileRawFinal = paste("aspaceSDEx",trimws(str_replace_all(as.character(Sys.Date()),":","")),".RData",sep="")
urlSaveRawData = paste(urlRawData,fileRawFinal,sep="")
save(xDataSDEatt, xDataSDEloc,file=urlSaveRawData)

shell("cls")
print("Fin procesamiento... ")