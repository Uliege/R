#Script para generar datos de SDE y SDD del paquete ASPACE desde los archivos raw (RData)

library(aspace)
library(stringr)
library(dplyr)
library(tidyr)


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

#Especifica el nombre de los archivos raw
rawName = "tld"

#Variable para indicar el mínimo de puntos por semana necesarios para el procesamiento
minSamplesWeek = 350
minSamples = minSamplesWeek * 4

#Variables donde se almacenarán los resultados 
wDataCluster = NULL
wDataSDEatt = NULL
wDataSDEloc = NULL
wDataSDDatt = NULL
wDataSDDloc = NULL
wDataBOXatt = NULL
wDataBOXloc = NULL

#Parámetros K-Mean Cluster
numCenters = 3
numStart = 50

#Leemos el archivo con todos los datos

idFile = paste(rawName,"Total",sep = "")
fileRawData = paste(idFile,".RData",sep="")
load(file=paste(urlRawData,fileRawData,sep = ""))


#Proceso repetitivo
for(k in 0:160) {
  #k=0
  #msg = k
  #print(msg)
  #writeLines(text = as.character(msg), con = con)
  
  #Leer el archivo raw#
  idNumber = k
  idFile = paste(rawName,idNumber,sep = "")
  dataQuito = filter(dataQuitoTotal, id == idFile)
  
  Y_W = NULL
  
  if(nrow(dataQuito) > minSamples){
    
    #Variable para contar registros
    dataQuito$contador = 1
    
    #Sacamos años y semanas a procesar que tengan mínimo número de regs
    Y_W = dataQuito %>% 
      group_by(Y,W) %>% 
      summarise(total = sum(contador)) %>%
      filter(total >= minSamplesWeek)
    
    if(nrow(Y_W) > 0){
      #para que se almacenen los archivos de ASPACE en el directorio output
      setwd(urlOutput)
      
      for(i in 1:nrow(Y_W)){
        #i=1
        #Variables de cada semana
        xYear = as.integer(Y_W[i,1])
        xWeek = as.integer(Y_W[i,2])
        xTotal = as.integer(Y_W[i,3])
        
        #Filtrar datos a procesar
        dataWeek = dataQuito %>%
          filter(Y == xYear & W == xWeek)
        
        # latitude - longitude
        xCoord = select(dataWeek, 2:3)
        
        #Cálculo SDE - Standard Deviation Ellipse      
        nameOutput = paste(idFile,"-",xYear,"-",xWeek,"-",nrow(dataWeek),sep = "")
        calc_sde(id=nameOutput, filename = paste("SDEloc_",nameOutput,"_Output.txt",sep=""), centre.xy=NULL, 
                 calccentre=TRUE, weighted=FALSE, weights=NULL, points=xCoord, verbose=FALSE)
        #Concatenar resultado
        wDataSDEatt = rbind(wDataSDEatt, sdeatt)
        wDataSDEloc = rbind(wDataSDEloc, sdeloc)
        
        #Cálculo SDD - Standard Distance Deviation (Standard Distance)
        calc_sdd(id=nameOutput, filename = paste("SDDloc_",nameOutput,"_Output.txt",sep=""), centre.xy=NULL, 
                 calccentre=TRUE, weighted=FALSE, weights=NULL, points=xCoord, verbose=FALSE)
        #Concatenar resultado
        wDataSDDatt = rbind(wDataSDDatt, sddatt)
        wDataSDDloc = rbind(wDataSDDloc, sddloc)
        
        #Cálculo BOX - Standard Deviation Box
        calc_box(id=nameOutput, filename = paste("BOXloc_",nameOutput,"_Output.txt",sep=""), centre.xy=NULL, 
                 calccentre=TRUE, weighted=FALSE, weights=NULL, points=xCoord, verbose=FALSE)
        #Concatenar resultado
        wDataBOXatt = rbind(wDataBOXatt, boxatt)
        wDataBOXloc = rbind(wDataBOXloc, boxloc)
        
        #Cálculo K-Mean Cluster
        km_clusters <- kmeans(x = xCoord, centers = numCenters, nstart = numStart)
        km_clustersFrame = as.data.frame(km_clusters$centers)
        km_clustersFrame$id = nameOutput
        wDataCluster = rbind(wDataCluster, km_clustersFrame)
      }
      #Volver al directorio por defecto
      setwd(urlDefault)
    }else{
      msg = paste("Archivo ",idFile," - 0 semanas procesadas. Pocos datos en cada semana",sep = "")
      print(msg)
      writeLines(text = msg, con = con)
    }
  }else{
    msg = paste("Archivo ",idFile," - No procesado. Pocos datos",sep = "")
    print(msg)
    writeLines(text = msg, con = con)
  }
  msg = paste("Archivo ",idFile," - ",nrow(Y_W)," semanas procesadas",sep = "")
  print(msg)
  writeLines(text = msg, con = con)
}

#separar columnas
wDataCluster = separate(wDataCluster, id, c("id","Y","W","samples"))
wDataSDEatt = separate(wDataSDEatt, id, c("id","Y","W","samples"))
wDataSDEloc = separate(wDataSDEloc, id, c("id","Y","W","samples"))
wDataSDDatt = separate(wDataSDDatt, id, c("id","Y","W","samples"))
wDataSDDloc = separate(wDataSDDloc, id, c("id","Y","W","samples"))
wDataBOXatt = separate(wDataBOXatt, id, c("id","Y","W","samples"))
wDataBOXloc = separate(wDataBOXloc, id, c("id","Y","W","samples"))

#Guardamos los archivos generados
fileRawFinal = paste("aspace",trimws(str_replace_all(as.character(Sys.Date()),":","")),".RData",sep="")
urlSaveRawData = paste(urlRawData,fileRawFinal,sep="")
save(wDataBOXatt, wDataBOXloc, wDataCluster, wDataSDDatt, wDataSDDloc, wDataSDEatt, wDataSDEloc,file=urlSaveRawData)
writeLines(text = as.character(Sys.time()), con = con)
close(con)
shell("cls")
print("Fin procesamiento... ")

rm(boxatt, boxloc, dataQuito, dataWeek, km_clusters, km_clustersFrame, r.BOX, r.SDD, r.SDE, sddatt, sddloc, sdeatt, sdeloc, xCoord, Y_W)
rm(con, fileLog, fileRawData, fileRawFinal, i, idFile, idNumber, k, minSamples, minSamplesWeek, msg, nameOutput, numCenters, numStart)
rm(rawName, xTotal, xWeek, xYear)


#############################################################################
#############################################################################
#############################################################################
#############################################################################

#Para leer en memoria el archivo con el total de datos

urlDefault = getwd()
urlData = paste(urlDefault,"/data/",sep = "")
urlFigures = paste(urlDefault,"/figures/",sep = "")
urlModels = paste(urlDefault,"/models/",sep = "")
urlRawData = paste(urlDefault,"/rawdata/",sep = "")
urlReports = paste(urlDefault,"/reports/",sep = "")
urlOutput = paste(urlDefault,"/output/",sep = "")

rawName = "aspace2020-03-27"
fileRawData = paste(rawName,".RData",sep="")
load(file=paste(urlRawData,fileRawData,sep = ""))

#############################################################################
#############################################################################
#############################################################################
#############################################################################










str(wDataBOXatt)

str(dataQuitoTotal)

summary(dataQuitoTotal)

dataQuitoTotal$contador = 1


data = ggplot(dataQuitoTotal)

data + geom_bar(aes(activity1))

data + geom_bar(aes(Y))

data + geom_bar(aes(W))

numData.Total = dataQuitoTotal %>% 
  group_by(id) %>% 
  summarise(total = sum(contador))



dataQuitoTotal.no.na <- na.omit(dataQuitoTotal)

summary(dataQuitoTotal.no.na)

numData.no.na = dataQuitoTotal.no.na %>% 
  group_by(id) %>% 
  summarise(total = sum(contador))


dataNum = dataQuitoTotal.no.na[4:8]

cov(dataNum)

cor(dataNum)

data.no.na = ggplot(dataQuitoTotal.no.na)

data.no.na + geom_bar(aes(activity1))

data.no.na + geom_bar(aes(velocity))

data.no.na + geom_bar(aes(accuracy))

data.no.na + geom_bar(aes(heading))

data.no.na + geom_bar(aes(Y))




