#Script para generar el dataset TOTAL, unión de todos los archivos (RData)

library(stringr)

nameScript = "ProcessJoinRaw"

#Directorios (url) de trabajo del proyecto
urlDefault = getwd()
urlData = paste(urlDefault,"/data/",sep = "")
urlFigures = paste(urlDefault,"/figures/",sep = "")
urlModels = paste(urlDefault,"/models/",sep = "")
urlRawData = paste(urlDefault,"/rawdata/",sep = "")
urlReports = paste(urlDefault,"/reports/",sep = "")
urlOutput = paste(urlDefault,"/output/",sep = "")

#Archivo para almacenar el log de ejecución
fileLog = paste("log",nameScript,trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
#Conexión para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(text = as.character(Sys.time()), con = con)

#Especifica el nombre de los archivos raw
rawName = "tld"

#Variables donde se almacenarán los resultados 
dataQuitoTotal = NULL

#Proceso repetitivo
for(k in 0:160) {
  #k=20
  msg = k
  print(msg)
  #writeLines(text = as.character(msg), con = con)
  
  #Leer el archivo raw#
  rawNumber = k
  idFile = paste(rawName,rawNumber,sep = "")
  fileRawData = paste(idFile,".RData",sep="")
  load(file=paste(urlRawData,fileRawData,sep = ""))
  if(ncol(dataQuito) == 21){
    dataQuito$id = idFile
    dataQuitoTotal = rbind(dataQuitoTotal, dataQuito)
  }else{
    msg = paste("El archivo ", k, " no se concatena porque tiene ", ncol(dataQuito), " columnas", sep = "")
    writeLines(text = as.character(msg), con = con)
  }
}

#Guardamos los archivos generados
fileRawFinal = paste("tldTotal",".RData",sep="")
urlSaveRawData = paste(urlRawData,fileRawFinal,sep="")
save(dataQuitoTotal, file=urlSaveRawData)
writeLines(text = as.character(Sys.time()), con = con)
close(con)
shell("cls")
print("Fin procesamiento... ")


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

rawName = "tld"
idFile = paste(rawName,"Total",sep = "")
fileRawData = paste(idFile,".RData",sep="")
load(file=paste(urlRawData,fileRawData,sep = ""))



#############################################################################
#############################################################################
#############################################################################
#############################################################################







