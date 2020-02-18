#install.packages("rlang")
# install dev version of ggmap
#devtools::install_github("dkahle/ggmap")

library(dplyr)
library(jsonlite)
library(VIM)
library(lubridate)
library(ggmap)

print("Inicio..")
urlData = "/home/giovanny/SmartGPSAnalysis/data/"
urlFigures = "/home/giovanny/SmartGPSAnalysis/figures/"
urlRawData = "/home/giovanny/SmartGPSAnalysis/rawdata/"
urlReports = "/home/giovanny/SmartGPSAnalysis/reports/"

print("Start..")
print("Script: loadData.R")
print(Sys.time())

outputFile = paste(urlReports,"outputLoad.txt",sep = "")

print("Abro archivo")
sink(file = outputFile)
print("Start..")
print("Script: loadData.R")
print(Sys.time())
sink()
print("cierro archivo")

readJsonFile <- function(fileName) {
  dataResult <- NULL
  
  archivo <- paste(urlData,fileName,sep = "")
  con <- file(archivo, open="r")
  dataJson <- readLines(con)
  close(con)
  
  dataFile <- fromJSON(dataJson)
  dspId <- dataFile$dspId
  dspLat <- dataFile$sensor$latitude
  dspLong <- dataFile$sensor$longitude
  dspNSat <- dataFile$sensor$numSatelites
  dspAlti <- dataFile$sensor$altitude
  dspVelo <- dataFile$sensor$velocidad
  dspActy <- dataFile$sensor$actividad
  dspAccu <- dataFile$sensor$precision
  dspFcIn <- dataFile$sensor$dateInsert
  dspFcUp <- dataFile$sensor$dateUpdate
  dspPSta <- dataFile$sensor$providerStatus
  
  dataFile <- data.frame(dspId, dspLat, dspLong, dspNSat, dspAlti, dspVelo, dspActy, dspAccu, dspFcIn, dspFcUp, dspPSta)
  
  return(dataFile)
}

print("Reading JSON files...")
print("Abro Archivo")
sink(file = outputFile, append = TRUE)

print("Reading JSON files...")
print(Sys.time())

print("Loading 1...")
data1 <- readJsonFile("data1.json")
print("Loading 1... OK")

print("Reading JSON files Ok")
print(Sys.time())

sink()
print("Cierro archivo")

print("Reading JSON files Ok")

print("Joining Data - dataSource")

dataSource <- data1

print("Abro archivo")
sink(file = outputFile, append = TRUE)

print("***********************************")
print("***********************************")
print("Type of DataSource")
print(typeof(dataSource))
print("Number of rows")
print(nrow(dataSource))
print("Number of columns")
print(ncol(dataSource))
print("Structure")
print(str(dataSource))
print("Summary")
print(summary(dataSource))
print("Summary Missing Data")
print(summary(aggr(dataSource, only.miss = TRUE, sortVars = TRUE, plot = FALSE)))
print("***********************************")
print("***********************************")



print("Script Finalizado")
print(Sys.time())
sink()
print("Cierro archivo")
print("End Script!!!")
print(Sys.time())
