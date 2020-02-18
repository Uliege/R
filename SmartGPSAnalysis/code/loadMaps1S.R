library(dplyr)
library(jsonlite)
library(VIM)
library(lubridate)
library(ggmap)
library(sqldf)

urlData = "/home/giovanny/SmartGPSAnalysis/data/"
urlFigures = "/home/giovanny/SmartGPSAnalysis/figures/"
urlRawData = "/home/giovanny/SmartGPSAnalysis/rawdata/"
urlReports = "/home/giovanny/SmartGPSAnalysis/reports/"

print("Start..")
print("Script: loadMaps.R")
print(Sys.time())

outputFile = paste(urlReports,"outputMaps.txt",sep = "")

sink(file = outputFile)
print("Start..")
print("Script: loadMaps.R")
print(Sys.time())
sink()

print("Reading CSV files OK!!")
print(Sys.time())
print("Maps of Quito")

sink(outputFile, append = TRUE)
print("Register API Google Maps")

print("Query Maps of Quito")
mapQuito <- get_map(location = c(long= -78.418674, lat = -0.183446), zoom = 11)
roadMapQuito <- get_map(location = c(long= -78.418674, lat = -0.183446), zoom = 11, maptype = "roadmap") 
satelliteMapQuito <- get_map(location = c(long= -78.418674, lat = -0.183446), zoom = 11, maptype = "satellite") 

sink()
print("Maps of Quito OK!!")
print("Saving Data...")

sink(outputFile, append = TRUE)
print("Saving Data...")
save(mapQuito,
     roadMapQuito,
     satelliteMapQuito,
     file=paste(urlRawData,"SmartGPSMaps.RData",sep = ""))

print("Saving Data OK!!!")
print("End Script!!!")
print(Sys.time())

sink()
print("Saving Data OK!!!")
print("End Script!!!")
print(Sys.time())
