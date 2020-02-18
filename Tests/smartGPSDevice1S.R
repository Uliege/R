library(dplyr)
library(jsonlite)
library(VIM)
library(lubridate)
library(ggmap)
library(sqldf)
library(vcd)

urlData = "/home/giovanny/SmartGPSAnalysis/data/"
urlFigures = "/home/giovanny/SmartGPSAnalysis/figures/"
urlRawData = "/home/giovanny/SmartGPSAnalysis/rawdata/"
urlReports = "/home/giovanny/SmartGPSAnalysis/reports/"

dataFile <- "SmartGPSData.RData"

outputFile = paste(urlReports,"outputDevices.txt",sep = "")
rawDataFile <- paste(urlRawData,dataFile,sep = "") 

print("Start..")
print("Script: smartGPSDevice.R")
print(Sys.time())

sink(file = outputFile)
print("Start..")
print("Script: smartGPSDevice.R")
print(Sys.time())
sink()

print("Load raw data")
print(Sys.time())

sink(file = outputFile, append = TRUE)
print("Load raw data")
print(Sys.time())
load(file=rawDataFile)
print("Load raw data OK!!!")
print(Sys.time())
sink()

print("Load raw data OK!!!")
print(Sys.time())
print("Generating Charts....")
nameChart<-"Device01-AndrVersionDevices.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print("Generating Charts....")
print(paste(nameChart, Sys.time()))
ggplot(dataDevice,
       aes(x=xDAndroV, fill=xDNumPoints))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Number Devices per Android Version") + 
  xlab("") +
  ylab("Number of Devices") 
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Device02-AndrVersionDataCollected.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(dataDevice,
       aes(x=xDAndroV, y=xDNumPoints))+ 
  geom_col(alpha=0.6)+
  ggtitle("GPS Data Collected per Android Version") + 
  xlab("") +
  ylab("Number of GPS Traces")
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()

print("End Script: smartGPSDevice.R!!")
print(Sys.time())

sink()

print("End Script: smartGPSDevice.R!!")
print(Sys.time())