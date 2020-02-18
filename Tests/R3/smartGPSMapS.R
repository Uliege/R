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

dataFile <- "SmartGPSData.RData"

outputFile <- paste(urlReports,"outputMaps.txt",sep = "")
rawDataFile <- paste(urlRawData,dataFile,sep = "") 

print("Start..")
print("Script: smartGPSMap.R")
print(Sys.time())

sink(file = outputFile)
print("Start..")
print("Script: smartGPSMap.R")
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
nameChart<-"Map01-GPSPointTrack.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print("Generating Charts....")
print(paste(nameChart, Sys.time()))
ggmap(mapQuito) + 
  geom_point(data = dataClean,
             aes(x = dspLong[], y = dspLat[]), 
             alpha = .5, 
             color="darkred", 
             size = .0001) +
  ggtitle("GPS Point Track") + 
  xlab("Longitude") +
  ylab("Latitude")
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

print("K-means clustering Params: Lattitude - Longitude, n=4")
nameChart<-"Map02-GPSKMeanCluster.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print("K-means clustering Params: Lattitude - Longitude, n=4")
print(paste(nameChart, Sys.time()))
set.seed(20)
clusters <- kmeans(dataClean[,2:3], 4)
print("Adding the cluster number in the dataset as column")
dataClean$dspCluster <- as.factor(clusters$cluster)
ggmap(mapQuito) + 
  geom_point(data = dataClean, 
             aes(x = dspLong[], y = dspLat[], colour = as.factor(dspCluster)),
             size = .001) +
  ggtitle("GPS K-Mean Cluster") + 
  xlab("Longitude") +
  ylab("Latitude")  +
  scale_fill_gradient("Cluster")
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Map03-GPSDensityPoligon.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggmap(mapQuito) + 
  stat_density2d(data = dataClean,
                 aes(x = dspLong[], y = dspLat[],
                     fill = ..level.. , 
                     alpha = ..level..),
                 size = 2, 
                 bins = 20,
                 color="red",
                 geom = "polygon") +
  ggtitle("GPS Point Density") + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradient(guide = FALSE) +
  scale_alpha(range = c(.4, .75), guide = FALSE)
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Map04-GPSDensity2D.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggmap(mapQuito) + 
  stat_density2d(data = dataClean,
                 aes(x = dspLong[], y = dspLat[],
                     alpha = ..level..),
                 size = 2, 
                 bins = 20,
                 color="red",
                 geom = "density_2d") +
  ggtitle("GPS Point Density") + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_alpha(guide = FALSE)
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()

print("End Script: smartGPSMap!!")
print(Sys.time())

sink()
print("End Script: smartGPSMap!!")
print(Sys.time())
