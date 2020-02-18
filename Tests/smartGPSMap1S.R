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
dataMaps <- "SmartGPSMaps.RData"

outputFile <- paste(urlReports,"outputMaps.txt",sep = "")
rawDataFile <- paste(urlRawData,dataFile,sep = "")
rawMapsFile <- paste(urlRawData,dataMaps,sep = "")

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
load(file=rawMapsFile)
print("Load raw data OK!!!")
print(Sys.time())
sink()

print("Load raw data OK!!!")
print(Sys.time())
print("Generating Charts....")
nameChart<-"Map01.1-GPSPointTrack-Terrain.png"
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

nameChart<-"Map01.2-GPSPointTrack-Road.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggmap(roadMapQuito) + 
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

nameChart<-"Map01.3-GPSPointTrack-Satellite.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggmap(satelliteMapQuito) + 
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

nameChart<-"Map02.1-GPSDensityPoligon-Terrain.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggmap(mapQuito) + 
  stat_density2d(data = dataClean,
                 aes(x = dspLong[], y = dspLat[],
                     fill = ..level.. , 
                     alpha = ..level..),
                 size = 2, 
                 bins = 9,
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

nameChart<-"Map02.2-GPSDensityPoligon-Road.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggmap(roadMapQuito) + 
  stat_density2d(data = dataClean,
                 aes(x = dspLong[], y = dspLat[],
                     fill = ..level.. , 
                     alpha = ..level..),
                 size = 2, 
                 bins = 9,
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

nameChart<-"Map02.3-GPSDensityPoligon-Satellite.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggmap(satelliteMapQuito) + 
  stat_density2d(data = dataClean,
                 aes(x = dspLong[], y = dspLat[],
                     fill = ..level.. , 
                     alpha = ..level..),
                 size = 2, 
                 bins = 9,
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

nameChart<-"Map03.1-GPSDensity2D-Terrain.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggmap(mapQuito) + 
  stat_density2d(data = dataClean,
                 aes(x = dspLong[], y = dspLat[],
                     alpha = ..level..),
                 size = 3, 
                 bins = 8,
                 color="red",
                 geom = "density_2d") +
  ggtitle("GPS Point Density") + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_alpha(guide = FALSE)
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Map03.2-GPSDensity2D-Road.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggmap(roadMapQuito) + 
  stat_density2d(data = dataClean,
                 aes(x = dspLong[], y = dspLat[],
                     alpha = ..level..),
                 size = 3, 
                 bins = 8,
                 color="red",
                 geom = "density_2d") +
  ggtitle("GPS Point Density") + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_alpha(guide = FALSE)
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Map03.3-GPSDensity2D-Satellite.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggmap(satelliteMapQuito) + 
  stat_density2d(data = dataClean,
                 aes(x = dspLong[], y = dspLat[],
                     alpha = ..level..),
                 size = 3, 
                 bins = 8,
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
