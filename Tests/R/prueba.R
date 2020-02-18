#install.packages("rlang")
# install dev version of ggmap
#devtools::install_github("dkahle/ggmap")

library(dplyr)
library(jsonlite)
library(VIM)
library(lubridate)
library(ggmap)

#install.packages("VIM", repos="https://cran.rstudio.com")
sink()

print("Inicio..")

readJsonFile <- function(fileName) {
  dataResult <- NULL
  
  archivo <- paste("/home/giovanny/SmartGPSAnalysis/data/",fileName,sep = "")
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


print("Loading 1...")
data1 <- readJsonFile("data1.json")
print("Loading 1... OK")

print("Loading 2...")
data2 <- readJsonFile("data2.json")
print("Loading 2... OK")

print("Loading 3...")
data3 <- readJsonFile("data3.json")
print("Loading 3... OK")

print("Loading 4...")
data4 <- readJsonFile("data4.json")
print("Loading 4... OK")

print("Loading 5...")
#data5 <- readJsonFile("data5.json")
#print("Loading 5... OK")

print("Loading 6...")
#data6 <- readJsonFile("data6.json")
#print("Loading 6... OK")

print("Loading 7...")
#data7 <- readJsonFile("data7.json")
#print("Loading 7... OK")

print("Loading 8...")
#data8 <- readJsonFile("data8.json")
#print("Loading 8... OK")

print("Loading 9...")
#data9 <- readJsonFile("data9.json")
#print("Loading 9... OK")

print("Loading 10...")
#data10 <- readJsonFile("data10.json")
#print("Loading 10... OK")

print("Loading 11...")
#data11 <- readJsonFile("data11.json")
#print("Loading 11... OK")

print("Loading 12...")
data12 <- readJsonFile("data12.json")
print("Loading 12... OK")

print("Loading 13...")
#data13 <- readJsonFile("data13.json")
#print("Loading 13... OK")

print("Loading 14...")
#data14 <- readJsonFile("data14.json")
#print("Loading 14... OK")

print("Loading 15...")
#data15 <- readJsonFile("data15.json")
#print("Loading 15... OK")

print("Loading 16...")
data16 <- readJsonFile("data16.json")
print("Loading 16... OK")

#dataSource <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12, data13, data14, data15, data16)
dataSource <- bind_rows(data1, data2, data3, data4, data12, data16)

sink(file = "/home/giovanny/SmartGPSAnalysis/reports/outputR.txt")

print("Type of DataSource")
print(typeof(dataSource))

print("Number of rows")
print(nrow(dataSource))

print("Number of columns")
print(ncol(dataSource))

print("Structure of DataSource")
print(str(dataSource))

print("Summary of DataSource")
print(summary(dataSource))

print("Summary of Missing Data")
print(summary(aggr(dataSource, only.miss = TRUE, sortVars = TRUE, plot = FALSE)))

print("Chart 01-MissingData.png")
png(filename = "/home/giovanny/SmartGPSAnalysis/figures/01-MissingData.png", height = 800, width = 800)
aggr(dataSource, col = c("yellow", "green", "gray"), only.miss = TRUE, sortVars = TRUE, numbers = TRUE)
graphics.off()

print("Cleaning DataSource - Delete rows with null data")
dataClean <- na.omit(dataSource)

print("New Missing Data with DataClean")
print(summary(aggr(dataClean, only.miss = TRUE, sortVars = TRUE, plot = FALSE)))

print("Aditional Columns")
dataClean$dspVeloKmH <- dataClean$dspVelo*3.6
dataClean$dspHour <- factor(hour(dataClean$dspFcIn))
dataClean$dspWeekDay <- factor(wday(dataClean$dspFcUp))

print("Coordinates range of Quito")
latMin = -0.400294
latMax =  0.026711
lonLef = -78.591624
lonRig = -78.270936
dataClean <- subset(dataClean, between(dspLat,latMin,latMax))
dataClean <- subset(dataClean, between(dspLong,lonLef,lonRig))

print("Structure of DataClean")
print(str(dataClean))

print("Summary of DataClean")
print(summary(dataClean))

print("Chart 02-ActivityDistributionBoxes.png")
ggplot(dataClean) + 
  geom_boxplot(aes(x=factor(dspHour),y=dspActy), 
               show.legend = TRUE)+
  ggtitle("Activity Recognition Distribution Per Hour in Quito") + 
  xlab("Hour") +
  ylab("Activity") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/02-ActivityDistributionBoxes.png")
graphics.off()

dataNum <- data.frame(dataClean$dspLat, dataClean$dspLong, dataClean$dspNSat, dataClean$dspAlti, dataClean$dspVelo)

print("Covariance Matrix ")
print(cov(dataNum))

print("Correlation Matrix")
print(cor(dataNum))

print("Chart 03-SatelliteNumberDensity.png")
png(filename = "/home/giovanny/SmartGPSAnalysis/figures/03-SatelliteNumberDensity.png", height = 800, width = 800)
plot(density(dataClean$dspNSat), type="h", col="blue", main="Satellite Number - Density")
graphics.off()

print("Chart 04-ActivityRecognitionDensity.png")
png(filename = "/home/giovanny/SmartGPSAnalysis/figures/04-ActivityRecognitionDensity.png", height = 800, width = 800)
plot(density(dataClean$dspActy), type="h", col="green", main='Activity Recognition - Density')
graphics.off()

print("Chart 05-AccuracyDensity.png")
png(filename = "/home/giovanny/SmartGPSAnalysis/figures/05-AccuracyDensity.png", height = 800, width = 800)
plot(density(dataClean$dspAccu), main='Accuracy - Density')
graphics.off()

print("Chart 06-VelocityDistributionActivityBoxplot.png")
png(filename = "/home/giovanny/SmartGPSAnalysis/figures/06-VelocityDistributionActivityBoxplot.png", height = 800, width = 800)
boxplot(dspVeloKmH ~ dspActy, data=dataClean, main='Velocity Distribution per Activity - Boxplot', col="brown", ylab="Velocity (Km/h)", xlab="Activity (0-Vehicle 1-Bicycle 2-OnFoot 3-Still 4-Unknown 5-Tillting 7-Walking 8-Running)")
graphics.off()

print("Chart 06.1-VelocityDistributionActivityBoxplot.png")
png(filename = "/home/giovanny/SmartGPSAnalysis/figures/06.1-VelocityDistributionActivityBoxplot.png", height = 800, width = 800)
boxplot(dspVeloKmH ~ dspActy, data=subset(dataClean, dspVeloKmH<100), main='Velocity Distribution per Activity - Boxplot', col="brown", ylab="Velocity (Km/h)", xlab="Activity (0-Vehicle 1-Bicycle 2-OnFoot 3-Still 4-Unknown 5-Tillting 7-Walking 8-Running)")
graphics.off()

print("Chart 07-VelocityDistributionHour.png")
ggplot(subset(dataClean, dspActy!=3 & dspActy!=4 & dspVeloKmH<125)) +   
  geom_jitter(aes(factor(dspHour),dspVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/07-VelocityDistributionHour.png")
graphics.off()

ggplot(subset(dataClean, as.character.Date(dataClean$dspFcIn)==as.character.Date(dataClean$dspFcUp) & 
                dspActy!=3 & dspActy!=4 & dspVeloKmH<125)) +
  geom_jitter(aes(factor(dspHour),dspVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/07.1-VelocityDistributionHour-OM.png")
graphics.off()


print("Chart 08-VelocityDistributionDay.png")
ggplot(subset(dataClean, dspActy!=3 & dspActy!=4 & dspVeloKmH<125)) +   
  geom_jitter(aes(factor(dspWeekDay),dspVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Day") + 
  xlab("Week Day (1-Monday)") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/08-VelocityDistributionDay.png")
graphics.off()


ggplot(subset(dataClean, as.character.Date(dataClean$dspFcIn)==as.character.Date(dataClean$dspFcUp) & 
                dspActy!=3 & dspActy!=4 & dspVeloKmH<125)) +   
  geom_jitter(aes(factor(dspWeekDay),dspVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Day") + 
  xlab("Week Day (1-Monday)") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/08.1-VelocityDistributionDay-OM.png")
graphics.off()


print("Chart 09-ActivityDistributionHour.png")
ggplot(subset(dataClean, dspActy!=3 & dspActy!=4 & dspVeloKmH<125)) +
  geom_point(aes(x=dspHour, y=dspVeloKmH, col=factor(dspActy)), size=1) +
  ggtitle("Activity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/09-ActivityDistributionHour.png")
graphics.off()


ggplot(subset(dataClean, as.character.Date(dataClean$dspFcIn)==as.character.Date(dataClean$dspFcUp) & 
                dspActy!=3 & dspActy!=4 & dspVeloKmH<125)) +
  geom_point(aes(x=dspHour, y=dspVeloKmH, col=factor(dspActy)), size=1) +
  ggtitle("Activity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/09.1-ActivityDistributionHour-OM.png")
graphics.off()


print("Chart 10-ActivityDistributionDay.png")
ggplot(subset(dataClean, dspActy!=3 & dspActy!=4 & dspVeloKmH<125)) +
  geom_point(aes(x=dspWeekDay, y=dspVeloKmH, col=factor(dspActy)), size=1) +
  ggtitle("Activity Distribution Per Day") + 
  xlab("Week Day (1-Monday)") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/10-ActivityDistributionDay.png")
graphics.off()


ggplot(subset(dataClean, as.character.Date(dataClean$dspFcIn)==as.character.Date(dataClean$dspFcUp) & 
                dspActy!=3 & dspActy!=4 & dspVeloKmH<125)) +
  geom_point(aes(x=dspWeekDay, y=dspVeloKmH, col=factor(dspActy)), size=1) +
  ggtitle("Activity Distribution Per Day") + 
  xlab("Week Day (1-Monday)") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/10.1-ActivityDistributionDay-OM.png")
graphics.off()


print("Register API Google Maps")
register_google(key = "AIzaSyDM3eTYsoIVaAdZJPMyEy9xO-pqXKryVjE")

print("Query Map")
QuitoMapCenter11 <- get_map(location = c(lat = -0.183446, long= -78.418674), zoom = 11)
mapQuito <- QuitoMapCenter11

print("Chart 11-GPSPointTrack.png")
ggmap(mapQuito) + 
  geom_point(data = dataClean,
             aes(x = dspLong[], y = dspLat[]), 
             alpha = .5, 
             color="darkred", 
             size = .0001) +
  ggtitle("GPS Point Track on the Map") + 
  xlab("Longitude") +
  ylab("Latitude") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/11-GPSPointTrack.png")
graphics.off()


print("Chart 12-GPSDensity.png")
ggmap(mapQuito) + 
  stat_density2d(data = dataClean,
                 aes(x = dspLong[], y = dspLat[],
                     fill = ..level.. , 
                     alpha = ..level..),
                 size = 2, 
                 bins = 4,
                 color="red",
                 geom = "polygon") +
  ggtitle("GPS Point Density on the Map") + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradient("Density") +
  scale_alpha(range = c(.4, .75), guide = FALSE) +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/12-GPSDensityPolygon.png")
graphics.off()


ggmap(mapQuito) + 
  stat_density2d(data = dataClean,
                 aes(x = dspLong[], y = dspLat[],
                     alpha = ..level..),
                 size = 2, 
                 bins = 4,
                 color="red",
                 geom = "density_2d") +
  ggtitle("GPS Point Density on the Map") + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_alpha(guide = FALSE) +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/12.1-GPSDensity2D.png")
graphics.off()


ggmap(mapQuito) + 
  stat_density2d(data = dataClean,
                 aes(x = dspLong[], y = dspLat[],
                     alpha = ..level..),
                 size = .0001, 
                 bins = 5,
                 color="red",
                 geom = "density_2d") +
  ggtitle("GPS Point Density on the Map") + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_alpha(guide = FALSE) +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/12.2-GPSDensity2D.png")
graphics.off()

print("K-means clustering with 5 clusters Params: Lattitude - Longitude, n=4")
set.seed(20)
clusters <- kmeans(dataClean[,2:3], 4)

print("Adding the cluster number in the dataset as column")
dataClean$dspCluster <- as.factor(clusters$cluster)

print("Chart 13-KMeanClusterGPS.png")
ggmap(mapQuito) + 
  geom_point(data = dataClean, 
             aes(x = dspLong[], y = dspLat[], colour = as.factor(dspCluster)),
             size = .001) +
  ggtitle("K-Mean Cluster of GPS Point") + 
  xlab("Longitude") +
  ylab("Latitude") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/13-KMeanClusterGPS.png")
graphics.off()

sink()
print("Script Finalizado")

save(dataClean, mapQuito, file="/home/giovanny/SmartGPSAnalysis/rawdata/SGPS.RData")
