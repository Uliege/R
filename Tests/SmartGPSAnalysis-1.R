##install.packages("rlang")
# install dev version of ggmap
##devtools::install_github("dkahle/ggmap")

library(dplyr)
library(jsonlite)
library(VIM)
library(lubridate)
library(ggmap)

print("Inicio..")

readJsonFile <- function(fileName) {
  dataResult <- NULL
  
  archivo <- paste("D:/tmp/",fileName,sep = "")
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
data1 <- readJsonFile("data9.json")
print("Loading 1... OK")

dataSource <- data1

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
print(summary(aggr(dataSource, only.miss = TRUE, sortVars = TRUE)))

png(filename = "/home/giovanny/SmartGPSAnalysis/figures/01-MissingData.png", height = 800, width = 800)
aggr(dataSource, col = c("yellow", "green", "gray"), only.miss = TRUE, sortVars = TRUE)
graphics.off()


#Missing Data
summary(aggr(dataFormat, col = c("yellow", "green", "gray"), only.miss = TRUE))
dataFormat<-NULL

#Eliminar filas con valores Nulos
dataClean <- na.omit(dataSource)

#New Missing Data with DataClean
summary(aggr(dataClean, col = c("yellow", "green", "gray"), only.miss = TRUE))

#Aditional Columns
dataClean$dspVeloKmH <- dataClean$dspVelo*3.6
dataClean$dspHour <- factor(hour(dataClean$dspFcIn))
dataClean$dspWeekDay <- factor(wday(dataClean$dspFcUp))

#Coordinates range of Quito
latMin = -0.400294
latMax =  0.026711
lonLef = -78.591624
lonRig = -78.270936
dataClean <- subset(dataClean, between(dspLat,latMin,latMax))
dataClean <- subset(dataClean, between(dspLong,lonLef,lonRig))

summary(dataClean)
str(dataClean)

print("Chart 02-ActivityDistributionBoxes.png")
ggplot(dataClean) + 
  geom_boxplot(aes(x=factor(dspHour),y=dspActy), 
               show.legend = TRUE)+
  ggtitle("Activity Recognition Distribution Per Hour in Quito") + 
  xlab("Hour") +
  ylab("Activity") +
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/02-ActivityDistributionBoxes.png")
graphics.off()

dataNum <- data.frame(dataClean$dspLat, dataClean$dspLong, dataClean$dspNSat, dataClean$dspAlti, dataClean$dspVelo)
print("Matriz de Covarianza")
print(cov(dataNum))
print("Matriz de Correlación")
print(cor(dataNum))

png(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/03-SatelliteNumberDensity.png", height = 800, width = 800)
plot(density(dataClean$dspNSat), type="h", col="blue", main="Satellite Number - Density")
graphics.off()


png(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/04-ActivityRecognitionDensity.png", height = 800, width = 800)
plot(density(dataClean$dspActy), type="h", col="green", main='Activity Recognition - Density')
graphics.off()


png(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/05-AccuracyDensity.png", height = 800, width = 800)
plot(density(dataClean$dspAccu), main='Accuracy - Density')
graphics.off()

png(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/06-VelocityDistributionActivityBoxplot.png", height = 800, width = 800)
boxplot(dspVeloKmH ~ dspActy, data=subset(dataClean, dspVeloKmH<100), main='Velocity Distribution per Activity - Boxplot', col="brown", ylab="Velocity (Km/h)", xlab="Activity (0-Vehicle 1-Bicycle 2-OnFoot 3-Still 4-Unknown 5-Tillting 7-Walking 8-Running)")
graphics.off()


#******************************************


ggplot(subset(dataClean, (dspActy!=3 & dspActy!=4))) +   
  geom_jitter(aes(factor(dspHour),dspVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/07-VelocityDistributionHour.png")
graphics.off()


ggplot(subset(dataClean, as.character.Date(dataClean$dspFcIn)==as.character.Date(dataClean$dspFcUp) & 
                dspActy!=3 & dspActy!=4)) +
  geom_jitter(aes(factor(dspHour),dspVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/07.1-VelocityDistributionHour-OM.png")
graphics.off()

ggplot(subset(dataClean, dspActy!=3 & dspActy!=4)) +   
  geom_jitter(aes(factor(dspWeekDay),dspVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Day") + 
  xlab("Week Day (1-Monday)") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/08-VelocityDistributionDay.png")
graphics.off()


ggplot(subset(dataClean, as.character.Date(dataClean$dspFcIn)==as.character.Date(dataClean$dspFcUp) & 
                dspActy!=3 & dspActy!=4)) +   
  geom_jitter(aes(factor(dspWeekDay),dspVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Day") + 
  xlab("Week Day (1-Monday)") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/08.1-VelocityDistributionDay-OM.png")
graphics.off()


ggplot(subset(dataClean, dspActy!=3 & dspActy!=4)) +
  geom_point(aes(x=dspHour, y=dspVeloKmH, col=factor(dspActy)), size=1) +
  ggtitle("Activity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/09-ActiviityDistributionHour.png")
graphics.off()


ggplot(subset(dataClean, as.character.Date(dataClean$dspFcIn)==as.character.Date(dataClean$dspFcUp) & 
                dspActy!=3 & dspActy!=4)) +
  geom_point(aes(x=dspHour, y=dspVeloKmH, col=factor(dspActy)), size=1) +
  ggtitle("Activity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/09.1-ActiviityDistributionHour-OM.png")
graphics.off()


ggplot(subset(dataClean, dspActy!=3 & dspActy!=4)) +
  geom_point(aes(x=dspWeekDay, y=dspVeloKmH, col=factor(dspActy)), size=1) +
  ggtitle("Activity Distribution Per Day") + 
  xlab("Week Day (1-Monday)") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/10-ActiviityDistributionDay.png")
graphics.off()


ggplot(subset(dataClean, as.character.Date(dataClean$dspFcIn)==as.character.Date(dataClean$dspFcUp) & 
                dspActy!=3 & dspActy!=4)) +
  geom_point(aes(x=dspWeekDay, y=dspVeloKmH, col=factor(dspActy)), size=1) +
  ggtitle("Activity Distribution Per Day") + 
  xlab("Week Day (1-Monday)") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/10.1-ActiviityDistributionDay-OM.png")
graphics.off()














#Map of Quito from Google
register_google(key = "AIzaSyDM3eTYsoIVaAdZJPMyEy9xO-pqXKryVjE")
QuitoMapCenter11 <- get_map(location = c(lat = -0.183446, long= -78.418674), zoom = 11)

mapQuito <- QuitoMapCenter11

#GPS Points Map
ggmap(mapQuito) + 
  geom_point(data = dataClean,
             aes(x = dspLong[], y = dspLat[]), 
             alpha = .5, 
             color="darkred", 
             size = .0001) +
  ggtitle("GPS Point Track on the Map") + 
  xlab("Longitude") +
  ylab("Latitude") +
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/11-GPSPointTrack.png")
graphics.off()


#Density Map
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
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/12-GPSDensityPolygon.png")
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
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/12.1-GPSDensity2D.png")
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
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/12.2-GPSDensity2D.png")
graphics.off()


#K-means clustering with 5 clusters Params: Lattitude - Longitude, n=5
set.seed(20)
clusters <- kmeans(dataClean[,2:3], 4)

#Save the cluster number in the dataset as column 'Cluster'
dataClean$cluster <- as.factor(clusters$cluster)

#Draw clusters on the Map
ggmap(mapQuito) + 
  geom_point(data = dataClean, 
             aes(x = dspLong[], y = dspLat[], colour = as.factor(cluster)),
             size = .001) +
  ggtitle("K-Mean Cluster of GPS Point") + 
  xlab("Longitude") +
  ylab("Latitude") +
  ggsave(filename = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/13-KMeanClusterGPS.png")
graphics.off()














  

  
  p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(alpha = year))

p
p + scale_alpha("cylinders")
p + scale_alpha(range = c(0.4, 0.8))  













dev.off()











#Velocity by hour, the color indicates Activity  --- NO
g = ggplot(subset(dataClean, dspVeloKmH<100)) +
  geom_line(aes(x=dspHour,y=dspVeloKmH,colour=dspActy))
print(g)


#Acumulación de actividades por hora  ---- NO
g = ggplot(dataClean) + 
  geom_jitter(aes(factor(dspHour),dspActy),
              colour='darkgreen')
print(g)

#Acumulación de velocidades por actividad  --- NO
g = ggplot(subset(dataClean, dspVeloKmH<100)) + 
  geom_jitter(aes(factor(dspActy),dspVeloKmH,col=dspVeloKmH)) + 
  scale_colour_gradient(low='red',high='green')
print(g)


#Relación entre la vel y act por hora
g = ggplot(subset(dataClean, dspVeloKmH<100)) + 
  geom_point(aes(x=dspHour,y=dspVeloKmH,col=factor(dspActy)))
print(g)


g = ggplot(subset(dataClean, dspVeloKmH<100)) + 
  geom_point(aes(x=dspWeekDay,y=dspVeloKmH,col=factor(dspActy)))
print(g)




dim(subset(dataClean, dspFeIn==dspFecUp))

dataClean$dspWeekDay





rm(act)
