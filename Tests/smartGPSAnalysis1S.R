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

outputFile <- paste(urlReports,"outputAnalysis.txt",sep = "")
rawDataFile <- paste(urlRawData,dataFile,sep = "") 

print("Start..")
print("Script: smartGPSAnalysis.R")
print(Sys.time())

sink(file = outputFile)
print("Start..")
print("Script: smartGPSAnalysis.R")
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

dataNum <- sqldf("SELECT 
                  dspLat as lat,
                  dspLong as long,
                  dspNSat as numSat,
                  dspAlti as altitude,
                  dspVelo as velocity,
                  dspAccu as Accuracy,
                  CAST(dspHour AS SIGNED) as hour
                  FROM dataClean")

fctActivityDay = ftable(dataActivity$qActyDesc, as.factor(dataActivity$qWeekDay),dnn = c("Activity", "Day"))
fctActivity0123Day = ftable(dataActivity0123$qActyDesc, as.factor(dataActivity0123$qWeekDay),dnn = c("Activity", "Day"))
fctActivityHour = ftable(dataActivity$qActyDesc, as.factor(dataActivity$qHour), dnn = c("Actividad", "Hour"))
fctActivity0123Hour = ftable(dataActivity0123$qActyDesc, as.factor(dataActivity0123$qHour),dnn = c("Activity", "Hour"))

print("Data Analysis....")
print(Sys.time())
print("Covariance Matrix ")
print(cov(dataNum))
print("Correlation Matrix - pearson Coefficient")
print(cor(dataNum))
print("Cramer's V - Activity-Day")
print(assocstats(fctActivityDay))
print("Cramer's V - Activity-Hour")
print(assocstats(fctActivityHour))

sink(file = outputFile, append = TRUE)
print("Data Analysis....")
print(Sys.time())
print("******************************************")
print("Covariance Matrix ")
print("******************************************")
print(cov(dataNum))
print("******************************************")
print("Correlation Matrix - pearson Coefficient")
print("******************************************")
print(cor(dataNum))
print("******************************************")
print("Cramer's V - Activity-Day")
print("******************************************")
print(assocstats(fctActivityDay))
print("******************************************")
print("Cramer's V - Activity-Hour")
print("******************************************")
print(assocstats(fctActivityHour))
sink()

print("Generating Charts....")
nameChart<-"Chart01.1-Density-Altitude.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print("Generating Charts....")
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), 
    height = 800, 
    width = 800)
plot(density(dataClean$dspAlti), 
     type="l", 
     col="blue", 
     main="Altitude Density",
     sub="Altitude")
graphics.off()
sink()

nameChart<-"Chart01.2-Density-Activity.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), 
    height = 800, 
    width = 800)
plot(density(dataActivity0123$qActy), 
     type="l", 
     col="blue", 
     main="Activity Density", 
     asp=.4,
     sub="Activity:   0-Vehicle   1-Bicycle   2-Foot   3-Still")
graphics.off()
sink()

nameChart<-"Chart01.3-Density-SatelliteNumber.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), 
    height = 800, 
    width = 800)
plot(density(dataClean$dspNSat), 
     type="l", 
     col="blue", 
     main="Satellite Number Density",
     sub="Number of Satellites")
graphics.off()
sink()

nameChart<-"Chart01.4-Density-Accuracy.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), 
    height = 800, 
    width = 800)
plot(density(dataClean$dspAccu), 
     type="l", 
     col="blue",  
     main='Accuracy Density',
     sub="Accuracy")
graphics.off()
sink()

nameChart<-"Chart01.5-Density-Velocity.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), 
    height = 800, 
    width = 800)
plot(density(dataClean$dspVeloKmH), 
     type="l", 
     col="blue",  
     main='Velocity Density',
     sub="Velocity (km/h)")
graphics.off()
sink()

nameChart<-"Chart02.1-Bars-ActivityDistributionHour.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(dataActivity0123,
       aes(x=qHour, fill = qActyDesc))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Activity Distribution Per Hour") + 
  xlab("Hour of the Day") +
  ylab("Data Collected") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()


nameChart<-"Chart02.2-Bars-ActivityDistributionDay.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(dataActivity0123,
       aes(x=qWeekDayDesc, fill=qActyDesc))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Activity Distribution Per Day") + 
  xlab("Day of the Week (1-Sunday ... 7-Saturday)") +
  ylab("Data Collected") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart03.1-Jitter-VelocityDistributionHour.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataClean, dspVeloKmH<125)) +   
  geom_jitter(aes(factor(dspHour),dspVeloKmH),
              colour="blue", 
              size=.0001) +
  ggtitle("Velocity Distribution Per Hour") + 
  xlab("Hour of the Day") +
  ylab("Velocity (km/h)") 
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart03.2-Jitter-VelocityDistributionDay.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActivity, qVeloKmH<125)) +   
  geom_jitter(aes(factor(qWeekDayDesc),qVeloKmH),
              colour="blue", 
              size=.0001) +
  ggtitle("Velocity Distribution Per Day") + 
  xlab("Day of the Week (1-Sunday ... 7-Saturday)") +
  ylab("Velocity (km/h)") 
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()

print("End Script: smartGPSAnalysis!!")
print(Sys.time())

sink()

print("End Script: smartGPSAnalysis!!")
print(Sys.time())
