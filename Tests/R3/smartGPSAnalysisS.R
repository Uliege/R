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

print("Data Analysis....")
print(Sys.time())
print("Covariance Matrix ")
print(cov(dataNum))
print("Correlation Matrix")
print(cor(dataNum))

sink(file = outputFile, append = TRUE)
print("Data Analysis....")
print(Sys.time())
print("******************************************")
print("Covariance Matrix ")
print("******************************************")
print(cov(dataNum))
print("******************************************")
print("Correlation Matrix")
print("******************************************")
print(cor(dataNum))
sink()

print("Generating Charts....")
nameChart<-"Chart01-SatelliteNumber-Density.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print("Generating Charts....")
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), 
    height = 800, 
    width = 800)
plot(density(dataClean$dspNSat), type="l", col="blue", main="Satellite Number Density")
graphics.off()
print(paste(nameChart, Sys.time()))
sink()

nameChart<-"Chart02-Activity-Density.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), 
    height = 800, 
    width = 800)
plot(density(dataActivity$qActy), type="l", col="blue", main='Activity Density')
graphics.off()
print(paste(nameChart, Sys.time()))
sink()

nameChart<-"Chart03-Accuracy-Density.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), 
    height = 800, 
    width = 800)
plot(density(dataClean$dspAccu), type="l", col="blue",  main='Accuracy Density')
graphics.off()
print(paste(nameChart, Sys.time()))
sink()

nameChart<-"Chart04-ActivityDistributionHour-Boxes.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(dataActivity) + 
  geom_boxplot(aes(x=factor(qHour),y=qActy))+
  ggtitle("Activity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Activity")
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
print(paste(nameChart, Sys.time()))
sink()

nameChart<-"Chart05-ActivityDistributionDay-Boxes.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(dataActivity) + 
  geom_boxplot(aes(x=factor(qWeekDay),y=qActy))+
  ggtitle("Activity Distribution Per Day") + 
  xlab("Week Day (1-Monday) ... (7-Sunday)") +
  ylab("Activity")
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
print(paste(nameChart, Sys.time()))
sink()

nameChart<-"Chart06-ActivityDistributionHour-Bars.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(dataActivity,
       aes(x=qHour, fill = qActyDesc))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Activity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Count") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart06.1-ActivityDistributionHour-Bars-Movement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyInMotion, qActy!=3 & qActy!=4),
       aes(x=qHour, fill = qActyDesc))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Activity Distribution Per Hour - Movement") + 
  xlab("Hour") +
  ylab("Count") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart06.2-ActivityDistributionHour-Bars-NoMovement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyNoMotion,  qActy==3),
       aes(x=qHour, fill = qActyDesc))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Activity Distribution Per Hour - No Movement") + 
  xlab("Hour") +
  ylab("Count") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart07-ActivityDistributionDay-Bars.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(dataActivity,
       aes(x=qWeekDayDesc, fill=qActyDesc))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Activity Distribution Per Day") + 
  xlab("Week Day (1-Monday) ... (7-Sunday)") +
  ylab("Count") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart07.1-ActivityDistributionDay-Bars-Movement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyInMotion, qActy!=3 & qActy!=4),
       aes(x=qWeekDayDesc, fill=qActyDesc))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Activity Distribution Per Day - Movement") + 
  xlab("Week Day (1-Monday) ... (7-Sunday)") +
  ylab("Count") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart07.2-ActivityDistributionDay-Bars-NoMovement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyNoMotion,  qActy==3),
       aes(x=qWeekDayDesc, fill=qActyDesc))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Activity Distribution Per Day - No Movement") + 
  xlab("Week Day (1-Monday) ... (7-Sunday)") +
  ylab("Count") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart08-ActivityVelocityDistributionHour-Points.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActivity, qVeloKmH<125))+ 
  geom_point(aes(x=qHour, y=qVeloKmH, col=qActyDesc), 
             size=1) +
  ggtitle("Activity and Velocity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart08.1-ActivityVelocityDistributionHour-Points-Movement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyInMotion, qActy!=3 & qActy!=4 & qVeloKmH<125))+ 
  geom_point(aes(x=qHour, y=qVeloKmH, col=qActyDesc), 
             size=1) +
  ggtitle("Activity and Velocity Distribution Per Hour - Movement") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart08.2-ActivityVelocityDistributionHour-Points-NoMovement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyNoMotion, qActy==3 & qVeloKmH<125))+ 
  geom_point(aes(x=qHour, y=qVeloKmH, col=qActyDesc), 
             size=1) +
  ggtitle("Activity and Velocity Distribution Per Hour - No Movement") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart09-ActivityVelocityDistributionDay-Points.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActivity, qVeloKmH<125))+ 
  geom_point(aes(x=qWeekDayDesc, y=qVeloKmH, col=qActyDesc), 
             size=1) +
  ggtitle("Activity and Velocity Distribution Per Day") + 
  xlab("Week Day (1-Monday) ... (7-Sunday)") +
  ylab("Velocity (km/h)") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart09.1-ActivityVelocityDistributionDay-Points-Movement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyInMotion, qActy!=3 & qActy!=4 & qVeloKmH<125))+ 
  geom_point(aes(x=qWeekDayDesc, y=qVeloKmH, col=qActyDesc), 
             size=1) +
  ggtitle("Activity and Velocity Distribution Per Day - Movement") + 
  xlab("Week Day (1-Monday) ... (7-Sunday)") +
  ylab("Velocity (km/h)") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart09.2-ActivityVelocityDistributionDay-Points-NoMovement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyNoMotion, qActy==3 & qVeloKmH<125))+ 
  geom_point(aes(x=qWeekDayDesc, y=qVeloKmH, col=qActyDesc), 
             size=1) +
  ggtitle("Activity and Velocity Distribution Per day - No Movement") + 
  xlab("Week Day (1-Monday) ... (7-Sunday)") +
  ylab("Velocity (km/h)") +
  guides(fill=guide_legend(title="Activity"))
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart10-VelocityDistributionActivity-Boxplot.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), height = 800, width = 800)
boxplot(data=subset(dataActivity, qVeloKmH<125), 
        qVeloKmH ~ qActyDesc, 
        main='Velocity Distribution per Activity', 
        ylab="Velocity (Km/h)", 
        xlab="Activity")
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart10.1-VelocityDistributionActivity-Boxplot-Movement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), height = 800, width = 800)
boxplot(data=subset(dataActyInMotion, qActy!=3 & qActy!=4 & qVeloKmH<125), 
        qVeloKmH ~ qActyDesc, 
        main='Velocity Distribution per Activity - Movement', 
        ylab="Velocity (Km/h)", 
        xlab="Activity")
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart10.2-VelocityDistributionActivity-Boxplot-NoMovement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), height = 800, width = 800)
boxplot(data=subset(dataActyNoMotion, qActy==3 & qVeloKmH<125), 
        qVeloKmH ~ qActyDesc, 
        main='Velocity Distribution per Activity - No Movement', 
        ylab="Velocity (Km/h)", 
        xlab="Activity")
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart11-VelocityDistributionHour.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActivity, qVeloKmH<125)) +   
  geom_jitter(aes(factor(qHour),qVeloKmH),
              colour="blue", 
              size=.0001) +
  ggtitle("Velocity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") 
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart11.1-VelocityDistributionHour-Movement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyInMotion, qVeloKmH<125)) +   
  geom_jitter(aes(factor(qHour),qVeloKmH),
              colour="blue", 
              size=.0001) +
  ggtitle("Velocity Distribution Per Hour - Movement") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") 
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart11.2-VelocityDistributionHour-NoMovement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyNoMotion, qVeloKmH<125)) +   
  geom_jitter(aes(factor(qHour),qVeloKmH),
              colour="blue", 
              size=.0001) +
  ggtitle("Velocity Distribution Per Hour - No Movement") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") 
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()


nameChart<-"Chart12-VelocityDistributionDay.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActivity, qVeloKmH<125)) +   
  geom_jitter(aes(factor(qWeekDayDesc),qVeloKmH),
              colour="blue", 
              size=.0001) +
  ggtitle("Velocity Distribution Per Day") + 
  xlab("Week Day (1-Monday) ... (7-Sunday)") +
  ylab("Velocity (km/h)") 
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart12.1-VelocityDistributionDay-Movement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyInMotion, qVeloKmH<125)) +   
  geom_jitter(aes(factor(qWeekDayDesc),qVeloKmH),
              colour="blue", 
              size=.0001) +
  ggtitle("Velocity Distribution Per Day - Movement") + 
  xlab("Week Day (1-Monday) ... (7-Sunday)") +
  ylab("Velocity (km/h)") 
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()
sink()

nameChart<-"Chart12.2-VelocityDistributionDay-NoMovement.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
ggplot(subset(dataActyNoMotion, qVeloKmH<125)) +   
  geom_jitter(aes(factor(qWeekDayDesc),qVeloKmH),
              colour="blue", 
              size=.0001) +
  ggtitle("Velocity Distribution Per Day - No Movement") + 
  xlab("Week Day (1-Monday) ... (7-Sunday)") +
  ylab("Velocity (km/h)") 
ggsave(filename = paste(urlFigures,nameChart,sep = ""))
graphics.off()

print("End Script: smartGPSAnalysis!!")
print(Sys.time())

sink()
print("End Script: smartGPSAnalysis!!")
print(Sys.time())
