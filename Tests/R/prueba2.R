library(dplyr)
library(jsonlite)
library(VIM)
library(lubridate)
library(ggmap)
library(sqldf)

print("Inicio!!")

print("Carga Datos")
load(file="/home/giovanny/SmartGPSAnalysis/rawdata/SGPS1.RData")
print("OK Carga")




ggplot(subset(dataActivity,qVeloKmH<125)) +   
  geom_jitter(aes(factor(qWeekDay),qVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Day") + 
  xlab("Week Day") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/3-VelocityDay.png")
graphics.off()

ggplot(subset(dataActivity,qVeloKmH<125)) +   
  geom_jitter(aes(factor(qHour),qVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Day") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") 

  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/4-VelocityHour.png")
graphics.off()







ggplot(subset(dataMovement,qVeloKmH<125)) +   
  geom_jitter(aes(factor(qWeekDay),qVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Day") + 
  xlab("Week Day") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/3.1-VelocityDay.png")
graphics.off()

ggplot(subset(dataMovement,qVeloKmH<125)) +   
  geom_jitter(aes(factor(qHour),qVeloKmH),
              colour="blue", size=.0001) +
  ggtitle("Velocity Distribution Per Day") + 
  xlab("Hour") +
  ylab("Velocity (km/h)") +
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/4.1-VelocityHour.png")
graphics.off()
print("Script Finalizado")

str(dataClean)
str(dataActivity)
str(dataMovement)
str(mapQuito)


print("Datos Guardados")

#rm(dataNum)










dataDevice <- read.csv("D:/tmp/DataDevice.csv")


dataQuest <- read.csv("D:/tmp/DataQuestionnaire.csv")


data


rm(dataDevice)

rm(dataQuest)
