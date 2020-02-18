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
print("Script: loadData.R")
print(Sys.time())

outputFile = paste(urlReports,"outputLoad.txt",sep = "")

sink(file = outputFile)
print("Start..")
print("Script: loadData.R")
print(Sys.time())
sink()

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
print(Sys.time())
sink(file = outputFile, append = TRUE)
print("Reading JSON files...")
print(Sys.time())

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
data5 <- readJsonFile("data5.json")
print("Loading 5... OK")

print("Loading 6...")
data6 <- readJsonFile("data6.json")
print("Loading 6... OK")

print("Loading 7...")
data7 <- readJsonFile("data7.json")
print("Loading 7... OK")

print("Loading 8...")
data8 <- readJsonFile("data8.json")
print("Loading 8... OK")

print("Loading 9...")
data9 <- readJsonFile("data9.json")
print("Loading 9... OK")

print("Loading 10...")
data10 <- readJsonFile("data10.json")
print("Loading 10... OK")

print("Loading 11...")
data11 <- readJsonFile("data11.json")
print("Loading 11... OK")

print("Loading 12...")
data12 <- readJsonFile("data12.json")
print("Loading 12... OK")

print("Loading 13...")
data13 <- readJsonFile("data13.json")
print("Loading 13... OK")

print("Loading 14...")
data14 <- readJsonFile("data14.json")
print("Loading 14... OK")

print("Loading 15...")
data15 <- readJsonFile("data15.json")
print("Loading 15... OK")

print("Loading 16...")
data16 <- readJsonFile("data16.json")
print("Loading 16... OK")

print("Reading JSON files Ok")
print(Sys.time())

sink()
print("Reading JSON files Ok")
print(Sys.time())
print("Joining Data - dataSource")
dataSource <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12, data13, data14, data15, data16)
sink(file = outputFile, append = TRUE)
dataSource <- distinct(dataSource)
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

nameChart<-"Chart00-MissingData.png"
print(paste(nameChart, Sys.time()))

png(filename = paste(urlFigures,nameChart,sep = ""), 
    height = 800, 
    width = 800)
aggr(dataSource, col = c("yellow", "green", "gray"), only.miss = TRUE, sortVars = TRUE, numbers = TRUE)
graphics.off()

sink()
print("Cleaning DataSource")
sink(outputFile, append = TRUE)
print("Cleaning DataSource - Delete rows with null data")

dataClean <- na.omit(dataSource)

print("Cleaning DataSource - Delete duplicated rows")
dataClean <- distinct(dataClean)

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

print("***********************************")
print("***********************************")
print("Type of DataClean")
print(typeof(dataClean))
print("Number of rows")
print(nrow(dataClean))
print("Number of columns")
print(ncol(dataClean))
print("Structure")
print(str(dataClean))
print("Summary")
print(summary(dataClean))
print("***********************************")
print("***********************************")

sink()
print("Query DataActivity")
print(Sys.time())
sink(outputFile, append = TRUE)
print("Query DataActivity")
print(Sys.time())

dataActivity <- sqldf("SELECT dspLat as qLat, 
                      dspLong as qLong,
                      dspActy as qActy,
                      case when dspActy==0 then '0-VEHICLE' 
                      when dspActy==1 then '1-BICYCLE' 
                      when dspActy==2 then '2-FOOT'
                      when dspActy==3 then '3-STILL'
                      when dspActy==4 then '4-UNKNOWN'
                      when dspActy==5 then '5-TILTING'
                      when dspActy==7 then '7-WALKING'
                      when dspActy==8 then '8-RUNNING'
                      end as qActyDesc, 
                      dspVeloKmH as qVeloKmH,
                      CAST(dspHour AS SIGNED) as qHour, 
                      CAST(dspWeekDay AS SIGNED) as qWeekDay,
                      case when dspWeekDay==1 then '1\nMON'
                      when dspWeekDay==2 then '2\nTUE'
                      when dspWeekDay==3 then '3\nWED'
                      when dspWeekDay==4 then '4\nTHU'
                      when dspWeekDay==5 then '5\nFRI'
                      when dspWeekDay==6 then '6\nSAT'
                      when dspWeekDay==7 then '7\nSUN'
                      end as qWeekDayDesc
                      FROM dataClean")

dataActyInMotion <- sqldf("SELECT dspLat as qLat, 
                          dspLong as qLong,
                          dspActy as qActy,
                          case when dspActy==0 then '0-VEHICLE' 
                          when dspActy==1 then '1-BICYCLE' 
                          when dspActy==2 then '2-FOOT'
                          when dspActy==3 then '3-STILL'
                          when dspActy==4 then '4-UNKNOWN'
                          when dspActy==5 then '5-TILTING'
                          when dspActy==7 then '7-WALKING'
                          when dspActy==8 then '8-RUNNING'
                          end as qActyDesc, 
                          dspVeloKmH as qVeloKmH,
                          CAST(dspHour AS SIGNED) as qHour, 
                          CAST(dspWeekDay AS SIGNED) as qWeekDay,
                          case when dspWeekDay==1 then '1\nMON'
                          when dspWeekDay==2 then '2\nTUE'
                          when dspWeekDay==3 then '3\nWED'
                          when dspWeekDay==4 then '4\nTHU'
                          when dspWeekDay==5 then '5\nFRI'
                          when dspWeekDay==6 then '6\nSAT'
                          when dspWeekDay==7 then '7\nSUN'
                          end as qWeekDayDesc
                          FROM dataClean
                          WHERE dspFcIn==dspFcUp")


dataActyNoMotion <- sqldf("SELECT dspLat as qLat, 
                          dspLong as qLong,
                          dspActy as qActy,
                          case when dspActy==0 then '0-VEHICLE' 
                          when dspActy==1 then '1-BICYCLE' 
                          when dspActy==2 then '2-FOOT'
                          when dspActy==3 then '3-STILL'
                          when dspActy==4 then '4-UNKNOWN'
                          when dspActy==5 then '5-TILTING'
                          when dspActy==7 then '7-WALKING'
                          when dspActy==8 then '8-RUNNING'
                          end as qActyDesc, 
                          dspVeloKmH as qVeloKmH,
                          CAST(dspHour AS SIGNED) as qHour, 
                          CAST(dspWeekDay AS SIGNED) as qWeekDay,
                          case when dspWeekDay==1 then '1\nMON'
                          when dspWeekDay==2 then '2\nTUE'
                          when dspWeekDay==3 then '3\nWED'
                          when dspWeekDay==4 then '4\nTHU'
                          when dspWeekDay==5 then '5\nFRI'
                          when dspWeekDay==6 then '6\nSAT'
                          when dspWeekDay==7 then '7\nSUN'
                          end as qWeekDayDesc
                          FROM dataClean
                          WHERE dspFcIn!=dspFcUp")

print("***********************************")
print("***********************************")
print("Type of DataActivity")
print(typeof(dataActivity))
print("Number of rows")
print(nrow(dataActivity))
print("Number of columns")
print(ncol(dataActivity))
print("Structure")
print(str(dataActivity))
print("Summary")
print(summary(dataActivity))
print("***********************************")
print("***********************************")

print("***********************************")
print("***********************************")
print("Type of DataActyInMotion")
print(typeof(dataActyInMotion))
print("Number of rows")
print(nrow(dataActyInMotion))
print("Number of columns")
print(ncol(dataActyInMotion))
print("Structure")
print(str(dataActyInMotion))
print("Summary")
print(summary(dataActyInMotion))
print("***********************************")
print("***********************************")

print("***********************************")
print("***********************************")
print("Type of DataActyNoMotion")
print(typeof(dataActyNoMotion))
print("Number of rows")
print(nrow(dataActyNoMotion))
print("Number of columns")
print(ncol(dataActyNoMotion))
print("Structure")
print(str(dataActyNoMotion))
print("Summary")
print(summary(dataActyNoMotion))
print("***********************************")
print("***********************************")

print("Query DataActivity OK!!!")
print(Sys.time())

sink()
print("Query DataActivity OK!!!")
print(Sys.time())
print("Reading CSV files..")
print(Sys.time())
sink(outputFile, append = TRUE)
print("Reading CSV files..")
print(Sys.time())

dataDevice <- read.csv(paste(urlData,"DataDevice.csv",sep = ""))

print("***********************************")
print("***********************************")
print("Type of DataDevice")
print(typeof(dataDevice))
print("Number of rows")
print(nrow(dataDevice))
print("Number of columns")
print(ncol(dataDevice))
print("Structure")
print(str(dataDevice))
print("Summary")
print(summary(dataDevice))
print("***********************************")
print("***********************************")

dataQuest <- read.csv(paste(urlData,"DataQuestionnaire.csv",sep = ""))

print("***********************************")
print("***********************************")
print("Type of DataQuest")
print(typeof(dataQuest))
print("Number of rows")
print(nrow(dataQuest))
print("Number of columns")
print(ncol(dataQuest))
print("Structure")
print(str(dataQuest))
print("Summary")
print(summary(dataQuest))
print("***********************************")
print("***********************************")

print("Reading CSV files OK!!")
print(Sys.time())

sink()
print("Reading CSV files OK!!")
print(Sys.time())
print("Maps of Quito")
sink(outputFile, append = TRUE)

print("Register API Google Maps")
register_google(key = "AIzaSyDM3eTYsoIVaAdZJPMyEy9xO-pqXKryVjE")

print("Query Map of Quito")
mapQuito <- get_map(location = c(lat = -0.183446, long= -78.418674), zoom = 11)

sink()
print("Maps of Quito OK!!")
print("Saving Data...")
sink(outputFile, append = TRUE)
print("Saving Data...")

save(dataSource,
     dataClean,
     dataActivity,
     dataActyInMotion,
     dataActyNoMotion,
     dataDevice, 
     dataQuest, 
     mapQuito, 
     file=paste(urlRawData,"SmartGPSData.RData",sep = ""))

print("Saving Data OK!!!")
print("End Script!!!")
print(Sys.time())

sink()
print("Saving Data OK!!!")
print("End Script!!!")
print(Sys.time())
