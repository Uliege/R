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

print("Start..")
print("Script: loadData.R")
print(Sys.time())

outputFile = paste(urlReports,"outputLoadData.txt",sep = "")

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

  dataFile <- data.frame(dspId, dspLat, dspLong, dspNSat, dspAlti, dspVelo, dspActy, dspAccu, dspFcIn, dspFcUp)
  
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
print("Generating dataSource....")

sink(file = outputFile, append = TRUE)
print("Generating dataSource....")
print("Join All Data")
dataJsonFiles <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12, data13, data14, data15, data16)
print("Join All Data OK")
sink()

print("Columns and Duplicates ")
print(nrow(dataJsonFiles))
print(table(duplicated(dataJsonFiles)))

sink(file = outputFile, append = TRUE)
print("Columns and Duplicates ")
print(nrow(dataJsonFiles))
print(table(duplicated(dataJsonFiles)))

print("Deleting Duplicate Data")
dataSource <- distinct(dataJsonFiles)

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
sink()

nameChart<-"Data01-SourceData.png"
print(paste(nameChart, Sys.time()))

sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), 
    height = 800, 
    width = 800)
aggr(dataSource, col = c("yellow", "green", "gray"), only.miss = TRUE, sortVars = TRUE, numbers = TRUE)
graphics.off()
sink()

print("Cleaning dataSource..")
print(Sys.time())

sink(outputFile, append = TRUE)
print("Cleaning dataSource..")

print("Delete rows with NAs in Latitude")
dataClean <- subset(dataSource,!is.na(dataSource$dspLat))

print("Coordinates range of Quito")
latMin = -0.400294
latMax =  0.026711
lonLef = -78.591624
lonRig = -78.270936
dataClean <- subset(dataClean, between(dspLat,latMin,latMax))
dataClean <- subset(dataClean, between(dspLong,lonLef,lonRig))

print("Assign the mean to the Missing Data")

tmp<-subset(dataClean,!is.na(dataClean$dspAlti))
dataClean$dspAlti[is.na(dataClean$dspAlti)] <- mean(tmp$dspAlti)

tmp<-subset(dataClean,!is.na(dataClean$dspNSat))
dataClean$dspNSat[is.na(dataClean$dspNSat)] <- as.integer(mean(tmp$dspNSat))

tmp<-subset(dataClean,!is.na(dataClean$dspVelo))
dataClean$dspVelo[is.na(dataClean$dspVelo)] <- mean(tmp$dspVelo)

tmp<-subset(dataClean,!is.na(dataClean$dspAccu))
dataClean$dspAccu[is.na(dataClean$dspAccu)] <- mean(tmp$dspAccu)

print("Updating Activity based on rules")

dataClean <- sqldf(c("UPDATE dataClean
                      SET dspActy = 2
                      WHERE dspActy is NULL
                      AND dspFcIn==dspFcUp
                      AND dspVelo <= 1.4",
                     "select * from main.dataClean"), 
                   method = "raw")

dataClean <- sqldf(c("UPDATE dataClean
                      SET dspActy = 0
                     WHERE dspActy is NULL
                     AND dspFcIn==dspFcUp
                     AND dspVelo > 1.4",
                     "select * from main.dataClean"), 
                   method = "raw")

dataClean <- sqldf(c("UPDATE dataClean
                     SET dspActy = 3
                     WHERE dspActy is NULL
                     AND dspFcIn!=dspFcUp",
                     "select * from main.dataClean"), 
                   method = "raw")

dataClean <- sqldf(c("UPDATE dataClean
                     SET dspActy = 2, dspFcIn=dspFcUp
                     WHERE dspActy is NULL
                     AND dspFcIn is NULL
                     AND dspFcUp is not NULL
                     AND dspVelo <= 1.4",
                     "select * from main.dataClean"), 
                   method = "raw")

dataClean <- sqldf(c("UPDATE dataClean
                     SET dspActy = 2, dspFcUp=dspFcIn
                     WHERE dspActy is NULL
                     AND dspFcIn is not NULL
                     AND dspFcUp is NULL
                     AND dspVelo <= 1.4",
                     "select * from main.dataClean"), 
                   method = "raw")

dataClean <- sqldf(c("UPDATE dataClean
                     SET dspActy = 0, dspFcIn=dspFcUp
                     WHERE dspActy is NULL
                     AND dspFcIn is NULL
                     AND dspFcUp is not NULL
                     AND dspVelo > 1.4",
                     "select * from main.dataClean"), 
                   method = "raw")

dataClean <- sqldf(c("UPDATE dataClean
                     SET dspActy = 0, dspFcUp=dspFcIn
                     WHERE dspActy is NULL
                     AND dspFcIn is not NULL
                     AND dspFcUp is NULL
                     AND dspVelo > 1.4",
                     "select * from main.dataClean"), 
                   method = "raw")
sink()

nameChart<-"Data02-CleaningData.png"
print(paste(nameChart, Sys.time()))


sink(file = outputFile, append = TRUE)
print(paste(nameChart, Sys.time()))
png(filename = paste(urlFigures,nameChart,sep = ""), 
    height = 800, 
    width = 800)
aggr(dataClean, col = c("orange", "purple", "gray"), only.miss = TRUE, sortVars = TRUE, numbers = TRUE)
graphics.off()

print("***********************************")
print("***********************************")
print("DataClean")
print("Number of rows")
print(nrow(dataClean))
print("Number of columns")
print(ncol(dataClean))
print("Structure")
print(str(dataClean))
print("Summary")
print(summary(dataClean))
print("Summary Missing Data")
print(summary(aggr(dataClean, only.miss = TRUE, sortVars = TRUE, plot = FALSE)))
print("***********************************")
print("***********************************")

print("Delete rows with NA in Acty, FcIn and FcUp")
dataClean <- na.omit(dataClean)

print("Aditional Columns")
dataClean$dspVeloKmH <- dataClean$dspVelo*3.6
dataClean$dspHour <- factor(hour(dataClean$dspFcIn))
dataClean$dspWeekDay <- factor(wday(dataClean$dspFcIn))

print("***********************************")
print("***********************************")
print("Final DataClean")
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

dataActivity <- sqldf("SELECT dspId as qId, 
                      dspLat as qLat, 
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
                      case when dspWeekDay==1 then '1\nSUN'
                      when dspWeekDay==2 then '2\nMON'
                      when dspWeekDay==3 then '3\nTUE'
                      when dspWeekDay==4 then '4\nWED'
                      when dspWeekDay==5 then '5\nTHU'
                      when dspWeekDay==6 then '6\nFRI'
                      when dspWeekDay==7 then '7\nSAT'
                      end as qWeekDayDesc
                      FROM dataClean")

dataActivity0123 <- sqldf("SELECT * 
                          FROM dataActivity
                          WHERE qActy in (0,1,2,3)")


print("***********************************")
print("***********************************")
print("dataActivity")
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
print("dataActivity0123")
print("Number of rows")
print(nrow(dataActivity0123))
print("Number of columns")
print(ncol(dataActivity0123))
print("Structure")
print(str(dataActivity0123))
print("Summary")
print(summary(dataActivity0123))
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
print("DataDevice")
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
print("DataQuest")
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
print("Saving Data...")
print(Sys.time())

sink(outputFile, append = TRUE)
print("Saving Data...")
print(Sys.time())
save(dataJsonFiles, 
     dataSource,
     dataClean,
     dataActivity,
     dataActivity0123,
     dataDevice, 
     dataQuest,
     file=paste(urlRawData,"SmartGPSData.RData",sep = ""))

print("Saving Data OK!!!")
print("End Script!!!")
print(Sys.time())

sink()
print("Saving Data OK!!!")
print("End Script!!!")
print(Sys.time())
