library(dplyr)
library(jsonlite)
library(VIM)
library(lubridate)
library(ggmap)
library(sqldf)
library(vcd)


urlData  =   "D:/RSoftware/RProjects/SmartGPSAnalysis/rawdata/"

urlData  =   "D:/tmp/"

readJsonFile <- function(fileName) {
  dataResult <- NULL
  
  archivo <- paste(urlData,fileName,sep = "")
  con <- file(archivo, open="r")
  dataJson <- readLines(con)
  close(con)
  
  dataFile <- fromJSON(dataJson)
  dspFcLoad <- dataFile$fecha
  dspId <- dataFile$dspId
  dspLat <- dataFile$sensor$latitude
  dspLong <- dataFile$sensor$longitude
  dspNSat <- dataFile$sensor$nSatellites
  dspAlti <- dataFile$sensor$altitude
  dspVelo <- dataFile$sensor$velocity
  dspActy <- dataFile$sensor$activity
  dspAccu <- dataFile$sensor$accuracy
  dspFcIn <- dataFile$sensor$dateInsert
  dspFcUp <- dataFile$sensor$dateUpdate
  
  dataFile <- data.frame(dspId, dspLat, dspLong, dspNSat, dspAlti, dspVelo, dspActy, dspAccu, dspFcLoad, dspFcIn, dspFcUp)
  
  return(dataFile)
}

data1 <- readJsonFile("device1036.json")
dataJsonFiles<- data1

roadMapLiege <- get_map(location = "Liège", zoom = 11, maptype = "roadmap") 

ggmap(roadMapQuito)
ggmap(roadMapLiege)

ggmap(roadMapLiege) + 
  geom_point(data = dataClean,
             aes(x = dspLong[], y = dspLat[]), 
             alpha = .5, 
             color="darkred", 
             size = .0001) +
  ggtitle("GPS Point Track") + 
  xlab("Longitude") +
  ylab("Latitude")

dataLatLong <- data.frame(dataClean$dspLat, dataClean$dspLong)


write.csv(dataLatLong, file=paste(urlData,"1033.csv",sep = ""), row.names = F)


urlFigures = "D:/RSoftware/RProjects/SmartGPSAnalysis/figures/"
urlRawData = "D:/RSoftware/RProjects/SmartGPSAnalysis/rawdata/"
urlReports = "D:/RSoftware/RProjects/SmartGPSAnalysis/reports/"

dataFile <- "dataClean.RData"
dataMaps <- "SmartGPSMaps.RData"

rawDataFile <- paste(urlRawData,dataFile,sep = "") 
rawMapsFile <- paste(urlRawData,dataMaps,sep = "")

load(file=rawDataFile)
load(file=rawMapsFile)

sqldf("select dspId, count(dspId) from dataClean group by dspId order by 2 desc")
sqldf("select distinct date(dspFcIn) from data293M")


data293 <- sqldf("select * from dataClean where dspId = 293 order by dspFcIn")

data293M <- sqldf("select * from data293 where dspFcIn = dspFcUp order by dspFcIn")

data293M22 <- sqldf("select * from data293M where date(dspFcIn) = '2019-01-24' order by dspFcIn")


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
                      FROM data293M22")

dataActivity0123 <- sqldf("SELECT * 
                          FROM dataActivity
                          WHERE qActy in (0,1,2,3)")




ggmap(roadMapQuito) + 
  geom_point(data = data293M22,
             aes(x = dspLong[], y = dspLat[]), 
             alpha = .5, 
             color="darkred", 
             size = .0001) +
  ggtitle("GPS Point Track") + 
  xlab("Longitude") +
  ylab("Latitude")


plot(density(data293M22$dspActy), 
     type="l", 
     col="blue", 
     main="Activity Density", 
     asp=.4,
     sub="Activity:   0-Vehicle   1-Bicycle   2-Foot   3-Still")


plot(density(data293M22$dspVeloKmH), 
     type="l", 
     col="blue",  
     main='Velocity Density',
     sub="Velocity (km/h)")

ggplot(dataActivity0123,
       aes(x=qHour, fill = qActyDesc))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Activity Distribution Per Hour") + 
  xlab("Hour of the Day") +
  ylab("Data Collected") +
  guides(fill=guide_legend(title="Activity"))






dataDevice <- read.csv(paste(urlData,"DataDevice.csv",sep = ""))

dataDevice

nameChart<-"Device01-AndrVersionDataCollected.png"
print(paste(nameChart, Sys.time()))
ggplot(dataDevice,
       aes(x=xDAndroV, y=xDNumPoints))+ 
  geom_col(alpha=0.6)+
  ggtitle("GPS Data Collected per Android Version") + 
  xlab("") +
  ylab("Number of GPS Traces")
ggsave(filename = paste(urlFigures,nameChart,sep = ""))


nameChart<-"Device02-AndrVersionDevices.png"
print(paste(nameChart, Sys.time()))
ggplot(dataDevice,
       aes(x=xDAndroV, fill=xDNumPoints))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Number Devices per Android Version") + 
  xlab("") +
  ylab("Number of Devices") 
ggsave(filename = paste(urlFigures,nameChart,sep = ""))





print("")


citation("ggmap")






























plot(density(dataClean$dspAlti), 
     type="l", 
     col="blue", 
     main="Satellite Number Density",
     sub="Number of Satellites")


















install.packages("lsr")
library("lsr")

# participants. Each participant chooses between one of three
# options. Possible data for this experiment:

condition1 <- c(30, 20, 50) 
condition2 <- c(35, 30, 35)
X <- cbind( condition1, condition2 )
rownames(X) <- c( 'choice1', 'choice2', 'choice3' )
print(X)

# To test the null hypothesis that the distribution of choices
# is identical in the two conditions, we would run a chi-square
# test:
chisq.test(X)

# To estimate the effect size we can use Cramer's V:
cramersV( X )  # returns a value of 0.159









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








rm(tmp)

readJsonFile <- function(fileName) {
  dataResult <- NULL
  
  archivo <- paste(urlData,fileName,sep = "")
  con <- file(archivo, open="r")
  dataJson <- readLines(con)
  close(con)
  
  dataFile <- fromJSON(dataJson)
  dspId <- dataFile$dspId
  #dspFc <- dataFile$fecha
  dspLat <- dataFile$sensor$latitude
  dspLong <- dataFile$sensor$longitude
  dspNSat <- dataFile$sensor$numSatelites
  dspAlti <- dataFile$sensor$altitude
  dspVelo <- dataFile$sensor$velocidad
  dspActy <- dataFile$sensor$actividad
  dspAccu <- dataFile$sensor$precision
  dspFcIn <- dataFile$sensor$dateInsert
  dspFcUp <- dataFile$sensor$dateUpdate
  
  #dataFile <- data.frame(dspId, dspFc, dspLat, dspLong, dspNSat, dspAlti, dspVelo, dspActy, dspAccu, dspFcIn, dspFcUp)
  dataFile <- data.frame(dspId, dspLat, dspLong, dspNSat, dspAlti, dspVelo, dspActy, dspAccu, dspFcIn, dspFcUp)
  return(dataFile)
}

rm(data1)
rm(readJsonFile)

urlData = "D:/RSoftware/RProjects/SmartGPSAnalysis/data/"

fileName = "data1.json"

data1 <- readJsonFile("data1.json")
dataSource <- data1

table(duplicated(data1))





x <- as.Date("2019-04-25")
wday(x) #4

wday(ymd(080101))



table(duplicated(dataClean2))

dataSource <- data1

dataClean2 <- sqldf("select * from dataClean order by dspFcIn desc")

data22 <- sqldf("select * from data1 order by dspFcIn desc")

table(duplicated(data2))
table(duplicated(data22))

data3 <- data1[!duplicated(data1), ]

table(duplicated(data3))

dataaa <- distinct(dataSource)


archivo <- paste(urlData,fileName,sep = "")
con <- file(archivo, open="r")
dataJson <- readLines(con)
close(con)

data293 <- fromJSON(dataJson)




rm(a)



df <- tibble(
  x = sample(10, 10, rep = TRUE),
  y = sample(10, 10, rep = TRUE)
)
nrow(df)
nrow(distinct(df))
nrow(distinct(df, x, y))

distinct(df, x)
distinct(df, y)

df1 <- sqldf("select * from df order by x, y")

table(duplicated(df))

xx <- distinct(df)



print("Correlation Matrix - kendall Coefficient")
print(cor(dataNum, method = "kendall"))


datos = read.csv("http://www.businessandeconomics.mq.edu.au/our_departments/Applied_Finance_and_Actuarial_Studies/acst_docs/glms_for_insurance_data/data/car.csv")


hist(datos$agecat)
summary(datos)
tabla = ftable(as.factor(datos$agecat), datos$area,
               dnn = c("Edad", "Valor"))
str(datos)

library(vcd)
assocstats(tabla)


hist(dataActivity$qActy)
str(dataActivity)

str(dataClean)


rm(clusters)




str(dataActivity)



dataaa <- sqldf("SELECT distinct(dspId) dspId, count(dspId) numpoints
                FROM dataSource
                GROUP BY dspId")

install.packages("xlsx")
library(xlsx)


write.xlsx(dataPoints, "D:/tmp/mydata.xlsx")

urlData = "D:/tmp/"

write.csv(dataPoints, paste(urlData,"DataPoints.csv",sep = ""))

write.xlsx(dataPoints, "D:/tmp/mydata.xlsx")



table(dataActivity$qActy)

aggr()






save(dataSource,
     file=paste(urlRawData,"dataSource.RData",sep = ""))


save(dataClean,
     file=paste(urlRawData,"dataClean.RData",sep = ""))

