#Script para crear el dataset que contiene SDE y MST

#Archivo para almacenar el log de ejecución
#fileLog = paste(urlLogs,"4.4.1Read-weekData",trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
#Conexión para escibir en el archivo
#con <- file(fileLog, open="a")
#Hora de inicio del proceso
#writeLines(text = as.character(Sys.time()), con = con)


#Delete  work space variables (Caution!!)
rm(list=ls())
shell("cls")

library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(lubridate)


#Directorios (url) de trabajo del proyecto
#urlProyecto = 'D:/G/GitHub/Uliege/R/TimeLineAnalysis/'
urlProyecto = getwd()
urlData = paste(urlProyecto,"/data/",sep = "")
urlFigures = paste(urlProyecto,"/figures/",sep = "")
urlModels = paste(urlProyecto,"/models/",sep = "")
urlRawData = paste(urlProyecto,"/dataRaw/",sep = "")
urlReports = paste(urlProyecto,"/reports/",sep = "")
urlOutput = paste(urlProyecto,"/output/",sep = "")
urlExcelData = paste(urlProyecto,"/dataExcel/",sep = "")
urlLogs = paste(urlProyecto,"/logs/",sep = "")


############################################################################################################################
############################################################################################################################
##### Demographic Data 
############################################################################################################################
############################################################################################################################

#Load demographyc data
aDataDemographic = NULL

#First group
dataDemFile = "dataDemographic.csv"
urlDataDem = paste(urlData,dataDemFile,sep="")
aDataDemographic1 = read.csv(urlDataDem)
aDataDemographic1 = aDataDemographic1[-2]
names(aDataDemographic1)[1] = 'idPerson' 
names(aDataDemographic1)[2] = 'university_individual'
names(aDataDemographic1)[3] = 'age'
names(aDataDemographic1)[4] = 'gender'
names(aDataDemographic1)[5] = 'residential_location'
names(aDataDemographic1)[6] = 'own_vehicle'
names(aDataDemographic1)[7] = 'usual_transport_pattern' 
aDataDemographic1$homeType = ''
aDataDemographic1$workingStatus = ''
aDataDemographic1$workplaceLocation = ''
aDataDemographic1$householdSize = 0
aDataDemographic1$childrenU12 = ''
aDataDemographic1$incomes = ''

#Second group
dataDemFile = "dataDemographic2.csv"
urlDataDem = paste(urlData,dataDemFile,sep="")
aDataDemographic2 = read.csv(urlDataDem)
aDataDemographic2 = aDataDemographic2[-2]
names(aDataDemographic2)[1] = 'idPerson' 
names(aDataDemographic2)[2] = 'university_individual'
names(aDataDemographic2)[3] = 'age'
names(aDataDemographic2)[4] = 'gender'
names(aDataDemographic2)[5] = 'residential_location'
names(aDataDemographic2)[6] = 'own_vehicle'
names(aDataDemographic2)[7] = 'usual_transport_pattern' 
aDataDemographic2$homeType = ''
aDataDemographic2$workingStatus = ''
aDataDemographic2$workplaceLocation = ''
aDataDemographic2$householdSize = 0
aDataDemographic2$childrenU12 = ''
aDataDemographic2$incomes = ''

#Third group
dataDemFile = "dataDemographic3.csv"
urlDataDem = paste(urlData,dataDemFile,sep="")
aDataDemographic3 = read.csv(urlDataDem)
aDataDemographic3 = aDataDemographic3[-2]
names(aDataDemographic3)[1] = 'idPerson' 
names(aDataDemographic3)[2] = 'university_individual'
names(aDataDemographic3)[3] = 'age'
names(aDataDemographic3)[4] = 'gender'
names(aDataDemographic3)[5] = 'residential_location'
names(aDataDemographic3)[6] = 'own_vehicle'
names(aDataDemographic3)[7] = 'usual_transport_pattern' 

#Join all databases of demographic information
aDataDemographic = rbind(aDataDemographic1, aDataDemographic2, aDataDemographic3)
rm(aDataDemographic1)
rm(aDataDemographic2)
rm(aDataDemographic3)

#Update Ages with error (Known Individuals)
aDataDemographic[aDataDemographic$idPerson == "tld75", "age"] = 26
aDataDemographic[aDataDemographic$idPerson == "tld110", "age"] = 25
aDataDemographic[aDataDemographic$idPerson == "tld130", "age"] = 27
aDataDemographic[aDataDemographic$idPerson == "tld144", "age"] = 24
aDataDemographic[aDataDemographic$idPerson == "tld156", "age"] = 24

#Update Residence Location with error (Known Individuals)
aDataDemographic[aDataDemographic$idPerson == "tld97", "residential_location"] = 'Centre'

#Update data for individual number 0
aDataDemographic[aDataDemographic$idPerson == "tld0", "homeType"] = 'Own'
aDataDemographic[aDataDemographic$idPerson == "tld0", "workingStatus"] = 'Yes'
aDataDemographic[aDataDemographic$idPerson == "tld0", "workplaceLocation"] = 'Centre'
aDataDemographic[aDataDemographic$idPerson == "tld0", "householdSize"] = 4
aDataDemographic[aDataDemographic$idPerson == "tld0", "childrenU12"] = 'Yes'
aDataDemographic[aDataDemographic$idPerson == "tld0", "incomes"] = 'Medium'

#Unify UsualTRansportPattern (Public Transportation & Others)
aDataDemographic[aDataDemographic$usual_transport_pattern == "Public Transportation",]$usual_transport_pattern = 'Public Transportation & Others'
aDataDemographic[aDataDemographic$usual_transport_pattern == "On foot",]$usual_transport_pattern = 'Public Transportation & Others'

#New columns
aDataDemographic$idPersonNum = as.numeric(substring(aDataDemographic$idPerson, first = 4))

#Delete repeated data 
aDataDemographic = aDataDemographic %>% filter(idPerson != 'tld161')
data.demographic = aDataDemographic %>% filter(!(idPersonNum %in% c(159,193,194,204,205,206,207,211,213,222,228,231,233,237,244,247,252,254,257,258,260)))

data.demographic = select(data.demographic, idPerson, idPersonNum, university_individual, age, gender, residential_location, 
                          own_vehicle, usual_transport_pattern, homeType, workingStatus, workplaceLocation, householdSize, 
                          childrenU12, incomes)

data.demographic = data.demographic %>% arrange(idPersonNum)

rm(aDataDemographic)
rm(dataDemFile)
rm(urlDataDem)

names(data.demographic)

names(data.demographic)[3] = 'universityIndividual'
names(data.demographic)[6] = 'homeLocation'
names(data.demographic)[7] = 'vehicleOwner'
names(data.demographic)[8] = 'usualTransportPattern'

data.demographic$idPerson = as.factor(data.demographic$idPerson)
data.demographic$universityIndividual = as.factor(data.demographic$universityIndividual)
data.demographic$gender = as.factor(data.demographic$gender)
data.demographic$homeLocation = as.factor(data.demographic$homeLocation)
data.demographic$vehicleOwner = as.factor(data.demographic$vehicleOwner)
data.demographic$usualTransportPattern = as.factor(data.demographic$usualTransportPattern)
data.demographic$homeType = as.factor(data.demographic$homeType)
data.demographic$workingStatus = as.factor(data.demographic$workingStatus)
data.demographic$workplaceLocation = as.factor(data.demographic$workplaceLocation)
data.demographic$childrenU12 = as.factor(data.demographic$childrenU12)
data.demographic$incomes = as.factor(data.demographic$incomes)

summary(data.demographic)


############################################################################################################################
############################################################################################################################
##### Home Data
############################################################################################################################
############################################################################################################################

#Load Home Data
homeDataFile = 'Home/Home-tldTotal 0-173.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData1 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 174-269.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData2 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 271-308.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData3 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 309-313.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData4 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 314-332.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData5 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 333-337.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData6 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 338-348.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData7 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 349-389.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData8 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 390-397.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData9 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 398-419.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData10 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 420-445.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData11 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 446-459.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData12 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 460-468.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData13 = read.csv(urlDataHome)

homeDataFile = 'Home/Home-tldTotal 469-473.csv'
urlDataHome = paste(urlExcelData,homeDataFile,sep="")
homeData14 = read.csv(urlDataHome)

homeData = rbind(homeData1, homeData2, homeData3, homeData4, homeData5, homeData6, homeData7, homeData8, 
                 homeData9, homeData10, homeData11, homeData12, homeData13, homeData14)

names(homeData)[2] = 'idPerson' 
names(homeData)[4] = 'lon' 
homeData$idPersonNum = as.numeric(substring(homeData$idPerson, first = 4))

#Delete duplicated old information
data.home = homeData %>% filter(!(idPersonNum %in% c(159,193,194,204,205,206,207,211,213,222,228,231,233,237,244,247,252,254,257,258,260)))

data.home = data.home %>% select(idPerson, idPersonNum, lon, lat, maps)

#Transform coordinates (lon, lat) from GPS degrees decimal (4362) to Ecuador projection in meters (32617) 
coord = select(data.home, lon, lat)
coord = coord %>% st_as_sf(coords = c("lon", "lat"))
coord = st_set_crs(coord, 4326)
coord = st_transform(coord, 32617)
coordDf = data.frame(st_coordinates(coord[,1]))
data.home$lon.X = coordDf$X
data.home$lat.Y = coordDf$Y

#Delete variables
rm(coord)
rm(coordDf)
rm(homeDataFile)
rm(homeData1)
rm(homeData2)
rm(homeData3)
rm(homeData4)
rm(homeData5)
rm(homeData6)
rm(homeData7)
rm(homeData8)
rm(homeData9)
rm(homeData10)
rm(homeData11)
rm(homeData12)
rm(homeData13)
rm(homeData14)
rm(homeData)
rm(urlDataHome)

summary(data.home)


############################################################################################################################
############################################################################################################################
##### Trip Data
############################################################################################################################
############################################################################################################################

#Load Trip Data
tripDataFile = 'Trip/Trips-tldTotal 0-173.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData1 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 174-269.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData2 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 271-308.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData3 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 309-313.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData4 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 314-332.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData5 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 333-337.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData6 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 338-348.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData7 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 349-389.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData8 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 390-397.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData9 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 398-419.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData10 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 420-445.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData11 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 446-459.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData12 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 460-468.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData13 = read.csv(urlDataTrip)

tripDataFile = 'Trip/Trips-tldTotal 469-473.csv'
urlDataTrip = paste(urlExcelData,tripDataFile,sep="")
tripData14 = read.csv(urlDataTrip)

tripData = rbind(tripData1, tripData2, tripData3, tripData4, tripData5, tripData6, tripData7, tripData8,
                 tripData9, tripData10, tripData11, tripData12, tripData13, tripData14)

names(tripData)[2] = 'idPerson'
tripData$idPersonNum = as.numeric(substring(tripData$idPerson, first = 4))

#Delete duplicated old information
data.trips = tripData %>% filter(!(idPersonNum %in% c(159,193,194,204,205,206,207,211,213,222,228,231,233,237,244,247,252,254,257,258,260)))

data.trips = data.trips %>% 
  select(idPerson, idPersonNum, idWeek, idTrip, tripTimeMin, tripLenKm, tripPoints)

data.trips = data.trips %>% 
  group_by(idPerson, idPersonNum, idWeek) %>% 
  summarise( numTrips = max(idTrip)+1, 
             numPoints = sum(tripPoints), 
             totalMin = sum(tripTimeMin),
             totalKm = sum(tripLenKm))

data.trips = data.trips %>% arrange(idPersonNum)

#Delete variables
rm(tripData1)
rm(tripData2)
rm(tripData3)
rm(tripData4)
rm(tripData5)
rm(tripData6)
rm(tripData7)
rm(tripData8)
rm(tripData9)
rm(tripData10)
rm(tripData11)
rm(tripData12)
rm(tripData13)
rm(tripData14)
rm(tripData)
rm(tripDataFile)
rm(urlDataTrip)


############################################################################################################################
############################################################################################################################
##### Activity Points Data (Stops)
############################################################################################################################
############################################################################################################################

#Read files generated in python
pName = "POI/tldTotal 0-173"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints1 = read.csv(excelFile)
}

pName = "POI/tldTotal 174-269"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints2 = read.csv(excelFile)
}

pName = "POI/tldTotal 271-308"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints3 = read.csv(excelFile)
}

pName = "POI/tldTotal 309-313"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints4 = read.csv(excelFile)
}

pName = "POI/tldTotal 314-332"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints5 = read.csv(excelFile)
}

pName = "POI/tldTotal 333-337"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints6 = read.csv(excelFile)
}

pName = "POI/tldTotal 338-348"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints7 = read.csv(excelFile)
}

pName = "POI/tldTotal 349-389"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints8 = read.csv(excelFile)
}

pName = "POI/tldTotal 390-397"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints9 = read.csv(excelFile)
}

pName = "POI/tldTotal 398-419"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints10 = read.csv(excelFile)
}

pName = "POI/tldTotal 420-445"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints11 = read.csv(excelFile)
}

pName = "POI/tldTotal 446-459"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints12 = read.csv(excelFile)
}

pName = "POI/tldTotal 460-468"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints13 = read.csv(excelFile)
}

pName = "POI/tldTotal 469-473"
idFile = pName
excelFile = paste(urlExcelData,idFile,'.csv',sep="")
if(file.exists(excelFile)){
  activityPoints14 = read.csv(excelFile)
}

activityPoints = rbind(activityPoints1, activityPoints2, activityPoints3, activityPoints4, activityPoints5, activityPoints6, 
                       activityPoints7, activityPoints8, activityPoints9, activityPoints10, activityPoints11, activityPoints12,
                       activityPoints13, activityPoints14)

#Change name columns
names(activityPoints)[3] = 'lon' 
names(activityPoints)[11] = 'idPerson'

#Add columns
activityPoints$maps = paste(activityPoints$lat, activityPoints$lon, sep = ',')
activityPoints$idPersonNum = as.numeric(substring(activityPoints$idPerson, first = 4))
activityPoints$idDay <- substr(activityPoints$datetime, 1, 10)

#Delete duplicated old information
activityPoints = activityPoints %>% filter(!(idPersonNum %in% c(159,193,194,204,205,206,207,211,213,222,228,231,233,237,244,247,252,254,257,258,260)))

#Summary Stops (Activity Points)
data.stops = activityPoints %>% 
  select(idPerson, idPersonNum, idWeek) %>%
  group_by(idPerson, idPersonNum, idWeek) %>% 
  count()

names(data.stops)[4] = 'activityPoints'

#Join Trip and Stop data
data.trips.stops = data.trips %>% 
  full_join(data.stops, by.x = 'idPerson', by.y = 'idweek') %>% 
  na.omit() %>%
  arrange(idPersonNum)

#Summary Days per Week
daysWeek = activityPoints %>% 
  select(idPerson, idWeek, day) %>%
  group_by(idPerson, idWeek, day) %>%
  count() %>%
  group_by(idPerson, idWeek) %>%
  count()

names(daysWeek)[3] = 'numDays'

data.trips.stops = data.trips.stops %>% 
  full_join(daysWeek, by.x = 'idPerson', by.y = 'idweek') %>% 
  na.omit() %>%
  arrange(idPersonNum)

#Summary Clusters (Clusters per Person)
data.clusters = activityPoints %>% 
  select(idPerson, idPersonNum, lon, lat, cluster) %>% 
  group_by(idPerson, idPersonNum, cluster) %>% 
  summarise(lon = mean(lon), 
            lat = mean(lat),
            maps = paste(lat, lon, sep = ',')) %>%
  ungroup()

freq = activityPoints %>% 
  select(idPerson, idPersonNum, cluster) %>% 
  group_by(idPerson, idPersonNum) %>% 
  summarise(numCluster = plyr::count(cluster))

data.clusters = data.clusters %>% 
  cbind(data.frame('frequency' = freq$numCluster$freq))

data.clusters = data.clusters %>% 
  filter(frequency > 1 ) %>% #Exclude individual clusters
  arrange(idPersonNum, desc(frequency), cluster)

#Transform coordinates (lon, lat) from GPS degrees decimal (4362) to Ecuador projection in meters (32617) 
coord = select(data.clusters, lon, lat)
coord = coord %>% st_as_sf(coords = c("lon", "lat"))
coord = st_set_crs(coord, 4326)
coord = st_transform(coord, 32617)
coordDf = data.frame(st_coordinates(coord[,1]))
data.clusters$lon.X = coordDf$X
data.clusters$lat.Y = coordDf$Y

data.clusters = data.clusters %>%
  select(idPerson, idPersonNum, cluster, frequency, lon, lat, maps, lon.X, lat.Y)


#Clusters per Person and Week
data.clusters.week = activityPoints %>% 
  select(idPerson, idPersonNum, idWeek, lon, lat, cluster) %>% 
  group_by(idPerson, idPersonNum, idWeek, cluster) %>% 
  summarise(lon = mean(lon), 
            lat = mean(lat),
            maps = paste(lat, lon, sep = ','))%>%
  ungroup()

freq = activityPoints %>% 
  select(idPerson, idPersonNum, idWeek, cluster) %>% 
  group_by(idPerson, idPersonNum, idWeek) %>% 
  summarise(numCluster = plyr::count(cluster))

data.clusters.week = data.clusters.week %>% 
  cbind(data.frame('frequency' = freq$numCluster$freq))

data.clusters.week = data.clusters.week %>%
  filter(frequency > 1) %>% #Exclude individual clusters
  arrange(idPersonNum, idWeek, desc(frequency), cluster)

#Transform coordinates (lon, lat) from GPS degrees decimal (4362) to Ecuador projection in meters (32617) 
coord = select(data.clusters.week, lon, lat)
coord = coord %>% st_as_sf(coords = c("lon", "lat"))
coord = st_set_crs(coord, 4326)
coord = st_transform(coord, 32617)
coordDf = data.frame(st_coordinates(coord[,1]))
data.clusters.week$lon.X = coordDf$X
data.clusters.week$lat.Y = coordDf$Y

data.clusters.week = data.clusters.week %>%
  select(idPerson, idPersonNum, idWeek, cluster, frequency, lon, lat, maps, lon.X, lat.Y)


#Clusters per Person and Day
#data.clusters.day = activityPoints %>% 
#  select(idPerson, idPersonNum, idWeek, idDay, lon, lat, cluster) %>% 
#  group_by(idPerson, idPersonNum, idWeek, idDay, cluster) %>% 
#  summarise(lon = mean(lon), 
#            lat = mean(lat),
#            maps = paste(lat, lon, sep = ',')) %>%
#  ungroup()

#freq = activityPoints %>% 
#  select(idPerson, idPersonNum, idWeek, idDay, cluster) %>% 
#  group_by(idPerson, idPersonNum, idWeek, idDay) %>% 
#  summarise(numCluster = plyr::count(cluster))

#data.clusters.day = data.clusters.day %>% 
#  cbind(data.frame('frequency' = freq$numCluster$freq))

#data.clusters.day = data.clusters.day %>%
#  arrange(idPersonNum, desc(frequency), cluster)

#Transform coordinates (lon, lat) from GPS degrees decimal (4362) to Ecuador projection in meters (32617) 
#coord = select(data.clusters.day, lon, lat)
#coord = coord %>% st_as_sf(coords = c("lon", "lat"))
#coord = st_set_crs(coord, 4326)
#coord = st_transform(coord, 32617)
#coordDf = data.frame(st_coordinates(coord[,1]))
#data.clusters.day$lon.X = coordDf$X
#data.clusters.day$lat.Y = coordDf$Y

#Delete variables
rm(activityPoints1)
rm(activityPoints2)
rm(activityPoints3)
rm(activityPoints4)
rm(activityPoints5)
rm(activityPoints6)
rm(activityPoints7)
rm(activityPoints8)
rm(activityPoints9)
rm(activityPoints10)
rm(activityPoints11)
rm(activityPoints12)
rm(activityPoints13)
rm(activityPoints14)
rm(coord)
rm(coordDf)
rm(freq)
rm(excelFile)
rm(idFile)
rm(pName)
rm(daysWeek)
rm(data.stops)
rm(data.trips)
rm(activityPoints)


############################################################################################################################
############################################################################################################################
##### Loading SDE Data - Weekly
############################################################################################################################
############################################################################################################################

#Load SDE Data
fileName = 'sdeWeek2021-07-01'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'aspace/',fileRawData,sep = "")) #0 - 269
aspWeekSDEatt1 = aspWeekSDEatt

fileName = 'sdeWeek2021-08-11'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'aspace/',fileRawData,sep = "")) #271 - 313
aspWeekSDEatt2 = aspWeekSDEatt

fileName = 'sdeWeek2021-08-12(0)'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'aspace/',fileRawData,sep = "")) #315 - 337
aspWeekSDEatt3 = aspWeekSDEatt

fileName = 'sdeWeek2021-08-12(1)'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'aspace/',fileRawData,sep = "")) #338 - 348
aspWeekSDEatt4 = aspWeekSDEatt

fileName = 'sdeWeek2021-08-13(0)'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'aspace/',fileRawData,sep = "")) #349 - 389
aspWeekSDEatt5 = aspWeekSDEatt

fileName = 'sdeWeek2021-08-13(1)'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'aspace/',fileRawData,sep = "")) #390 - 397
aspWeekSDEatt6 = aspWeekSDEatt

fileName = 'sdeWeek2021-08-14'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'aspace/',fileRawData,sep = "")) #398 - 419
aspWeekSDEatt7 = aspWeekSDEatt

fileName = 'sdeWeek2021-08-15'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'aspace/',fileRawData,sep = "")) #420 - 445
aspWeekSDEatt8 = aspWeekSDEatt

fileName = 'sdeWeek2021-08-19'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'aspace/',fileRawData,sep = "")) #446 - 459
aspWeekSDEatt9 = aspWeekSDEatt

fileName = 'sdeWeek2021-08-20'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'aspace/',fileRawData,sep = "")) #460 - 468
aspWeekSDEatt10 = aspWeekSDEatt

fileName = 'sdeWeek2021-08-21'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'aspace/',fileRawData,sep = "")) #469 - 473
aspWeekSDEatt11 = aspWeekSDEatt


aspWeekSDEatt = rbind(aspWeekSDEatt1, aspWeekSDEatt2, aspWeekSDEatt3, aspWeekSDEatt4, aspWeekSDEatt5,aspWeekSDEatt6, 
                      aspWeekSDEatt7, aspWeekSDEatt8, aspWeekSDEatt9, aspWeekSDEatt10, aspWeekSDEatt11)

aspWeekSDEatt = aspWeekSDEatt %>% select(idFile, idWeek, activityPoints, Area.sde, Eccentricity, CENTRE.x, CENTRE.y, Sigma.x, Sigma.y)

#Rename & add Columns
names(aspWeekSDEatt)[1] = 'idPerson'
aspWeekSDEatt$idPersonNum = as.numeric(substring(aspWeekSDEatt$idPerson, first = 4))

#Delete duplicated old information
data.SDE.week = aspWeekSDEatt %>% filter(!(idPersonNum %in% c(159,193,194,204,205,206,207,211,213,222,228,231,233,237,244,247,252,254,257,258,260)))

#Add major Sigma column
data.SDE.week = data.SDE.week %>%
  mutate(majorSigma = ifelse(Sigma.x > Sigma.y, Sigma.x, Sigma.y))

#Express all measures in km or km2
# 1km = 1000 m
# 1km2 = 1000*1000 m2
data.SDE.week$Area.sde = data.SDE.week$Area.sde/(1000*1000)
data.SDE.week$Sigma.x = data.SDE.week$Sigma.x/1000
data.SDE.week$Sigma.y = data.SDE.week$Sigma.y/1000
data.SDE.week$majorSigma = data.SDE.week$majorSigma/1000

data.SDE.week = data.SDE.week %>% 
  select(idPerson, idPersonNum, idWeek, activityPoints, Area.sde, Eccentricity, CENTRE.x, CENTRE.y, Sigma.x, Sigma.y, majorSigma)

#Delete Variables
rm(aspWeekSDEloc)
rm(aspWeekSDEatt1)
rm(aspWeekSDEatt2)
rm(aspWeekSDEatt3)
rm(aspWeekSDEatt4)
rm(aspWeekSDEatt5)
rm(aspWeekSDEatt6)
rm(aspWeekSDEatt7)
rm(aspWeekSDEatt8)
rm(aspWeekSDEatt9)
rm(aspWeekSDEatt10)
rm(aspWeekSDEatt11)
rm(aspWeekSDEatt)
rm(fileName)
rm(fileRawData)


############################################################################################################################
############################################################################################################################
##### Loading MST Data - Weekly
############################################################################################################################
############################################################################################################################

#Load MST Data
fileName = 'MSTWeekTotal2021-07-06'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'MST/',fileRawData,sep = ""))#0 - 269
MSTWeekSummary1 = MSTWeekSummary

fileName = 'MSTWeekTotal2021-08-11'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'MST/',fileRawData,sep = ""))#271 - 313
MSTWeekSummary2 = MSTWeekSummary

fileName = 'MSTWeekTotal2021-08-12(0)'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'MST/',fileRawData,sep = ""))#315 - 337
MSTWeekSummary3 = MSTWeekSummary

fileName = 'MSTWeekTotal2021-08-12(1)'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'MST/',fileRawData,sep = ""))#338 - 348
MSTWeekSummary4 = MSTWeekSummary

fileName = 'MSTWeekTotal2021-08-13(0)'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'MST/',fileRawData,sep = ""))#349 - 389
MSTWeekSummary5 = MSTWeekSummary

fileName = 'MSTWeekTotal2021-08-13(1)'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'MST/',fileRawData,sep = ""))#390 - 397
MSTWeekSummary6 = MSTWeekSummary

fileName = 'MSTWeekTotal2021-08-14'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'MST/',fileRawData,sep = ""))#398 - 419
MSTWeekSummary7 = MSTWeekSummary

fileName = 'MSTWeekTotal2021-08-15'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'MST/',fileRawData,sep = ""))#420 - 445
MSTWeekSummary8 = MSTWeekSummary

fileName = 'MSTWeekTotal2021-08-19'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'MST/',fileRawData,sep = ""))#446 - 459
MSTWeekSummary9 = MSTWeekSummary

fileName = 'MSTWeekTotal2021-08-20'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'MST/',fileRawData,sep = ""))#460 - 468
MSTWeekSummary10 = MSTWeekSummary

fileName = 'MSTWeekTotal2021-08-21'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,'MST/',fileRawData,sep = ""))#469 - 473
MSTWeekSummary11 = MSTWeekSummary

MSTWeekSummary = rbind(MSTWeekSummary1, MSTWeekSummary2, MSTWeekSummary3, MSTWeekSummary4, MSTWeekSummary5,MSTWeekSummary6, 
                       MSTWeekSummary7, MSTWeekSummary8, MSTWeekSummary9, MSTWeekSummary10, MSTWeekSummary11)

#Rename Columns
names(MSTWeekSummary)[1] = 'idPerson'
MSTWeekSummary$idPersonNum = as.numeric(substring(MSTWeekSummary$idPerson, first = 4))

#Delete duplicated old information
data.MST.week = MSTWeekSummary %>% filter(!(idPersonNum %in% c(159,193,194,204,205,206,207,211,213,222,228,231,233,237,244,247,252,254,257,258,260)))

#Express all measures in km or km2
# 1km = 1000 m
# 1km2 = 1000*1000 m2
data.MST.week$mstDistance = data.MST.week$mstDistance/1000

data.MST.week = data.MST.week %>% 
  select(idPerson, idPersonNum, idWeek, activityPoints, mstDistance)

#Delete variables
rm(MSTWeek)
rm(MSTWeekSummary1)
rm(MSTWeekSummary2)
rm(MSTWeekSummary3)
rm(MSTWeekSummary4)
rm(MSTWeekSummary5)
rm(MSTWeekSummary6)
rm(MSTWeekSummary7)
rm(MSTWeekSummary8)
rm(MSTWeekSummary9)
rm(MSTWeekSummary10)
rm(MSTWeekSummary11)
rm(MSTWeekSummary)
rm(fileName)
rm(fileRawData)
rm(file)

########################################################
########################################################
## Join all data
########################################################
########################################################

#Join SDE and MST data
data.TOTAL.week = data.SDE.week %>% 
  full_join(data.MST.week, by.x = 'idPerson', by.y = 'idweek') %>% 
  na.omit()

#Join total length of Trips and number of: Trips, Activity Points and Days per week
data.TOTAL.week = data.TOTAL.week %>%
  full_join(data.trips.stops, by.x = 'idPerson', by.y = 'idweek') %>% 
  na.omit()

#Column to identify yyyy-mm
data.TOTAL.week$idGroup1 = substring(data.TOTAL.week$idWeek, first = 1, last = 7)

#Column to identify COVID dates
data.TOTAL.week$idGroup2 = ifelse(data.TOTAL.week$idGroup1 < '2020-03', 'PreCovid', 'PostCovid')

data.TOTAL.week$levelRestrictions = 
  ifelse(data.TOTAL.week$idGroup1 < '2020-03', 0, #Before COVID
         ifelse(data.TOTAL.week$idGroup1 >= '2020-03' & data.TOTAL.week$idGroup1 <= '2020-05', 3, #Full restrictions
                ifelse(data.TOTAL.week$idGroup1 == '2020-06', 2, #Middle restrictions
                       ifelse(data.TOTAL.week$idGroup1 >= '2020-07' & data.TOTAL.week$idGroup1 <= '2020-12', 1, #Low restrictions
                              ifelse(data.TOTAL.week$idGroup1 >= '2021-01' & data.TOTAL.week$idGroup1 <= '2021-05', 2, #Middle restrictions
                                     1))))) #Low restrictions
                                     
names(data.TOTAL.week)[5] = 'areaSDE'
names(data.TOTAL.week)[6] = 'eccentricity'
names(data.TOTAL.week)[12] = 'distanceMST'
names(data.TOTAL.week)[13] = 'totalTrips'
names(data.TOTAL.week)[14] = 'totalPoints'
names(data.TOTAL.week)[18] = 'yearMonth'
names(data.TOTAL.week)[19] = 'covidStatus'

data.TOTAL.week$idPerson = as.factor(data.TOTAL.week$idPerson)
data.TOTAL.week$idWeek = as.factor(data.TOTAL.week$idWeek)
data.TOTAL.week$activityPoints = as.numeric(data.TOTAL.week$activityPoints)
data.TOTAL.week$totalPoints = as.numeric(data.TOTAL.week$totalPoints)
data.TOTAL.week$numDays = as.numeric(data.TOTAL.week$numDays)
data.TOTAL.week$yearMonth = as.factor(data.TOTAL.week$yearMonth)
data.TOTAL.week$covidStatus = as.factor(data.TOTAL.week$covidStatus)
data.TOTAL.week$levelRestrictions = as.factor(data.TOTAL.week$levelRestrictions)

summary(data.TOTAL.week)

#Joining TOTAL data with demographics
data.TOTAL.week.demographics = data.TOTAL.week %>%
  left_join(data.demographic) %>% 
  na.omit()  # 27597 28075 29028

summary(data.TOTAL.week.demographics)


#Persist Data
fileRawData = paste("dataReadTOTAL",trimws(str_replace_all(as.character(Sys.Date()),":","")),".RData",sep="")
urlSaveRawData = paste(urlRawData,fileRawData,sep="")
save(data.clusters, data.clusters.week, data.demographic, data.home, 
     data.TOTAL.week, data.TOTAL.week.demographics, file=urlSaveRawData)

fileExcelData = paste("data.TOTAL.week",trimws(str_replace_all(as.character(Sys.Date()),":","")),".csv",sep="")
urlSaveExcelData = paste(urlExcelData,fileExcelData,sep="")
write.csv(data.TOTAL.week,urlSaveExcelData)

fileExcelData = paste("data.TOTAL.week.demographics",trimws(str_replace_all(as.character(Sys.Date()),":","")),".csv",sep="")
urlSaveExcelData = paste(urlExcelData,fileExcelData,sep="")
write.csv(data.TOTAL.week.demographics,urlSaveExcelData)

fileExcelData = paste("data.clusters",trimws(str_replace_all(as.character(Sys.Date()),":","")),".csv",sep="")
urlSaveExcelData = paste(urlExcelData,fileExcelData,sep="")
write.csv(data.clusters,urlSaveExcelData)

fileExcelData = paste("data.clusters.week",trimws(str_replace_all(as.character(Sys.Date()),":","")),".csv",sep="")
urlSaveExcelData = paste(urlExcelData,fileExcelData,sep="")
write.csv(data.clusters.week,urlSaveExcelData)

fileExcelData = paste("data.demographic",trimws(str_replace_all(as.character(Sys.Date()),":","")),".csv",sep="")
urlSaveExcelData = paste(urlExcelData,fileExcelData,sep="")
write.csv(data.demographic,urlSaveExcelData)

fileExcelData = paste("data.home",trimws(str_replace_all(as.character(Sys.Date()),":","")),".csv",sep="")
urlSaveExcelData = paste(urlExcelData,fileExcelData,sep="")
write.csv(data.home,urlSaveExcelData)


#Delete variables
rm(data.trips.stops)
rm(data.SDE.week)
rm(data.MST.week)
rm(fileExcelData)
rm(fileRawData)
rm(urlSaveExcelData)
rm(urlSaveRawData)


summary(data.TOTAL.week)


#######################################################################################
#######################################################################################
## Resumen de cálculos estadísticos
#######################################################################################
#######################################################################################

#library(rstatix)

#stats.person.area = data.TOTAL.week %>%
#  group_by(idPerson) %>%
#  get_summary_stats(Area.sde, type = "common") %>%
#  na.omit()

#stats.person.mst = data.TOTAL.week %>%
#  group_by(idPerson) %>%
#  get_summary_stats(mstDistance, type = "common") %>%
#  na.omit()

#stats.week.area = data.TOTAL.week %>%
#  group_by(idWeek) %>%
#  get_summary_stats(Area.sde, type = "common") %>%
#  na.omit()

#stats.week.mst = data.TOTAL.week %>%
#  group_by(idWeek) %>%
#  get_summary_stats(mstDistance, type = "common") %>%
#  na.omit()
