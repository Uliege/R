install.packages("rlang")
# install dev version of ggmap
devtools::install_github("dkahle/ggmap")

library(dplyr)
library(jsonlite)
library(VIM)
library(lubridate)
library(ggmap)


readJsonFile <- function(fileName) {
  dataResult <- NULL
  archivo1 <- paste("D:/tmp/",fileName, sep = "")
  con <- file(archivo1, open="r")
  dataJson1 <- readLines(con)
  close(con)
  data1 <- fromJSON(dataJson1)
  
  dspId <- data1$dspId
  dspLat <- data1$sensor$latitude
  dspLong <- data1$sensor$longitude
  dspNumSat <- data1$sensor$numSatelites
  dspAlti <- data1$sensor$altitude
  dspVelo <- data1$sensor$velocidad
  dspActy <- data1$sensor$actividad
  dspAccu <- data1$sensor$precision
  dspFecIn <- data1$sensor$dateInsert
  dspFecUp <- data1$sensor$dateUpdate
  dspProvStat <- data1$sensor$providerStatus
  
  data1 <- data.frame(dspId, dspLat, dspLong, dspNumSat, dspAlti, dspVelo, dspActy, dspAccu, dspFecIn, dspFecUp, dspProvStat)
  
  return(data1)
}

archivo1 <- "D:/tmp/device295.json"
archivo2 <- "D:/tmp/device247.json"
archivo3 <- "D:/tmp/deviceV.json"

con <- file(archivo1, open="r")
dataJson1 <- readLines(con)
close(con)
data1 <- fromJSON(dataJson1)

print("Loading 2")
con <- file(archivo2, open="r")
dataJson2 <- readLines(con)
close(con)
data2 <- fromJSON(dataJson2)

dspId <- data2$dspId
dspLat <- data2$sensor$latitude
dspLong <- data2$sensor$longitude
dspNumSat <- data2$sensor$numSatelites
dspAlti <- data2$sensor$altitude
dspVelo <- data2$sensor$velocidad
dspActy <- data2$sensor$actividad
dspAccu <- data2$sensor$precision
dspFecIn <- data2$sensor$dateInsert
dspFecUp <- data2$sensor$dateInsert
dspProvStat <- data2$sensor$providerStatus

data2F <- data.frame(dspId, dspLat, dspLong, dspNumSat, dspAlti, dspVelo, dspActy, dspAccu, dspFecIn, dspFecUp, dspProvStat)


print("Loading 3")
con <- file(archivo3, open="r")
dataJson3 <- readLines(con)
close(con)
data3 <- fromJSON(dataJson3)

dspId <- data3$dspId
dspLat <- data3$sensor$latitude
dspLong <- data3$sensor$longitude
dspNumSat <- data3$sensor$numSatelites
dspAlti <- data3$sensor$altitude
dspVelo <- data3$sensor$velocidad
dspActy <- data3$sensor$actividad
dspAccu <- data3$sensor$precision
dspFecIn <- data3$sensor$dateInsert
dspFecUp <- data3$sensor$dateInsert
dspProvStat <- data3$sensor$providerStatus

data3F <- data.frame(dspId, dspLat, dspLong, dspNumSat, dspAlti, dspVelo, dspActy, dspAccu, dspFecIn, dspFecUp, dspProvStat)


typeof(data2)
typeof(data3F)

dataSource <- bind_rows(data2F, data3F)






typeof(dataJson3)

dataJsonss <- paste(dataJson1,dataJson2,dataJson3)
dataSource <- fromJSON(dataJsonss)

typeof(data1)

dim(data1)[1] #Number of rows
dim(data1)[2] #Number of columns

dim(data2)[1] #Number of rows
dim(data2)[2] #Number of columns

dim(data3)[1] #Number of rows
dim(data3)[2] #Number of columns


nrow(data3)
ncol(data3)

m <- matrix(1:20, nrow=4, byrow=T)
print(m)
m

m - diag(nrow=4, ncol=5)



x <- 1:10
y <- c("abc", "d", "ef", "g")
ls <- list(x, y) %>% print()
ls

ls[[2]]
ls[[2]][1]


dataSource <- data1 + data2 + data3

dataSource <- Reduce(merge, list(data2, data3))

dataSource <- bind_rows(list(data1, data2))

library(gtools)
dataSource <- smartbind(data2, data3)

dataSource <- NULL

dataSource <- rbind(setDT(data2), setDT(data3), fill=TRUE)


typeof(datos295)
str(datos295)
names(datos295)
summary(datos295)






fec <- datos295$sensor$dateInsert
lat <- datos295$sensor$latitude
long <- datos295$sensor$longitude
vel <- datos295$sensor$velocidad
act <- datos295$sensor$actividad

dataGPS <- data.frame(fec, lat, long, vel, act)

summary(dataGPS)
str(dataGPS)

#Missing Data
summary(aggr(dataGPS, col = c("yellow", "green", "gray"), only.miss = TRUE))

#Number of NAs
length(which(is.na(dataGPS)))

#Number of rows with NAs
length(which(!complete.cases(dataGPS)))

#Number of rows without NAs
length(which(complete.cases(dataGPS)))

#Eliminar filas con valores Nulos
dataGPS <- na.omit(dataGPS)

summary(aggr(dataGPS, col = c("yellow", "green", "gray"), only.miss = TRUE))


# Separate or mutate the Date/Time columns


dataGPS$year <- factor(year(dataGPS$fec))
dataGPS$month <- factor(month(dataGPS$fec))
dataGPS$dayy <- factor(day(dataGPS$fec))
dataGPS$weekDay <- factor(wday(dataGPS$fec))
dataGPS$hourr <- factor(hour(dataGPS$fec))
dataGPS$min <- factor(minute(dataGPS$fec))
dataGPS$sec <- factor(second(dataGPS$fec))

dataGPS[,2:3]

#K-Means Clustering with R
set.seed(20)
clusters <- kmeans(dataGPS[,2:3], 5)

#Probar K-means with 4 clusters
clusters <- kmeans(dataGPS[,2:3], 4)

# Save the cluster number in the dataset as column 'Cluster'
dataGPS$cluster <- as.factor(clusters$cluster)

hist(clusters$cluster)


#Draw the Map
register_google(key = "AIzaSyDM3eTYsoIVaAdZJPMyEy9xO-pqXKryVjE")

QuitoMap <- get_map("Quito")
QuitoMap3 <- get_map("Quito", zoom = 3)
QuitoMap10 <- get_map("Quito", zoom = 10)
QuitoMap12 <- get_map("Quito", zoom = 12)
QuitoMap17 <- get_map("Quito", zoom = 17)
QuitoMap21 <- get_map("Quito", zoom = 21)

QuitoMapCenter17 <- get_map(location = c(lat = -0.183446, long= -78.418674), zoom = 17)

QuitoMapCenter10 <- get_map(location = c(lat = -0.183446, long= -78.418674), zoom = 10)

##Ok
QuitoMapCenter11 <- get_map(location = c(lat = -0.183446, long= -78.418674), zoom = 11)

##Ok
QuitoMapCenter12 <- get_map(location = c(lat = -0.223446, long= -78.458674), zoom = 12)

QuitoMapCenter13 <- get_map(location = c(lat = -0.183446, long= -78.418674), zoom = 13)

ggmap(QuitoMapCenter13)
ggmap(QuitoMapCenter12)
ggmap(QuitoMapCenter11)


ggmap(QuitoMap) + geom_point(aes(x = long[], y = lat[], colour = as.factor(cluster)),data = dataGPS) +
  ggtitle("Quito using KMean")

ggmap(QuitoMap3) + geom_point(aes(x = long[], y = lat[], colour = as.factor(cluster)),data = dataGPS) +
  ggtitle("Quito using KMean")

ggmap(QuitoMap10) + geom_point(aes(x = long[], y = lat[], colour = as.factor(cluster)),data = dataGPS) +
  ggtitle("Quito using KMean")


#Mapa con Clusters
ggmap(QuitoMap12) + geom_point(aes(x = long[], y = lat[], colour = as.factor(cluster)),data = dataGPS) +
  ggtitle("Quito using KMean")

ggmap(QuitoMapCenter11) + geom_point(aes(x = long[], y = lat[], colour = as.factor(cluster)),data = dataGPS) +
  ggtitle("Quito 11 using 4 KMean")

ggmap(QuitoMapCenter12) + geom_point(aes(x = long[], y = lat[], colour = as.factor(cluster)),data = dataGPS) +
  ggtitle("Quito using KMean")

#Mapa con Puntos
ggmap(QuitoMapCenter11)+ geom_point(aes(x = long[], y = lat[]), 
                                    data = dataGPS, 
                                    alpha = .5, 
                                    color="darkred", 
                                    size = .5)

dataGPS6a8 = subset(dataGPS, hourr==6 | hourr==7 | hourr==8)

ggmap(QuitoMapCenter11)+ geom_point(aes(x = long[], y = lat[]), 
                                    data = dataGPS6a8, 
                                    alpha = .5, 
                                    color="darkred", 
                                    size = .5)

dataGPS9a12 = subset(dataGPS, hourr==9 | hourr==10 | hourr==11 | hourr==12)

ggmap(QuitoMapCenter11)+ geom_point(aes(x = long[], y = lat[]), 
                                    data = dataGPS9a12, 
                                    alpha = .5, 
                                    color="green", 
                                    size = .5)

dataGPS9 = subset(dataGPS, hourr==9)

ggmap(QuitoMapCenter11)+ geom_point(aes(x = long[], y = lat[]), 
                                    data = dataGPS9, 
                                    alpha = .5, 
                                    color="blue", 
                                    size = .5)

dataGPS14 = subset(dataGPS, hourr==14)

ggmap(QuitoMapCenter11)+ geom_point(aes(x = long[], y = lat[]), 
                                    data = dataGPS14, 
                                    alpha = .5, 
                                    color="pink", 
                                    size = .5)



##Densidades
QMap11 <- ggmap(QuitoMapCenter11, extent = 'device', legend = 'topleft')

ggmap(QuitoMapCenter11) + 
  stat_density2d(aes(x = long[], y = lat[],
                     fill = ..level.. , 
                     alpha = ..level..),
                     size = 2, 
                     bins = 4,
                     data = dataGPS,
                     color="yellow",
                     geom = 'polygon') +
  scale_fill_gradient('Density') +
  scale_alpha(range = c(.4, .75), guide = FALSE) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))







base_plot <- ggplot(dataGPS, aes(x = long[], y = lat[])) + 
  geom_point()

base_plot + 
  stat_density2d(aes(color = ..level..))

base_plot + 
  stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE)

base_plot +
  stat_density2d(aes(alpha = ..density..), geom = "tile", contour = FALSE)


ggmap(QuitoMapCenter11) + 
  geom_point(aes(x = long[], y = lat[]),
             data = dataGPS, 
             alpha = .5, 
             color="darkred", 
             size = .5) +
  stat_density2d(aes(color = ..level..))





ggmap(QuitoMap12)

ggmap(QuitoMap21)


UCEMap <- get_map(location = c(lat = -0.19915, lon = -78.50341), zoom = 17)

ggmap(UCEMap)


#Para exportar el Mapa a un archivo
png("D:/tmp/UCEMap.png",width=1200,height=820,units="px",pointsize=12,bg="white",res=300)
ggmap(UCEMap)
dev.off()



summary(dataGPS)

subset(dataGPS, dataGPS$cluster == 3)

typeof(dataGPS$cluster)


##Histogramas

hist(as.numeric(dataGPS$cluster))
hist(as.numeric(dataGPS$vel))
hist(as.numeric(dataGPS$vel), freq = FALSE,  col = "gray", labels = TRUE)

hist(as.numeric(dataGPS$act))
hist(dataGPS$act, freq = FALSE,  col = "yellow", labels = TRUE, probability = TRUE)


##Diagramas de Caja

boxplot(as.numeric(dataGPS$vel), ylim=c(0,10))
boxplot(as.numeric(dataGPS$lat), ylim=c(-0.5,0.2))
boxplot(as.numeric(dataGPS$act))

boxplot(as.numeric(dataGPS$cluster))



#We can also do statistical graphics - like a boxplot showing the distribution of speed values by gear
g = ggplot(df) + geom_boxplot(aes(factor(NGear),vCar))
print(g)

g = ggplot(dataGPS) + 
  geom_boxplot(aes(x=factor(hourr),y=act))

print(g)


g = ggplot(subset(dataGPS, vel<28)) + 
  geom_jitter(aes(factor(hourr),act),colour='darkgreen') + 
  geom_jitter(aes(factor(hourr),vel),colour='darkred')

g = ggplot(subset(dataGPS, vel<28)) + 
  geom_jitter(aes(factor(hourr),vel)) + 
  scale_colour_gradient(low='red',high='green')

g = ggplot(dataGPS) + 
  geom_jitter(aes(factor(hourr),act),colour='darkgreen')

print(g)


subset(dataGPS, vel>50)








require(stats)
set.seed(14)
x <- rchisq(100, df = 4)

## Comparing data with a model distribution should be done with qqplot()!
qqplot(x, qchisq(ppoints(x), df = 4)); abline(0, 1, col = 2, lty = 2)

## if you really insist on using hist() ... :
hist(x, freq = FALSE, ylim = c(0, 0.2))
curve(dchisq(x, df = 4), col = 2, lty = 2, lwd = 2, add = TRUE)







#Exportar subconjuntos a CSV
dataTmp <- subset(dataGPS, dataGPS$dayy == 24 & dataGPS$month == 1)
write.csv(dataTmp, file = "D:/tmp/exp.csv")







ggplot(subset(dataGPS, dataGPS$cluster != 2)) + 
  geom_point(aes(x=lat,y=long,col=sign(vel),size=abs(vel))) + 
  coord_map(project="mercator")

#g = ggplot(subset(dataGPS, dataGPS$dayy == 21)) +
g = ggplot(dataGPS) +
  geom_line(aes(x=dataGPS$hourr,y=vel,colour=act))

print(g)

g = ggplot(dataGPS) + 
  geom_boxplot(aes(x=act,y=vel,group=act))

g = ggplot(subset(dataGPS,dataGPS$act==2)) + 
  geom_boxplot(aes(x=act,y=vel,group=act))

print(g)


subset(dataGPS,act==0 & vel>15)


g = ggplot(dataGPS) + 
  geom_jitter(aes(factor(act),vel,col=vel)) + 
  scale_colour_gradient(low='red',high='green')
print(g)


hist(dataGPS$vel)

library(magrittr)
dataGPS$vel %>% density() %>% plot(main="aaaa")




install.packages("sqldf")
library(sqldf)

sqldf("SELECT * FROM dataClean where dspActy == 2")

dataActivity <- sqldf("SELECT 
                      case when dspActy==0 then 'IN_VEHICLE' 
                      when dspActy==1 then '01 ON_BICYCLE' 
                      when dspActy==2 then '02 ON_FOOT'
                      when dspActy==3 then '03 STILL'
                      when dspActy==4 then '04 UNKNOWN'
                      when dspActy==5 then '05 TILTING'
                      when dspActy==7 then '07 WALKING'
                      when dspActy==8 then '08 RUNNING'
                      end as qActy, 
                      case when dspWeekDay==1 then '1\nMON'
                      when dspWeekDay==2 then '2\nTUESDAY'
                      when dspWeekDay==3 then '3\nWEDNESDAY'
                      when dspWeekDay==4 then '4\nTHURSDAY'
                      when dspWeekDay==5 then '5\nFRIDAY'
                      when dspWeekDay==6 then '6\nSATURDAY'
                      when dspWeekDay==7 then '7\nSUNDAY'
                      end as qWeekDay, 
                      CAST(dspHour AS SIGNED) as qHour, 
                      dspVeloKmH as qVeloKmH 
                      FROM dataClean")
str(dataActivity)

ggplot(dataActivity,aes(x=qWeekDay, fill=qActy))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Activity Distribution Per Day") + 
  xlab("Week Day") +
  ylab("count") +
  guides(fill=guide_legend(title="Activity")) 
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/1-ActivityDay.png")



ggplot(dataActivity,aes(x=qHour, fill = qActy))+ 
  geom_bar(alpha=0.6)+
  ggtitle("Activity Distribution Per Hour") + 
  xlab("Hour") +
  ylab("count") +
  guides(fill=guide_legend(title="Activity")) 
  ggsave(filename = "/home/giovanny/SmartGPSAnalysis/figures/2-ActivityHour.png")

  
  
  
    
  
  