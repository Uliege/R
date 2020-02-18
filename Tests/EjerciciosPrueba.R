hist(rnorm(100)) 

library(VIM)

dataCSV = read.csv("D:/tmp/DataDevice.csv")
dim(dataCSV)
print(dataCSV)
str(dataCSV)
hist(dataCSV$manufacturer)
print(dataCSV$manufacturer)
t <- table(dataCSV$manufacturer)
print(t)
barplot(t, xlab="Manufacturers", ylab="NumDevices", border="blue")

help(hist)
hist(t, breaks = 5)

summary(dataCSV)
aggr(dataCSV)


t1<-NULL

help(filter)
help(droplevels)
help(geom_bar)
help(factor)








library(rjson)
dataJSON = fromJSON(file = "D:/tmp/deviceG.json")
dataFrameJSON <- NULL
dataFrameJSON <- as.data.frame(dataJSON)
str(dataJSON)
str(dataFrameJSON)
t <- table(dataJSON[0])
dim(dataJSON)
print(dataJSON)
print(dataJSON[[300]]$sensor)
hist(t)
summary(dataJSON)
print(dataJSON[1])
print(dataJSON$dspId)
aggr(dataJSON)





#Rutina para leer el archivo JSON

library(jsonlite)
archivo <- "D:/tmp/deviceG.json"
con <- file(archivo, open="r")
ejemplo_json <- readLines(con)
close(con)
datosJSON2 <- fromJSON(ejemplo_json)
print(datosJSON2)
str(datosJSON2)


print(datosJSON2$dspId)
print(datosJSON2$fecha)
print(datosJSON2$sensor$dateInsert)
print(datosJSON2$sensor$dateUpdate)
t <- table(datosJSON2$sensor$dateInsert, datosJSON2$sensor$dateUpdate)
print(t)
print(datosJSON2$sensor$dateInsert + ' ' + datosJSON2$sensor$dateUpdate)




#Equipo con mayor recolección de datos

archivo <- "D:/tmp/device295.json"
con <- file(archivo, open="r")
ejemplo_json <- readLines(con)
close(con)
datos295 <- fromJSON(ejemplo_json)
print(datos295)
str(datos295)




archivo <- "D:/tmp/data8.json"
con <- file(archivo, open="r")
ejemplo_json <- readLines(con)
close(con)
datos8 <- fromJSON(ejemplo_json)


#Para hacer consultas a la base de datos MySQL

library(RMySQL)
driver=dbDriver("MySQL");
conexion = dbConnect(MySQL(),host="localhost",dbname="gmoncayo_smart_gps",user="root",pass="_Arsenal2011");
query = dbGetQuery(conexion,statement="SELECT * FROM tabla");
query















#################

library(jsonlite)
archivo <- "D:/tmp/device295.json"
con <- file(archivo, open="r")
ejemplo_json <- readLines(con)
close(con)
datos295 <- fromJSON(ejemplo_json)

typeof(datos295)
str(datos295)
names(datos295)
summary(datos295)

datos295a <- data.frame(datos295$sensor$numSatelites, datos295$sensor$velocidad, datos295$sensor$actividad, datos295$sensor$altitude, datos295$sensor$latitude, datos295$sensor$longitude)

latlong <- data.frame(satelites = datos295$sensor$numSatelites, lat = datos295$sensor$latitude, long = datos295$sensor$longitude)

summary(latlong)

library(VIM)
aggr(latlong, col = c("black", "green", "gray"), numbers = TRUE, combined = TRUE)
aggr(latlong, col = c("blue", "black", "gray"), only.miss = TRUE)



datos295$fecha[1]
datos295$sensor$dateInsert[746000]
as.character(datos295$sensor$dateInsert[746000])
typeof(datos295$sensor$dateInsert[746000])  
as.Date(datos295$sensor$dateInsert[746000])


factor(year(as.Date(datos295$fecha)))


















iris


#Análisis Exploratorio 

library(ggvis)



