library(dplyr)
library(jsonlite)
library(VIM)
library(lubridate)
library(ggmap)

print("Inicio!!")

print("Carga Datos")
load(file="/home/giovanny/SmartGPSAnalysis/rawdata/SGPS.RData")
print("OK Carga")

print("Type of DataSource")
typeof(dataSource)
print("Number of rows")
nrow(dataSource)
print("Number of columns")
ncol(dataSource)
print("Structure")
str(dataSource)
print("Summary")
summary(dataSource)

print("*****************************")

print("Type of DataClean")
typeof(dataClean)
print("Number of rows")
nrow(dataClean)
print("Number of columns")
ncol(dataClean)
print("Structure")
str(dataClean)
print("Summary")
summary(dataClean)

print("*****************************")

print("Type of Map")
typeof(mapQuito)

print("Script Finalizado")


