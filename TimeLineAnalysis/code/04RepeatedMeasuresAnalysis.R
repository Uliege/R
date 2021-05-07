#Script para realizar ANOVA de las áreas que recorre cada persona durante las diferentes semanas

library(dplyr)
library(stringr)
library(ggplot2)


#Directorios (url) de trabajo del proyecto
urlDefault = getwd()
urlData = paste(urlDefault,"/data/",sep = "")
urlFigures = paste(urlDefault,"/figures/",sep = "")
urlModels = paste(urlDefault,"/models/",sep = "")
urlRawData = paste(urlDefault,"/rawdata/",sep = "")
urlReports = paste(urlDefault,"/reports/",sep = "")
urlOutput = paste(urlDefault,"/output/",sep = "")
urlExcelData = paste(urlDefault,"/exceldata/",sep = "")

#Archivo para almacenar el log de ejecución
fileLog = paste("log",trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
#Conexión para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(text = as.character(Sys.time()), con = con)

#Carga del archivo con los datos a procesar
#Change the name with the file generated in AspacePoints process 
fileName = 'aspaceSDEx2021-04-15'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,fileRawData,sep = ""))

##########################################################
#First Analysis - within-weeks person variability (ANOVA)
##########################################################

#Transformar a Factor los datos categóricos id, Y, M, W
xDataSDEatt$id = as.factor(xDataSDEatt$id)
xDataSDEatt$Y = as.factor(xDataSDEatt$Y)
xDataSDEatt$M = as.factor(xDataSDEatt$M)
xDataSDEatt$W = as.factor(xDataSDEatt$W)
#Aumentar columna para identificar las semanas
xDataSDEatt$W1 = as.factor(paste(xDataSDEatt$Y, xDataSDEatt$M, xDataSDEatt$W,sep = ''))


#Limpieza de personas con menos de 2 semanas de datos
xDataSDEatt = xDataSDEatt[xDataSDEatt$id != 'tld7',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$id != 'tld8',]

#Limpieza de semanas con menos de 2 personas con datos
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20131043',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20131044',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20131144',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '2013832',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '2013835',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '2013936',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '2013937',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '201411',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20191148',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20131040',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20131041',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20131042',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20131253',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '2013833',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '2013834',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '2013935',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '2013939',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20191248',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20191249',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20191250',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20191251',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '20191252',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '202011',]
xDataSDEatt = xDataSDEatt[xDataSDEatt$W1 != '202012',]

str(xDataSDEatt)

#Variables para almacenar los resultados
xANOVAPerson = NULL

idPersons = xDataSDEatt %>% 
  group_by(id) %>%
  count(id, sort = TRUE) %>%
  filter(n>2)

for(i in 1:nrow(idPersons)){
  #i=1
  xId = idPersons[i,1]
  
  dataPerson = xDataSDEatt %>%
    filter(id == xId$id)
  
  #Para el proceso, la persona al menos debe tener 2 semanas de datos
  if(nrow(dataPerson) > 1 ){
    fm1 = aov( lm(Area.sde ~ W, data = dataPerson) )
    valP = summary(fm1)[[1]][1,5]
    valF = summary(fm1)[[1]][1,4]
    if(!is.null(valF)){
      xANOVAPerson = rbind(xANOVAPerson,data.frame('id' =xId, 'numWeeks'=nrow(dataPerson), 'F'=valF, 'p'=valP))
      print(paste(i,'.............',nrow(dataPerson),sep=""))
    }
  }
}

#Cálculo general
fm1 = aov( lm(Area.sde ~ W, data = xDataSDEatt) )
summary(fm1)
fm1ValP = summary(fm1)[[1]][1,5]
fm1ValF = summary(fm1)[[1]][1,4]

#P < 0.05 -> Rechazar hipótesis nula, es decir, existe diferencia
#H0:Existe el mismo tamaño de actividad
#En este caso, p=0.11 > 0.05 - No se rechaza H0, no existe variabilidad, existe mismo tamaño de actividad  

#res = 0 -> 'Reject H0' 
#res = 1 -> 'Accept H0'

xANOVAPerson = xANOVAPerson %>%
  mutate(res = ifelse(p < 0.05, 'Reject H0', 'Accept H0'))

xANOVAPerson = xANOVAPerson %>%
  mutate(resBinary = ifelse(p < 0.05, 0, 1))

# Gráfico de los porcentajes de aceptación del ANOVA

ggplot(xANOVAPerson, aes(res)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.6) + 
  scale_y_continuous(labels=scales::percent)+
  xlab("H0 - Null Hypothesis") +
  ylab("Number of Participants")

xANOVAPerson$res = as.factor(xANOVAPerson$res)
summary(xANOVAPerson)

#Carga de datos demográficos
urlDefault = getwd()
urlData = paste(urlDefault,"/data/",sep = "")

dataDemFile = "dataDemographic.csv"
dataDemUrl = paste(urlData,dataDemFile,sep="")
  
xDataDemographic = read.csv(dataDemUrl)

summary(xDataDemographic)
str(xDataDemographic)
str(xANOVAPerson)

colnames(xDataDemographic)
names(xDataDemographic)[1] = 'id' 

xDataDemographic$id = as.factor(xDataDemographic$id)
xDataDemographic <- select(xDataDemographic, -res)

# Binary Logit Model

xDataLogitModel = merge(xANOVAPerson, xDataDemographic, by = "id")
names(xDataLogitModel)[6] = 'resANOVA' 
names(xDataLogitModel)[7] = 'UniversityStudent' 
names(xDataLogitModel)[12] = 'UsualTransportPattern' 

#Corregimos las edades con error
xDataLogitModel[xDataLogitModel$id == "tld130", "Age"] = 27
xDataLogitModel[xDataLogitModel$id == "tld75", "Age"] = 26
xDataLogitModel[xDataLogitModel$id == "tld110", "Age"] = 25
xDataLogitModel[xDataLogitModel$id == "tld144", "Age"] = 24
xDataLogitModel[xDataLogitModel$id == "tld156", "Age"] = 24
#Corregimos residencias con error
xDataLogitModel[xDataLogitModel$id == "tld97", "ResidenceLocation"] = 'Centre'
#Corregimos UsualTransportPattern con error
xDataLogitModel[xDataLogitModel$id == "tld131", "UsualTransportPattern"] = 'Public Transportation'
xDataLogitModel[xDataLogitModel$id == "tld50", "UsualTransportPattern"] = 'Public Transportation'
#Cambiamos valor de Public Transportation
xDataLogitModel[xDataLogitModel$UsualTransportPattern != "Public Transportation",]$UsualTransportPattern = 'Others'

summary(xDataLogitModel)

str(xDataLogitModel)
summary(xDataLogitModel)

xDataLogitModel$res = as.factor(xDataLogitModel$res)
xDataLogitModel$UniversityStudent = as.factor(xDataLogitModel$UniversityStudent)
xDataLogitModel$Gender = as.factor(xDataLogitModel$Gender)
xDataLogitModel$ResidenceLocation = as.factor(xDataLogitModel$ResidenceLocation)
xDataLogitModel$OwnVehicle = as.factor(xDataLogitModel$OwnVehicle)
xDataLogitModel$UsualTransportPattern = as.factor(xDataLogitModel$UsualTransportPattern)
summary(xDataLogitModel)

names(xDataLogitModel)

mylogit <- glm(resANOVA ~ UniversityStudent + Age + Gender + ResidenceLocation + OwnVehicle + UsualTransportPattern, 
               data = xDataLogitModel, 
               family = binomial)

summary(mylogit)

summary(mylogit)$coef

#Fin del script....
