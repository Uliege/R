#Script para realizar ANOVA de las áreas que recorre cada persona durante las diferentes semanas


#Delete  work space variables (Caution!!)
rm(list=ls())
shell("cls")

library(dplyr)
library(stringr)
library(ggplot2)

#Directorios (url) de trabajo del proyecto
#urlProyecto = 'D:/G/GitHub/Uliege/R/TimeLineAnalysis/'
urlProyecto = getwd()
urlData = paste(urlProyecto,"/data/",sep = "")
urlFigures = paste(urlProyecto,"/figures/",sep = "")
urlModels = paste(urlProyecto,"/models/",sep = "")
urlRawData = paste(urlProyecto,"/rawdata/",sep = "")
urlReports = paste(urlProyecto,"/reports/",sep = "")
urlOutput = paste(urlProyecto,"/output/",sep = "")
urlExcelData = paste(urlProyecto,"/exceldata/",sep = "")
urlLogs = paste(urlProyecto,"/logs/",sep = "")

#Archivo para almacenar el log de ejecución
fileLog = paste(urlLogs,"logANOVA4.3",trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
#Conexión para escibir en el archivo
con <- file(fileLog, open="a")
#Hora de inicio del proceso
writeLines(text = as.character(Sys.time()), con = con)


####################################################
####################################################
##### Demographic Data 
####################################################
####################################################

#Load demographyc data
aDataDemographic = NULL

#First group
dataDemFile = "dataDemographic.csv"
dataDemUrl = paste(urlData,dataDemFile,sep="")
aDataDemographic1 = read.csv(dataDemUrl)
aDataDemographic1 = aDataDemographic1[-2]
names(aDataDemographic1)[1] = 'idPerson' 
names(aDataDemographic1)[2] = 'university_student'
names(aDataDemographic1)[3] = 'age'
names(aDataDemographic1)[4] = 'gender'
names(aDataDemographic1)[5] = 'residential_location'
names(aDataDemographic1)[6] = 'own_vehicle'
names(aDataDemographic1)[7] = 'usual_transport_pattern' 

#Second group
dataDemFile = "dataDemographic2.csv"
dataDemUrl = paste(urlData,dataDemFile,sep="")
aDataDemographic2 = read.csv(dataDemUrl)
aDataDemographic2 = aDataDemographic2[-2]
names(aDataDemographic2)[1] = 'idPerson' 
names(aDataDemographic2)[2] = 'university_student'
names(aDataDemographic2)[3] = 'age'
names(aDataDemographic2)[4] = 'gender'
names(aDataDemographic2)[5] = 'residential_location'
names(aDataDemographic2)[6] = 'own_vehicle'
names(aDataDemographic2)[7] = 'usual_transport_pattern' 

#Join 2 databases of demographic information
aDataDemographic = rbind(aDataDemographic1, aDataDemographic2)
rm(aDataDemographic1)
rm(aDataDemographic2)

#Delete repeated data 0 is the same than 161
aDataDemographic = aDataDemographic %>% filter(idPerson != 'tld0')

#Update Ages with error (Known Individuals)
aDataDemographic[aDataDemographic$idPerson == "tld75", "age"] = 26
aDataDemographic[aDataDemographic$idPerson == "tld110", "age"] = 25
aDataDemographic[aDataDemographic$idPerson == "tld130", "age"] = 27
aDataDemographic[aDataDemographic$idPerson == "tld144", "age"] = 24
aDataDemographic[aDataDemographic$idPerson == "tld156", "age"] = 24

#Update Residence Location with error (Known Individuals)
aDataDemographic[aDataDemographic$idPerson == "tld97", "residential_location"] = 'Centre'

#Unify UsualTRansportPattern (Public Transportation & Others)
aDataDemographic[aDataDemographic$usual_transport_pattern == "Public Transportation",]$usual_transport_pattern = 'Public Transportation & Others'
aDataDemographic[aDataDemographic$usual_transport_pattern == "On foot",]$usual_transport_pattern = 'Public Transportation & Others'

aDataDemographic$idPersonNum = as.numeric(substring(aDataDemographic$idPerson, first = 4))

aDataDemographic = select(aDataDemographic, idPerson, idPersonNum, university_student, age, gender, residential_location, own_vehicle, usual_transport_pattern)

aDataDemographic$idPerson = as.factor(aDataDemographic$idPerson)


####################################################
####################################################
##### Activity Points Data 
####################################################
####################################################


#Load Activity Points Data
#Change the name with the file generated in AspaceAnalysis process 
fileName = 'wdweDataSDE2021-06-18'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,fileRawData,sep = ""))

#Backup initial data
wdweDataSDEattIni = wdweDataSDEatt

#To try again without loading
#wdweDataSDEatt = wdweDataSDEattIni

#Test with data from 2019 in advance
wdweDataSDEatt$yearData = as.numeric(substring(wdweDataSDEatt$idGroup, first = 1, last = 4))
wdweDataSDEatt = wdweDataSDEatt %>% filter(yearData >= 2019)

#Delete observations with negative value in the area calculation
wdweDataSDEatt = wdweDataSDEatt %>% filter(Area.sde > 0)

#Delete observations with area < 0.1 m2
wdweDataSDEatt = wdweDataSDEatt %>%
  filter(Area.sde >= 0.1)

#Express area in square kilometers
# 1km = 1000 m
# 1km2 = 1000*1000 m2
wdweDataSDEatt$Area.sde =  wdweDataSDEatt$Area.sde/1000000

#Select columns for the analysis
wdweDataSDEatt = select(wdweDataSDEatt, idFile, idGroup, weekYear, weekMonth, typeWeek, Area.sde, activityPoints, id)

#Change name of the columns
names(wdweDataSDEatt)[1] = 'idPerson' 
names(wdweDataSDEatt)[6] = 'areaSDE'
names(wdweDataSDEatt)[8] = 'secEllipse'


###############################################
###############################################
#One way ANOVA for each person
##############################################
##############################################
#Calculate variability for each person (One result for each one)
#GROUPS: Months of the year (at least 3)
#INDEPENDIENT SAMPLES: Week's area, grouped by month (at leat 3)

minGroups = 3
minSamplesPerGroup = 3
minAOV = minGroups*minSamplesPerGroup

wdweANOVA = wdweDataSDEatt

#Id of persons with SDE data
wdweIdPerson = wdweANOVA %>% 
  group_by(idPerson) %>%
  count(idPerson, sort = TRUE) %>%
  filter(n >= minAOV)

wdweIdPerson = wdweIdPerson %>% arrange(n)

#Variables for ANOVA Results
wdweANOVARes = NULL

#Loop to calculate ANOVA within-weeks (no unique) for each person
for(i in 1:nrow(wdweIdPerson)){
  #i=1
  wdweId = wdweIdPerson[i,1]$idPerson
  wdweEachPerson = wdweANOVA %>%
    filter(idPerson == wdweId)
  
  #Delete weeks with less than minSamplesPerGroup data
  wdweDeleteWeeks = wdweEachPerson %>% 
    group_by(idGroup) %>%
    count(idGroup, sort = TRUE) %>%
    filter(n < minSamplesPerGroup)
  
  wdweEachPerson = filter(wdweEachPerson, !(idGroup %in% wdweDeleteWeeks$idGroup))
  
  #Verify is the person has at least 3 Groups data
  wdweNumGroups = wdweEachPerson %>% 
    group_by(idGroup) %>%
    count(idGroup) %>%
    nrow()
  
  #Each Person need at least 3 groups with at least 3 elements, Total 9
  #if(nrow(wdweEachPerson) >= minAOV){
  if(wdweNumGroups >= minGroups){
    #People with more than 2 year data and more than 3 weeeks
    #aResANOVAPerson = aov( lm(area_sde ~ week, data = wdweEachPerson) )
    aResANOVAPerson = aov( lm(areaSDE ~ idGroup, data = wdweEachPerson) )
    valP = summary(aResANOVAPerson)[[1]][1,5]
    valF = summary(aResANOVAPerson)[[1]][1,4]
    if(!is.null(valF)){
      wdweANOVARes = rbind(wdweANOVARes, data.frame('idPerson' =wdweId, 'numWeeks'=nrow(wdweEachPerson), 'F'=valF, 'p'=valP, 'case'=1))
    }else{
      wdweANOVARes = rbind(wdweANOVARes, data.frame('idPerson' =wdweId, 'numWeeks'=nrow(wdweEachPerson), 'F'=-99, 'p'=-99, 'case'=2))
    }
  }else{
    wdweANOVARes = rbind(wdweANOVARes, data.frame('idPerson' =wdweId, 'numWeeks'=nrow(wdweEachPerson), 'F'=-99, 'p'=-99, 'case'=3))
  }
}

wdweANOVARes = wdweANOVARes %>%
  filter(case==1)




##########################################################
##########################################################
# Binary Logit Model
##########################################################
##########################################################

#p < 0.05 -> Reject Null Hypotesis, it means, difference exists
#H0: Space Activity is the same (equal) within-weeks
#H1: Space Activity is different

#binaryRes = 0 -> 'Reject H0 *' (p value <0.05) - Activity Space is different
#binaryRes = 1 -> 'Accept H0'

wdweANOVARes = wdweANOVARes %>%
  mutate(textRes = ifelse(p < 0.05, 'Reject H0 *', 'Accept H0'))

wdweANOVARes = wdweANOVARes %>%
  mutate(binaryRes = ifelse(p < 0.05, 0, 1))

#Graphic with portion of Reject/Support H0
ggplot(wdweANOVARes, aes(textRes)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.6) + 
  scale_y_continuous(labels=scales::percent)+
  xlab("H0 - Null Hypothesis") +
  ylab("Number of Participants")

#Binary Logit Model
#Join ANOVA results and demographic information
aDataLogitModel = NULL
aDataLogitModel = merge(wdweANOVARes, aDataDemographic, by = "idPerson")

aDataLogitModel$university_student = as.factor(aDataLogitModel$university_student)
aDataLogitModel$gender = as.factor(aDataLogitModel$gender)
aDataLogitModel$residential_location = as.factor(aDataLogitModel$residential_location)
aDataLogitModel$own_vehicle = as.factor(aDataLogitModel$own_vehicle)
aDataLogitModel$usual_transport_pattern = as.factor(aDataLogitModel$usual_transport_pattern)

aLogitModel <- glm(binaryRes ~ university_student + age + gender + residential_location + own_vehicle + usual_transport_pattern, 
                   data = aDataLogitModel, 
                   family = binomial)

#En este resultado se muestra que no existe diferencia significativa
summary(aLogitModel)

summary(aLogitModel)$coef


###############################################################
###############################################################
#### ANÁLISIS DE LAS DISTRIBUCIONES DE ÁREA, DISTANCIAS, SIGMAS
################################################################
###############################################################

aa = hist(wdweDataSDEatt$areaSDE, breaks = 1000)
plot(aa$count, log="xy", type='points')


aaa = hist(wdweDataSDEattIni$Sigma.x, breaks = 1000)
plot(aaa)


aaaa = hist(wdweDataSDEattIni$Sigma.y, breaks = 1000)
plot(aaaa)


aaaaa = hist(wdweDataSDEattIni$Eccentricity, breaks = 1000)
plot(aaaaa)


wdweDataSDEattIni$distance = sqrt(wdweDataSDEattIni$Sigma.x*wdweDataSDEattIni$Sigma.x + wdweDataSDEattIni$Sigma.y*wdweDataSDEattIni$Sigma.y)

hist(wdweDataSDEattIni$distance, breaks = 1000)

summary(wdweDataSDEatt)











