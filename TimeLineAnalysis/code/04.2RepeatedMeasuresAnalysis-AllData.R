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
fileLog = paste(urlLogs,"logANOVA",trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
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
fileName = 'ymwDataSDE2021-06-16'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,fileRawData,sep = ""))

#Backup initial data
ymwDataSDEattIni = ymwDataSDEatt

#To try again without loading
#ymwDataSDEatt = ymwDataSDEattIni

#Test with data from 2019 in advance
#ymwDataSDEatt$yearData = as.numeric(substring(ymwDataSDEatt$idGroup, first = 1, last = 4))
#ymwDataSDEatt = ymwDataSDEatt %>% filter(yearData >= 2019)

#Delete observations with negative value in the area calculation
ymwDataSDEatt = ymwDataSDEatt %>% filter(Area.sde > 0)

#Delete observations Eccentricity = 0 (Circle) or = 1 (Parabola)
ymwDataSDEatt$Eccentricity2 = signif(ymwDataSDEatt$Eccentricity,5)
ymwDataSDEatt = ymwDataSDEatt %>% filter(Eccentricity2 > 0)
ymwDataSDEatt = ymwDataSDEatt %>% filter(Eccentricity2 < 1)

#Delete observations with area < 1 m2
ymwDataSDEatt = ymwDataSDEatt %>%
  filter(Area.sde >= 1)

#Express area in square kilometers, and other in kilometers
# 1km = 1000 m
# 1km2 = 1000*1000 m2
#ymwDataSDEatt$Area.sde =  ymwDataSDEatt$Area.sde/1000000

#Add the distance and Major Sigma
ymwDataSDEatt$distance = sqrt(ymwDataSDEatt$Sigma.x^2 + ymwDataSDEatt$Sigma.y^2)
ymwDataSDEatt$majorSigma = ifelse(ymwDataSDEatt$Major == 'SigmaX', ymwDataSDEatt$Sigma.x, ymwDataSDEatt$Sigma.y)

#Select columns for the analysis
ymwDataSDEatt = select(ymwDataSDEatt, idFile, idGroup, weekYear, weekMonth, Area.sde, activityPoints, id, distance, majorSigma)

#Change name of the columns
names(ymwDataSDEatt)[1] = 'idPerson' 
names(ymwDataSDEatt)[5] = 'areaSDE'
names(ymwDataSDEatt)[7] = 'secEllipse'


###############################################
###############################################
#One way ANOVA for each person - Area of the ellipse
##############################################
##############################################
#Calculate variability for each person (One result for each one)
#GROUPS: Months of the year (at least 3)
#INDEPENDIENT SAMPLES: Week's area, grouped by month (at leat 3)

minGroups = 3
minSamplesPerGroup = 3
minAOV = minGroups*minSamplesPerGroup

ymwANOVA = ymwDataSDEatt

#Id of persons with SDE data
ymwIdPerson = ymwANOVA %>% 
  group_by(idPerson) %>%
  count(idPerson, sort = TRUE) %>%
  filter(n >= minAOV)

ymwIdPerson = ymwIdPerson %>% arrange(n)

#Variables for ANOVA Results
ymwANOVARes = NULL

#Loop to calculate ANOVA within-weeks (no unique) for each person
for(i in 1:nrow(ymwIdPerson)){
  #i=1
  ymwId = ymwIdPerson[i,1]$idPerson
  ymwEachPerson = ymwANOVA %>%
    filter(idPerson == ymwId)
  
  #Delete weeks with less than minSamplesPerGroup data
  ymwDeleteWeeks = ymwEachPerson %>% 
    group_by(idGroup) %>%
    count(idGroup, sort = TRUE) %>%
    filter(n < minSamplesPerGroup)
  
  ymwEachPerson = filter(ymwEachPerson, !(idGroup %in% ymwDeleteWeeks$idGroup))
  
  #Verify is the person has at least 3 Groups data
  ymwNumGroups = ymwEachPerson %>% 
    group_by(idGroup) %>%
    count(idGroup) %>%
    nrow()
  
  #Each Person need at least 3 groups with at least 3 elements, Total 9
  #if(nrow(ymwEachPerson) >= minAOV){
  if(ymwNumGroups >= minGroups){
    #People with more than 2 year data and more than 3 weeeks
    #aResANOVAPerson = aov( lm(area_sde ~ week, data = ymwEachPerson) )
    aResANOVAPerson = aov( lm(areaSDE ~ idGroup, data = ymwEachPerson) )
    valP = summary(aResANOVAPerson)[[1]][1,5]
    valF = summary(aResANOVAPerson)[[1]][1,4]
    if(!is.null(valF)){
      ymwANOVARes = rbind(ymwANOVARes, data.frame('idPerson' =ymwId, 'numWeeks'=nrow(ymwEachPerson), 'F'=valF, 'p'=valP, 'case'=1))
    }else{
      ymwANOVARes = rbind(ymwANOVARes, data.frame('idPerson' =ymwId, 'numWeeks'=nrow(ymwEachPerson), 'F'=-99, 'p'=-99, 'case'=2))
    }
  }else{
    ymwANOVARes = rbind(ymwANOVARes, data.frame('idPerson' =ymwId, 'numWeeks'=nrow(ymwEachPerson), 'F'=-99, 'p'=-99, 'case'=3))
  }
}

ymwANOVARes = ymwANOVARes %>%
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

ymwANOVARes = ymwANOVARes %>%
  mutate(textRes = ifelse(p < 0.05, 'Reject H0 *', 'Accept H0'))

ymwANOVARes = ymwANOVARes %>%
  mutate(binaryRes = ifelse(p < 0.05, 0, 1))

#Graphic with portion of Reject/Support H0
ggplot(ymwANOVARes, aes(textRes)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.6) + 
  scale_y_continuous(labels=scales::percent)+
  xlab("H0 - Null Hypothesis") +
  ylab("Number of Participants")

#Binary Logit Model
#Join ANOVA results and demographic information
aDataLogitModel = NULL
aDataLogitModel = merge(ymwANOVARes, aDataDemographic, by = "idPerson")

aDataLogitModel$university_student = as.factor(aDataLogitModel$university_student)
aDataLogitModel$gender = as.factor(aDataLogitModel$gender)
aDataLogitModel$residential_location = as.factor(aDataLogitModel$residential_location)
aDataLogitModel$own_vehicle = as.factor(aDataLogitModel$own_vehicle)
aDataLogitModel$usual_transport_pattern = as.factor(aDataLogitModel$usual_transport_pattern)

aLogitModel <- glm(binaryRes ~ university_student + age + gender + residential_location + own_vehicle + usual_transport_pattern, 
                   data = aDataLogitModel, 
                   family = binomial)

summary(aLogitModel)

summary(aLogitModel)$coef





###############################################
###############################################
#One way ANOVA for each person - majorSigma
##############################################
##############################################
#Calculate variability for each person (One result for each one)
#GROUPS: Months of the year (at least 3)
#INDEPENDIENT SAMPLES: Week's area, grouped by month (at leat 3)

minGroups = 3
minSamplesPerGroup = 3
minAOV = minGroups*minSamplesPerGroup

ymwANOVA = ymwDataSDEatt

#Id of persons with SDE data
ymwIdPerson = ymwANOVA %>% 
  group_by(idPerson) %>%
  count(idPerson, sort = TRUE) %>%
  filter(n >= minAOV)

ymwIdPerson = ymwIdPerson %>% arrange(n)

#Variables for ANOVA Results
ymwANOVARes1 = NULL

#Loop to calculate ANOVA within-weeks (no unique) for each person
for(i in 1:nrow(ymwIdPerson)){
  #i=1
  ymwId = ymwIdPerson[i,1]$idPerson
  ymwEachPerson = ymwANOVA %>%
    filter(idPerson == ymwId)
  
  #Delete weeks with less than minSamplesPerGroup data
  ymwDeleteWeeks = ymwEachPerson %>% 
    group_by(idGroup) %>%
    count(idGroup, sort = TRUE) %>%
    filter(n < minSamplesPerGroup)
  
  ymwEachPerson = filter(ymwEachPerson, !(idGroup %in% ymwDeleteWeeks$idGroup))
  
  #Verify is the person has at least 3 Groups data
  ymwNumGroups = ymwEachPerson %>% 
    group_by(idGroup) %>%
    count(idGroup) %>%
    nrow()
  
  #Each Person need at least 3 groups with at least 3 elements, Total 9
  #if(nrow(ymwEachPerson) >= minAOV){
  if(ymwNumGroups >= minGroups){
    #People with more than 2 year data and more than 3 weeeks
    #aResANOVAPerson = aov( lm(area_sde ~ week, data = ymwEachPerson) )
    aResANOVAPerson = aov( lm(majorSigma ~ idGroup, data = ymwEachPerson) )
    valP = summary(aResANOVAPerson)[[1]][1,5]
    valF = summary(aResANOVAPerson)[[1]][1,4]
    if(!is.null(valF)){
      ymwANOVARes1 = rbind(ymwANOVARes1, data.frame('idPerson' =ymwId, 'numWeeks'=nrow(ymwEachPerson), 'F'=valF, 'p'=valP, 'case'=1))
    }else{
      ymwANOVARes1 = rbind(ymwANOVARes1, data.frame('idPerson' =ymwId, 'numWeeks'=nrow(ymwEachPerson), 'F'=-99, 'p'=-99, 'case'=2))
    }
  }else{
    ymwANOVARes1 = rbind(ymwANOVARes1, data.frame('idPerson' =ymwId, 'numWeeks'=nrow(ymwEachPerson), 'F'=-99, 'p'=-99, 'case'=3))
  }
}

ymwANOVARes1 = ymwANOVARes1 %>%
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

ymwANOVARes1 = ymwANOVARes1 %>%
  mutate(textRes = ifelse(p < 0.05, 'Reject H0 *', 'Accept H0'))

ymwANOVARes1 = ymwANOVARes1 %>%
  mutate(binaryRes = ifelse(p < 0.05, 0, 1))

#Graphic with portion of Reject/Support H0
ggplot(ymwANOVARes1, aes(textRes)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.6) + 
  scale_y_continuous(labels=scales::percent)+
  xlab("H0 - Null Hypothesis") +
  ylab("Number of Participants")

#Binary Logit Model
#Join ANOVA results and demographic information
aDataLogitModel = NULL
aDataLogitModel = merge(ymwANOVARes1, aDataDemographic, by = "idPerson")

aDataLogitModel$university_student = as.factor(aDataLogitModel$university_student)
aDataLogitModel$gender = as.factor(aDataLogitModel$gender)
aDataLogitModel$residential_location = as.factor(aDataLogitModel$residential_location)
aDataLogitModel$own_vehicle = as.factor(aDataLogitModel$own_vehicle)
aDataLogitModel$usual_transport_pattern = as.factor(aDataLogitModel$usual_transport_pattern)

aLogitModel <- glm(binaryRes ~ university_student + age + gender + residential_location + own_vehicle + usual_transport_pattern, 
                   data = aDataLogitModel, 
                   family = binomial)

#En este resultado se muestra que existe diferencia significativa en la ubicación de la casa
summary(aLogitModel)

summary(aLogitModel)$coef



###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
