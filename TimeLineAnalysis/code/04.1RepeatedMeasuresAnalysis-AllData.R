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
fileName = 'tDataSDE2021-06-09'
fileRawData = paste(fileName,".RData",sep="")
load(file=paste(urlRawData,fileRawData,sep = ""))

aDataSDEatt = tDataSDEatt

#Select columns for the analysis
aDataSDEatt = select(aDataSDEatt, idFile, id, year, week, Area.sde, numPoints)

#Change name of the columns
names(aDataSDEatt)[1] = 'idPerson' 
names(aDataSDEatt)[2] = 'idWeek'
names(aDataSDEatt)[5] = 'area_sde'
names(aDataSDEatt)[6] = 'num_points'

#Delete idPerson with less than 3 week data
deletePerson = aDataSDEatt %>% 
  group_by(idPerson) %>%
  count(idPerson, sort = TRUE) %>%
  filter(n<3)

for(i in 1:nrow(deletePerson)){
  xDel = as.character(deletePerson[i,1])
  aDataSDEatt = aDataSDEatt %>%
    filter(idPerson != xDel)
}

rm(deletePerson)

#Delete idWeek with less than 3 person data
deleteWeek = aDataSDEatt %>% 
  group_by(idWeek) %>%
  count(idWeek, sort = TRUE) %>%
  filter(n<3)

for(i in 1:nrow(deleteWeek)){
  xDel = as.character(deleteWeek[i,1])
  aDataSDEatt = aDataSDEatt %>%
    filter(idWeek != xDel)
}

rm(deleteWeek)

aDataSDEatt = aDataSDEatt[order(aDataSDEatt$idPerson, aDataSDEatt$year, aDataSDEatt$week),]

#Add numerical columns

aDataSDEatt$idPersonNum = as.numeric(substring(aDataSDEatt$idPerson, first = 4))
aDataSDEatt$idWeekNum = as.numeric(aDataSDEatt$idWeek)

aDataSDEatt = select(aDataSDEatt, idPerson, idPersonNum, idWeek, idWeekNum, year, week, area_sde, num_points)

#write.csv(aDataSDEatt ,paste(urlDownloads,'dataSDE.csv', sep = ''))

#Transform to Factor categorical data idPersona, idWeek
aDataSDEatt$idPerson = as.factor(aDataSDEatt$idPerson)
aDataSDEatt$idWeek = as.factor(aDataSDEatt$idWeek)



##########################################################
##########################################################
#First Analysis - within-weeks person variability (ANOVA)
##########################################################
##########################################################

#Variables for ANOVA Analysis
aANOVASDEatt = aDataSDEatt
aANOVADemographic = aDataDemographic

#Filter data with SDE and Demographic information
aANOVASDEatt = filter(aANOVASDEatt, idPerson %in% aANOVADemographic$idPerson)
aANOVADemographic = filter(aANOVADemographic, idPerson %in% aANOVASDEatt$idPerson)

aANOVASDEatt$year = as.factor(aANOVASDEatt$year)
aANOVASDEatt$week = as.factor(aANOVASDEatt$week)

#select columns for the analysis

aANOVASDEatt = aANOVASDEatt %>% select(idPerson, year, idWeek, week, area_sde)

#Area expresed in square kilometres
aANOVASDEatt$area_sde = aANOVASDEatt$area_sde/1000000


#Id of persons with SDE data
aIdPerson = aANOVASDEatt %>% 
  group_by(idPerson) %>%
  count(idPerson, sort = TRUE)

aIdPerson = aIdPerson %>% arrange(desc(n))

#Variables for ANOVA Results
aANOVARes = NULL

#Loop to calculate ANOVA within-weeks (no unique) for each person
for(i in 1:nrow(aIdPerson)){
  #i=85
  aId = aIdPerson[i,1]$idPerson
  aEachPerson = aANOVASDEatt %>%
    filter(idPerson == aId)
  
  if(length(unique(aEachPerson$year)) > 2){
    #People with more than 2 year data and more than 3 weeeks
    #aResANOVAPerson = aov( lm(area_sde ~ week, data = aEachPerson) )
    aResANOVAPerson = aov( lm(area_sde ~ year + week, data = aEachPerson) )
    valP = summary(aResANOVAPerson)[[1]][1,5]
    valF = summary(aResANOVAPerson)[[1]][1,4]
    if(!is.null(valF)){
      aANOVARes = rbind(aANOVARes, data.frame('idPerson' =aId, 'numWeeks'=nrow(aEachPerson), 'F'=valF, 'p'=valP, 'case'=1))
      #print(aId)
    }else{
      #Variability for non group data
      #aVar = var(aEachPerson$area_sde)
      #print(aId)
      #People with more than 2 year data not so much weeeks
      aResANOVAPerson = aov( lm(area_sde ~ week, data = aEachPerson) )
      #aResANOVAPerson = aov( lm(area_sde ~ year + week, data = aEachPerson) )
      valP = summary(aResANOVAPerson)[[1]][1,5]
      valF = summary(aResANOVAPerson)[[1]][1,4]
      if(!is.null(valF)){
        aANOVARes = rbind(aANOVARes, data.frame('idPerson' =aId, 'numWeeks'=nrow(aEachPerson), 'F'=valF, 'p'=valP, 'case'=2))
        #print(aId)
      }
    }
  }else{
    #People with more 2 year data and some weeeks
    aResANOVAPerson = aov( lm(area_sde ~ week, data = aEachPerson) )
    #aResANOVAPerson = aov( lm(area_sde ~ year + week, data = aEachPerson) )
    valP = summary(aResANOVAPerson)[[1]][1,5]
    valF = summary(aResANOVAPerson)[[1]][1,4]
    if(!is.null(valF)){
      aANOVARes = rbind(aANOVARes, data.frame('idPerson' =aId, 'numWeeks'=nrow(aEachPerson), 'F'=valF, 'p'=valP, 'case'=3))
      #print(aId)
    }else{
      #Have only one year data (NO Groups)
      #Variability for non group data
      #aVar = var(aEachPerson$area_sde)
      #print(aId)
      
      #People with 2 year data and some weeeks - can't
      aANOVARes = rbind(aANOVARes, data.frame('idPerson' =aId, 'numWeeks'=nrow(aEachPerson), 'F'=1000, 'p'=1000, 'case'=4))
    }
  }
}
rm(aEachPerson)
rm(aResANOVAPerson)
print('End... ANOVA within-weeks person (no unique)')
print(paste(nrow(aANOVARes), 'Persons with gruop data, from', nrow(aIdPerson)))
print(paste(nrow(aIdPerson)-nrow(aANOVARes), 'Has no group data....'))

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

aANOVARes = aANOVARes %>%
  mutate(textRes = ifelse(p < 0.05, 'Reject H0 *', 'Accept H0'))

aANOVARes = aANOVARes %>%
  mutate(binaryRes = ifelse(p < 0.05, 0, 1))

#Graphic with portion of Reject/Support H0
ggplot(aANOVARes, aes(textRes)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha=0.6) + 
  scale_y_continuous(labels=scales::percent)+
  xlab("H0 - Null Hypothesis") +
  ylab("Number of Participants")

#Binary Logit Model
#Join ANOVA results and demographic information
aDataLogitModel = merge(aANOVARes, aANOVADemographic, by = "idPerson")

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

#Fit the model

#library(tidyverse)
#library(caret)
#library(leaps)
#library(MASS)
#aLogitModel.fit <- stepAIC(aLogitModel, direction = "both", trace = FALSE)
#summary(aLogitModel.fit)

#Fin del script....



############################################################################
############################################################################
#Total computation ANOVA
############################################################################
############################################################################

aLogitModelWeek = aov( lm(area_sde ~ year + week, data = aANOVASDEatt) )
summary(aLogitModelWeek)
valP_area_week = summary(aLogitModelWeek)[[1]][1,5]
valF_area_week = summary(aLogitModelWeek)[[1]][1,4]
print(paste('p = ', valP_area_week))
print(paste('F = ', valF_area_week))
print('SIGNIFICATIVO')

aLogitModelWeek = aov( lm(area_sde ~ week, data = aANOVASDEatt) )
summary(aLogitModelWeek)
valP_area_week = summary(aLogitModelWeek)[[1]][1,5]
valF_area_week = summary(aLogitModelWeek)[[1]][1,4]
print(paste('p = ', valP_area_week))
print(paste('F = ', valF_area_week))
print('NO significativo')

aLogitModelWeek = aov( lm(area_sde ~ idWeek, data = aANOVASDEatt) )
summary(aLogitModelWeek)
valP_area_week = summary(aLogitModelWeek)[[1]][1,5]
valF_area_week = summary(aLogitModelWeek)[[1]][1,4]
print(paste('p = ', valP_area_week))
print(paste('F = ', valF_area_week))
print('NO significativo')

############################################################################
############################################################################
#Third Analysis - within-person variability (ANOVA)
#idPerson considered as unique value (not possible to calculate per each week)
############################################################################
############################################################################

aLogitModelPerson = aov( lm(area_sde ~ idPerson, data = aANOVASDEatt) )
summary(aLogitModelPerson)
valP_area_person = summary(aLogitModelPerson)[[1]][1,5]
valF_area_person = summary(aLogitModelPerson)[[1]][1,4]
print(paste('p = ', valP_area_person))
print(paste('F = ', valF_area_person))
print('SIGNIFICATIVO ???')


##############################################################################
##############################################################################
# WABA Analysis
##############################################################################
##############################################################################

library(WABA)

wWABASDEatt = aDataSDEatt

#WABA Analysis idPerson ~ area_sde
wDataWABAPerson = wDataWABAPerson %>% select(idPersonNum, area_sde)
waba(wDataWABAPerson)


#WABA Analysis idWeek ~ area_sde
wDataWABAWeek = select(wWABASDEatt, idWeekNum, area_sde)
waba(wDataWABAWeek)





















#######################################3
#########################################

mSDEatt = aDataSDEatt



