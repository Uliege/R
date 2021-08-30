#Script para realizar Random Interception Model
#Requiere ejecución del Script 04.4.1Read-WeekData.R

#Delete  work space variables (Caution!!)
#rm(list=ls())
#shell("cls")

#Archivo para almacenar el log de ejecución
#fileLog = paste(urlLogs,"logRandomInterceptionModel",trimws(str_replace_all(as.character(Sys.time()),":","")),".txt", sep = "")
#Conexión para escibir en el archivo
#con <- file(fileLog, open="a")
#Hora de inicio del proceso
#writeLines(text = as.character(Sys.time()), con = con)

library(dplyr)
library(stringr)
library(ggplot2)

#Directorios (url) de trabajo del proyecto
#urlProyecto = 'D:/G/GitHub/Uliege/R/TimeLineAnalysis'
urlProyecto = getwd()
urlData = paste(urlProyecto,"/data/",sep = "")
urlFigures = paste(urlProyecto,"/figures/",sep = "")
urlModels = paste(urlProyecto,"/models/",sep = "")
urlRawData = paste(urlProyecto,"/dataRaw/",sep = "")
urlReports = paste(urlProyecto,"/reports/",sep = "")
urlOutput = paste(urlProyecto,"/output/",sep = "")
urlExcelData = paste(urlProyecto,"/dataExcel/",sep = "")
urlLogs = paste(urlProyecto,"/logs/",sep = "")

#Load all Data for the analysis 
dataFileTotal = 'dataReadTOTAL2021-08-26.RData'
urlDataFileTotal = paste(urlRawData,dataFileTotal,sep="") # 36016 Observations
load(urlDataFileTotal)

rm(dataFileTotal)
rm(urlDataFileTotal)

#Filter individuals with at least 5 days data per week
minDaysPerWeek = 5 

#Filter original data (Weeks with at least 5 days data)
data.analysis.week = data.TOTAL.week %>%
  filter(numDays >= minDaysPerWeek) #2137, 21388, 22339, 22712, 22960, 23568 Observations

#Identify Person with data before and after COVID
idPersons = data.analysis.week %>% 
  group_by(idPerson, covidStatus) %>%
  count() %>%
  group_by(idPerson) %>%
  count() %>%
  filter(n==2)  #71 , 153, 161, 164, 168, 172  Individuals

data.analysis.week = data.analysis.week %>%
  filter(idPerson %in% idPersons$idPerson) #5999, 13964, 14646, 15105, 15350, 15746 Observations

rm(idPersons)

data.rim.week = data.analysis.week %>%
  select(idPerson, idPersonNum, idWeek, activityPoints, areaSDE, eccentricity, majorSigma, 
         distanceMST, totalTrips, totalPoints, totalMin, totalKm, numDays, yearMonth, covidStatus, 
         levelRestrictions)

rm(data.analysis.week)

allWeeks = unique(data.rim.week$idWeek) 
allWeeks = data.frame(idWeek = allWeeks)
allWeeks = allWeeks %>%
  arrange(idWeek) %>%
  mutate(idWeekNum = c(1:nrow(allWeeks)))

data.rim.week = data.rim.week %>%
  left_join(allWeeks)

rm(allWeeks)

data.rim.week = data.rim.week %>%
  select(idPerson, idPersonNum, idWeek, idWeekNum, yearMonth, covidStatus, levelRestrictions, 
         activityPoints, areaSDE, eccentricity, majorSigma, distanceMST, totalTrips, 
         totalPoints, totalMin, totalKm, numDays)

data.rim.week$idWeekNum = as.numeric(data.rim.week$idWeekNum)

summary(data.rim.week)

#Join socio-demographic data
data.rim.week.demographics = data.rim.week %>%
  left_join(data.demographic) %>% 
  na.omit() # 2043, 9936, 10301, 11179, 11958, 12367, 12571, 13194, 13229  Observations

length(unique(data.rim.week.demographics$idPerson)) # 23, 98, 100, 111, 122, 126, 128, 136  Individuals

rm(minDaysPerWeek)

summary(data.rim.week.demographics)

#Explore Data

#Normality

ggplot(data = data.rim.week, aes(x = areaSDE)) + #NO Normal
  geom_histogram()

ggplot(data = data.rim.week, aes(x = log(areaSDE))) +
  geom_histogram()

ggplot(data = data.rim.week.demographics, aes(x = areaSDE)) +
  geom_histogram()

ggplot(data = data.rim.week.demographics, aes(x = log(areaSDE))) +
  geom_histogram()


ggplot(data = data.rim.week, aes(x = distanceMST)) +  #Normal
  geom_histogram()

ggplot(data = data.rim.week, aes(x = log(distanceMST))) +
  geom_histogram()

ggplot(data = data.rim.week.demographics, aes(x = distanceMST)) + 
  geom_histogram()

ggplot(data = data.rim.week.demographics, aes(x = log(distanceMST))) +
  geom_histogram()



#Correlation Matrix
library('corrplot')
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(data.rim.week[8:17])
#More info about correlation Matrix: http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need


#Correlation

ggplot(data = data.rim.week, aes(x = areaSDE, y = majorSigma)) +
  geom_point() +
  geom_smooth(method = lm, se=F)

ggplot(data = data.rim.week, aes(x = log(areaSDE), y = majorSigma)) +
  geom_point() +
  geom_smooth(method = lm, se=F)

ggplot(data = data.rim.week.demographics, aes(x = areaSDE, y = majorSigma)) +
  geom_point() +
  geom_smooth(method = lm, se=F)

ggplot(data = data.rim.week.demographics, aes(x = log(areaSDE), y = majorSigma)) +
  geom_point() +
  geom_smooth(method = lm, se=F)


ggplot(data = data.rim.week, aes(x = areaSDE, y = distanceMST)) +
  geom_point() +
  geom_smooth(method = lm)


ggplot(data = data.rim.week, aes(x = areaSDE, y = activityPoints)) +
  geom_point() +
  geom_smooth(method = lm)


ggplot(data = data.rim.week, aes(x = areaSDE, y = totalPoints)) +
  geom_point() +
  geom_smooth(method = lm)



length(unique(data.TOTAL.week$idPerson))
length(unique(data.TOTAL.week.demographics$idPerson))

length(unique(data.rim.week$idPerson))
length(unique(data.rim.week.demographics$idPerson))

names(data.rim.week.demographics)

a = data.frame(cut(data.demographic$age, breaks = 5))

summary(a)

a = data.demographic %>% group_by(age) %>% count()

k = nclass.scott(data.demographic$age)
kA = diff(range(data.demographic$age)) / k

#######################################################################################
#######################################################################################
# Data for Random Interception Model
# 1 entry per person, per week
# Dependent Variable: Activity Space Characteristics
# Independent Variable (Explanatory Factors): COVID status, Month, Week???
#########################################################################################
#######################################################################################


#############################################################################
#############################################################################
# Random Interception Model (Mixed Effects Analysis)
#############################################################################
############################################################################

#Fixed Effects: Independent Variables - Explanatory Factors (x)
#Random Effects: Random Factors (Groups) Persons and allWeeks
# y ~ x + (1|group) 

library(lme4)
library(Matrix)
library(broom.mixed)
library(Rcpp)
library(rlang)


# Random Intercept Model
# REML=FALSE - Do this when you compare models using the likelihood ratio test (Pinheiro & Bates, 2000; Bolker et al., 2009)

#####################################################################################
# Area Base Model - Intercept Model (Estimate de mean of the data)
#####################################################################################
rim.area.base = lmer(areaSDE ~ 1 + (1|idPerson), REML=FALSE, data=data.rim.week)
print(rim.area.base)
tidy(rim.area.base, conf.int = T)

# Area ~ covidStatus Model  
# Model: areaSDE ~ covidStatus + (1|idPerson)
# Fixed Effect: areaSDE
# Random Effect: idPerson  -  1 Random Effect
# Control Effect: covidStatus
# H-null: Activity space's Area isn't influenced (affected) by COVID
# H-alterna: Activity space's Area is influenced (affected) by COVID

rim.area.covid = lmer(areaSDE ~ covidStatus + (1|idPerson), REML=FALSE, data = data.rim.week)
print(rim.area.covid)
tidy(rim.area.covid, conf.int = T)
#Compare Models
anova(rim.area.base, rim.area.covid) 
# Interpretation: Reject H0
# COVID affect significantly SDE area (Chi2(1)=14.432, p=0.0001453, alpha=0, 100%)
# COVID decrease SDE area by about 2.72 Km2 ± 0.717 (standard errors)

#Pre -> Increase, Pos -> Decrease (In the same proportion)
data.rim.week$covidStatus1 = ifelse(data.rim.week$covidStatus == 'PreCovid', 'No' ,'Yes')
rim.area.covid1 = lmer(areaSDE ~ covidStatus1 + (1|idPerson), REML=FALSE, data = data.rim.week)
print(rim.area.covid1)
tidy(rim.area.covid1, conf.int = T)
#Compare Models
anova(rim.area.base, rim.area.covid1) 



# Model: areaSDE ~ levelRestrictions + (1|idPerson)
# Fixed Effect: areaSDE
# Random Effect: idPerson  -  1 Random Effect
# Control Effect: levelRestrictions
# H-null: Activity space's Area isn't influenced (affected) by different levels of restrictions due the COVID
# H-alterna: Activity space's Area is influenced (affected) by levels
rim.area.levelRestrictions = lmer(areaSDE ~ levelRestrictions + (1|idPerson), REML=FALSE, data = data.rim.week)
print(rim.area.levelRestrictions)
tidy(rim.area.levelRestrictions, conf.int = T)
#Compare Models
anova(rim.area.base, rim.area.levelRestrictions) 
# Interpretation: 
# Different COVID restriction's level affect significantly SDE area (Chi2(1)=45.942, p=5.836e-11, alpha=0, 100%)
# 1	Low restrictions: decrease SDE area by about 0.245 Km2 ± 0.90 (se) ?Correct??
# 2	Middle restrictions: decrease SDE area by about 3.34 Km2 ± 1.11 (se)
# 3	Full restrictions: decrease SDE area by about 9.47 Km2 ± 1.55 (se)



#####################################################################################
# Minimun Spanning Tree Base Model - Intercept Model (Estimate de mean of the data)
#####################################################################################
rim.mst.base = lmer(distanceMST ~ 1 + (1|idPerson), REML=FALSE, data=data.rim.week)
print(rim.mst.base)
tidy(rim.mst.base, conf.int = T)

# MST ~ covidStatus Model  
# Model: distanceMST ~ covidStatus + (1|idPerson)
# Fixed Effect: distanceMST
# Random Effect: idPerson  -  1 Random Effect
# Control Effect: covidStatus
# H-null: MST's distance isn't influenced (affected) by COVID
# H-alterna: MST's distance is influenced (affected) by COVID
rim.mst.covid = lmer(distanceMST ~ covidStatus + (1|idPerson), REML=FALSE, data = data.rim.week)
print(rim.mst.covid)
tidy(rim.mst.covid, conf.int = T)
#Compare Models
anova(rim.mst.base, rim.mst.covid) 
# Interpretation: 
# COVID affect significantly MST distance (Chi2(1)=362.17, p=2.2e-16, alpha=0, 100%)
# COVID decrease MST distance by about 5.35 Km ± 0.280 (standard errors)


# Model: distanceMST ~ levelRestrictions + (1|idPerson)
# Fixed Effect: distanceMST
# Random Effect: idPerson  -  1 Random Effect
# Control Effect: levelRestrictions
# H-null: MST's distance isn't influenced (affected) by different levels of restrictions due the COVID
# H-alterna: MST's distance is influenced (affected) by levels
rim.mst.levelRestrictions = lmer(distanceMST ~ levelRestrictions + (1|idPerson), REML=FALSE, data = data.rim.week)
print(rim.mst.levelRestrictions)
tidy(rim.mst.levelRestrictions, conf.int = T)
#Compare Models
anova(rim.mst.base, rim.mst.levelRestrictions) 
# Interpretation: 
# Different COVID restriction's level affect significantly MST distance (Chi2(1)=427.5, p=2.2e-16, alpha=0, 100%)
# 1	Low restrictions: decrease MST distance by about 4.06 Km ± 0.352 (se)
# 2	Middle restrictions: decrease MST distance by about 5.46 Km ± 0.433 (se)
# 3	Full restrictions: decrease MST distance by about 9.28 Km ± 0.588 (se)

###########
#Full data
##########
###########
#Area
##########
all.rim.area.base = lmer(areaSDE ~ 1 + (1|idPerson), REML=FALSE, data=data.TOTAL.week)
print(all.rim.area.base)
tidy(all.rim.area.base, conf.int = T)
all.rim.area.covid = lmer(areaSDE ~ covidStatus + (1|idPerson), REML=FALSE, data = data.TOTAL.week)
print(all.rim.area.covid)
tidy(all.rim.area.covid, conf.int = T)
anova(all.rim.area.base, all.rim.area.covid) 
# Interpretation: 
# COVID affect significantly SDE area (Chi2(1)=41.139, p=1.417e-10, alpha=0, 100%)
# COVID decrease SDE area by about 3.59 Km2 ± 0.560 (standard errors)


all.rim.area.levelRestrictions = lmer(areaSDE ~ levelRestrictions + (1|idPerson), REML=FALSE, data = data.TOTAL.week)
print(all.rim.area.levelRestrictions)
tidy(all.rim.area.levelRestrictions, conf.int = T)
anova(all.rim.area.base, all.rim.area.levelRestrictions) 
# Interpretation: 
# Different COVID restriction's level affect significantly SDE area (Chi2(1)=131.47, p=2.2e-11, alpha=0, 100%)
# 1	Low restrictions: decrease SDE area by about 0.256 Km2 ± 0.706 (se)
# 2	Middle restrictions: decrease SDE area by about 4.50 Km2 ± 0.851 (se)
# 3	Full restrictions: decrease SDE area by about 12.3 Km2 ± 1.17 (se)


######################
#Minimun Spanning Tree
######################
all.rim.mst.base = lmer(distanceMST ~ 1 + (1|idPerson), REML=FALSE, data=data.TOTAL.week)
print(all.rim.mst.base)
tidy(all.rim.mst.base, conf.int = T)

all.rim.mst.covid = lmer(distanceMST ~ covidStatus + (1|idPerson), REML=FALSE, data = data.TOTAL.week)
print(all.rim.mst.covid)
tidy(all.rim.mst.covid, conf.int = T)
anova(all.rim.mst.base, all.rim.mst.covid) 
# Interpretation: 
# COVID affect significantly MST distance (Chi2(1)=912.45, p=2.2e-16, alpha=0, 100%)
# COVID decrease MST distance by about 5.96 Km ± 0.196 (standard errors) - approx same than first

all.rim.mst.levelRestrictions = lmer(distanceMST ~ levelRestrictions + (1|idPerson), REML=FALSE, data = data.TOTAL.week)
print(all.rim.mst.levelRestrictions)
tidy(all.rim.mst.levelRestrictions, conf.int = T)
anova(all.rim.mst.base, all.rim.mst.levelRestrictions) 
# Interpretation: 
# Different COVID restriction's level affect significantly MST distance (Chi2(1)=1134.4, p=2.2e-16, alpha=0, 100%)
# 1	Low restrictions: decrease MST distance by about 4.26 Km ± 0.246 (se)
# 2	Middle restrictions: decrease MST distance by about 6.14 Km ± 0.297 (se)
# 3	Full restrictions: decrease MST distance by about 10.9 Km ± 0.407 (se)


all.rim.mst.levelRestrictions1 = lmer(distanceMST ~ levelRestrictions + activityPoints + (1|idPerson), REML=FALSE, data = data.TOTAL.week)
print(all.rim.mst.levelRestrictions1)
tidy(all.rim.mst.levelRestrictions1, conf.int = T)
anova(all.rim.mst.base, all.rim.mst.levelRestrictions1) 





#############################################################################################################################
#############################################################################################################################
# Random Intercept Model + Demographics
#############################################################################################################################
#############################################################################################################################

summary(data.rim.week.demographics)

# REML=FALSE - Do this when you compare models using the likelihood ratio test (Pinheiro & Bates, 2000; Bolker et al., 2009)

# Area Base Model - Intercept Model (Estimate de mean of the data)
rimd.area.base = lmer(areaSDE ~ 1 + (1|idPerson), REML=FALSE, data=data.rim.week.demographics)
print(rimd.area.base)
tidy(rimd.area.base, conf.int = T)

rimd.area.covid = lmer(areaSDE ~ covidStatus + (1|idPerson), REML=FALSE, data = data.rim.week.demographics)
print(rimd.area.covid)
tidy(rimd.area.covid, conf.int = T)
anova(rimd.area.base, rimd.area.covid) 
# Interpretation: 
# COVID affect significantly SDE area (Chi2(1)=13.626, p=0.0002231, alpha=0, 100%)
# COVID decrease SDE area by about 2.96 Km2 ± 0.802 (standard errors)

rimd.area.levelRestrictions = lmer(areaSDE ~ levelRestrictions + (1|idPerson), REML=FALSE, data = data.rim.week.demographics)
print(rimd.area.levelRestrictions)
tidy(rimd.area.levelRestrictions, conf.int = T)
anova(rimd.area.base, rimd.area.levelRestrictions) 
# Interpretation: 
# Different levels of COVID restrictions affect significantly SDE area (Chi2(1)=37.704, p=3.266e-08, alpha=0, 100%)
# 1	Low restrictions: decrease SDE area by about 0.449 Km2 ± 1.02 (se) ?Correct??
# 2	Middle restrictions: decrease SDE area by about 3.68 Km2 ± 1.20 (se)
# 3	Full restrictions: decrease SDE area by about 9.66 Km2 ± 1.74 (se)

#Incorporate demographics
rimd.area.levelRestrictions.2 = lmer(areaSDE ~ levelRestrictions 
                                     + age + gender + homeLocation + vehicleOwner + usualTransportPattern
                                     + workingStatus + childrenU12
                                     + (1|idPerson), 
                                     REML=FALSE, 
                                     data = data.rim.week.demographics)
print(rimd.area.levelRestrictions.2)
tidy(rimd.area.levelRestrictions.2, conf.int = T)
anova(rimd.area.base, rimd.area.levelRestrictions.2) 





#############################





















# area.sde ~ covid + (1|idWeek) #1 Random Effect
# Area Base Model
##rimWeek.area.base = lmer(sde.Area ~ 1 + (1|idWeek), REML=FALSE, data=data.rim.week) #Intercept Model (Estimate de mean of the data)
##print(rimWeek.area.base)
##tidy(rimWeek.area.base, conf.int = T) #
# Area ~ covidStatus
##rimWeek.area.covid = lmer(sde.Area ~ 1 + covidStatus + (1 | idWeek), REML=FALSE, data = data.rim.week)
##print(rimWeek.area.covid)
##tidy(rimWeek.area.covid, conf.int = T)
##anova(rimWeek.area.base, rimWeek.area.covid) 
# Interpretation: COVID affected sde area (Chi2(1)=10.753, p=0.001041, alpha=0.001, 99%), decreasing it by about 4.25 Km2 ± 1.28 (standard errors) 
# Approx the same results than idPerson.


# 1 Random Effect: idPerson
# area.sde ~ covid + (1|idPerson) #1 Random Effect

#Area Base Model
rimPerson.area.base = lmer(sde.Area ~ 1 + (1|idPerson), REML=FALSE, data=data.rim.week) #Intercept Model (Estimate de mean of the data)
#REML=FALSE - Do this when you compare models using the likelihood ratio test (Pinheiro & Bates, 2000; Bolker et al., 2009)
print(rimPerson.area.base)
tidy(rimPerson.area.base, conf.int = T) #
#Area ~ covidStatus  -  Does the activity space's Area is influenced by COVID?
rimPerson.area.covid = lmer(sde.Area ~ 1 + covidStatus + (1|idPerson), REML=FALSE, data = data.rim.week)
print(rimPerson.area.covid)
tidy(rimPerson.area.covid, conf.int = T)
#Compare Models
anova(rimPerson.area.base, rimPerson.area.covid) 
# Interpretation: COVID affected sde area (Chi2(1)=17.457, p=2.938e-05, alpha=0, 100%), decreasing it by about 4.50 Km2 ± 1.08 (standard errors)


#MST Base Model
rimPerson.mst.base = lmer(mst.Distance ~ 1 + (1|idPerson), REML=FALSE, data=data.rim.week) #Intercept Model (Estimate de mean of the data)
print(rimPerson.mst.base)
tidy(rimPerson.mst.base, conf.int = T) #
#MST ~ covidStatus  -  Does the Minimum Spanning Tree Lenght is influenced by COVID?
rimPerson.mst.covid = lmer(mst.Distance ~ 1 + covidStatus + (1|idPerson), REML=FALSE, data = data.rim.week)
print(rimPerson.mst.covid)
tidy(rimPerson.mst.covid, conf.int = T)
#Compare Models
anova(rimPerson.mst.base, rimPerson.mst.covid) 
# Interpretation: COVID affected mst length (Chi2(1)=150.76, p=2.2e-16, alpha=0, 100%), decreasing it by about 5.30 Km ± 0.429 (standard errors)



#2 Random Effects: idPerson + idWeek
# area.sde ~ covid + (1|idPerson) +  (1|idWeek)   #2 Random Effects

#Area Base Model
rimPersonWeek.area.base = lmer(sde.Area ~ 1 + (1|idPerson) + (1|idWeek), REML=FALSE, data=data.rim.week)
print(rimPersonWeek.area.base)
tidy(rimPersonWeek.area.base, conf.int = T) #
#Covid Model #Area ~ covidStatus  -  Does the activity space's Area is influenced by COVID?
rimPersonWeek.area.covid = lmer(sde.Area ~ covidStatus + (1|idPerson) + (1|idWeek), REML=FALSE, data = data.rim.week)
print(rimPersonWeek.area.covid)
tidy(rimPersonWeek.area.covid, conf.int = T)
anova(rimPersonWeek.area.base, rimPersonWeek.area.covid) 
#Interpretation: COVID affected SDE area (Chi2(1)=7.5844, p=0.005888, alpha=0.001, 99%), decreasing it by about 3.89 Km2 ± 1.38 (standard errors)


#MST Base Model
rimPersonWeek.mst.base = lmer(mst.Distance ~ 1 + (1|idPerson) + (1|idWeek), REML=FALSE, data=data.rim.week) #Intercept Model (Estimate de mean of the data)
print(rimPersonWeek.mst.base)
tidy(rimPersonWeek.mst.base, conf.int = T) #
#MST ~ covidStatus  -  Does the Minimum Spanning Tree Lenght is influenced by COVID?
rimPersonWeek.mst.covid = lmer(mst.Distance ~ 1 + covidStatus + (1|idPerson) + (1|idWeek), REML=FALSE, data = data.rim.week)
print(rimPersonWeek.mst.covid)
tidy(rimPersonWeek.mst.covid, conf.int = T)
#Compare Models
anova(rimPersonWeek.mst.base, rimPersonWeek.mst.covid) 
# Interpretation: COVID affected mst length (Chi2(1)=28.854, p=7.805e-08, alpha=0, 100%), decreasing it by about 4.21 Km ± 0.727 (standard errors)



######################################################################################################################################
#
# Model joinig demographic data
#
######################################################################################################################################

# 1 Random Effect: idPerson
# area.sde ~ covid + (1|idPerson) #1 Random Effect

#Area Base Model
rimPerson.area.base_2 = lmer(sde.Area ~ 1 + (1|idPerson), REML=FALSE, data=data.rim.week.demographics) #Intercept Model (Estimate de mean of the data)
#REML=FALSE - Do this when you compare models using the likelihood ratio test (Pinheiro & Bates, 2000; Bolker et al., 2009)
print(rimPerson.area.base_2)
tidy(rimPerson.area.base_2, conf.int = T) #
#Area ~ covidStatus  -  Does the activity space's Area is influenced by COVID?
rimPerson.area.covid_2 = lmer(sde.Area ~ 1 + covidStatus + gender + own_vehicle + workingStatus + (1|idPerson), REML=FALSE, data = data.rim.week.demographics)
print(rimPerson.area.covid_2)
tidy(rimPerson.area.covid_2, conf.int = T)
#Compare Models
anova(rimPerson.area.base_2, rimPerson.area.covid_2) 
# Interpretation: COVID affected sde area (Chi2(1)=10.453, p=0.001225, alpha=0.001, 99%), decreasing it by about 7.14 Km2 ± 2.21 (standard errors)


#MST Base Model
rimPerson.mst.base_2 = lmer(mst.Distance ~ 1 + (1|idPerson), REML=FALSE, data=data.rim.week.demographics) #Intercept Model (Estimate de mean of the data)
print(rimPerson.mst.base_2)
tidy(rimPerson.mst.base_2, conf.int = T) #
#MST ~ covidStatus  -  Does the Minimum Spanning Tree Lenght is influenced by COVID?
rimPerson.mst.covid_2 = lmer(mst.Distance ~ 1 + covidStatus + (1|idPerson), REML=FALSE, data = data.rim.week.demographics)
print(rimPerson.mst.covid_2)
tidy(rimPerson.mst.covid_2, conf.int = T)
#Compare Models
anova(rimPerson.mst.base_2, rimPerson.mst.covid_2) 
# Interpretation: COVID affected mst length (Chi2(1)=70.659, p=2.2e-16, alpha=0, 100%), decreasing it by about 6.54 Km ± 0.772 (standard errors)



#2 Random Effects: idPerson + idWeek
# area.sde ~ covid + (1|idPerson) +  (1|idWeek)   #2 Random Effects

#Area Base Model
rimPersonWeek.area.base_2 = lmer(sde.Area ~ 1 + (1|idPerson) + (1|idWeek), REML=FALSE, data=data.rim.week.demographics)
print(rimPersonWeek.area.base_2)
tidy(rimPersonWeek.area.base_2, conf.int = T) #
#Covid Model #Area ~ covidStatus  -  Does the activity space's Area is influenced by COVID?
rimPersonWeek.area.covid_2 = lmer(sde.Area ~ covidStatus + (1|idPerson) + (1|idWeek), REML=FALSE, data = data.rim.week.demographics)
print(rimPersonWeek.area.covid_2)
tidy(rimPersonWeek.area.covid_2, conf.int = T)
anova(rimPersonWeek.area.base_2, rimPersonWeek.area.covid_2) 
#Interpretation: COVID affected SDE area (Chi2(1)=7.9571, p=0.00479, alpha=0.001, 99%), decreasing it by about 6.82 Km2 ± 2.37 (standard errors)


#MST Base Model
rimPersonWeek.mst.base_2 = lmer(mst.Distance ~ 1 + (1|idPerson) + (1|idWeek), REML=FALSE, data=data.rim.week.demographics) #Intercept Model (Estimate de mean of the data)
print(rimPersonWeek.mst.base_2)
tidy(rimPersonWeek.mst.base_2, conf.int = T) #
#MST ~ covidStatus  -  Does the Minimum Spanning Tree Lenght is influenced by COVID?
rimPersonWeek.mst.covid_2 = lmer(mst.Distance ~ 1 + covidStatus + (1|idPerson) + (1|idWeek), REML=FALSE, data = data.rim.week.demographics)
print(rimPersonWeek.mst.covid_2)
tidy(rimPersonWeek.mst.covid_2, conf.int = T)
#Compare Models
anova(rimPersonWeek.mst.base_2, rimPersonWeek.mst.covid_2) 
# Interpretation: COVID affected mst length (Chi2(1)=45.965, p=1.2045e-11, alpha=0, 100%), decreasing it by about 6.38 Km ± 0.862 (standard errors)









































#Full model
# Does the activity space's Area is influenced by: Gender, Age  and Residential Location?

model_full = lmer(Area.sde ~ gender + age + residential_location + (1 | idWeek), data = data.rim.week.demograp)

print(model_full)

tidy(model_full, conf.int = T)









########################################################################################################################################
########################################################################################################################################
### COVID + DEMOGRAPHIC DATA
########################################################################################################################################
########################################################################################################################################

names(data.rim.week.demograp)

library(lme4)

#1 Random Effect: Person vs Area
#Base Model
rim_base_area = lmer(Area.sde ~ 1 + (1|idPerson), REML=FALSE, data=data.rim.week.demograp) #Intercept Model (Estimate de mean of the data)
print(rim_base_area)
tidy(rim_base_area, conf.int = T) #

#Fixed Effect: covidStatus vs Area
rim_covid_area = lmer(Area.sde ~ 1 + covidStatus + (1 | idPerson), REML=FALSE, data = data.rim.week.demograp)
print(rim_covid_area)
tidy(rim_covid_area, conf.int = T)

anova(rim_base_area, rim_covid_area) #COVID affected Area.sde (Chi2(1)=10.453, p=0.001225, alpha=0.001, 99%), decreasing it by about 7.14 Km2 ± 2.21 (standard errors)

library(lmerTest)

ranova(rim_base1)
ranova(rim_covid1)


#1 Random Effect: Person vs MST
#Base Model
rim_base_mst = lmer(mstDistance ~ 1 + (1|idPerson), REML=FALSE, data=data.rim.week.demograp) #Intercept Model (Estimate de mean of the data)
print(rim_base_mst)
tidy(rim_base_mst, conf.int = T) #

#Fixed Effect: covidStatus vs MST
rim_covid_mst = lmer(mstDistance ~ 1 + covidStatus + (1 | idPerson), REML=FALSE, data = data.rim.week.demograp)
print(rim_covid_mst)
tidy(rim_covid_mst, conf.int = T)

anova(rim_base_mst, rim_covid_mst) #COVID affected MST Distance (Chi2(1)=70.659, p=2.2e-16, alpha=0, 100%), decreasing it by about 6.54 Km ± 2.84 (standard errors)


#1 Random Effect: Person vs totalTrips
#Base Model
rim_base_trips = lmer(totalTrips ~ 1 + (1|idPerson), REML=FALSE, data=data.rim.week.demograp) #Intercept Model (Estimate de mean of the data)
print(rim_base_trips)
tidy(rim_base_trips, conf.int = T) #

#Fixed Effect: covidStatus vs totalTrips
rim_covid_trips = lmer(totalTrips ~ 1 + covidStatus + (1 | idPerson), REML=FALSE, data = data.rim.week.demograp)
print(rim_covid_trips)
broom.mixed::tidy(rim_covid_trips, conf.int = T)

anova(rim_base_trips, rim_covid_trips) #COVID affected Total Trips during week (Chi2(1)=54.187, p=1.823e-13, alpha=0, 100%), decreasing it by about 4.59 Trips ± 1.43 (standard errors)



library(broom.mixed)
















#covidStatus + university_individual Model
rim_covid_Univerity1 = lmer(Area.sde ~ covidStatus + university_individual + (1 | idPerson), REML=FALSE, data = data.rim.week.demograp)
print(rim_covid_Univerity1)
tidy(rim_covid_Univerity1, conf.int = T)

anova(rim_covid1, rim_covid_Univerity1) #Be part of the University affected Area.sde (Chi2(1)=29.83, p=4.702e-08, 100%), decreasing 71.73 km2 ± 9 (se) ???? IMPOSSIBLE, MEAN =32 Km2

#Randomn Inteercepts
coef(rim_base1)
coef(rim_covid1)
coef(rim_covid_Univerity1)





#Include Random Slopes

#Base Model
rim_rs_base1 = lmer(Area.sde ~ 1 + (1 + covidStatus|idPerson), REML=FALSE, data=data.rim.week.demograp) #Intercept Model (Estimate de mean of the data)
print(rim_rs_base1)
tidy(rim_rs_base1, conf.int = T) #

#covidStatus Model ---->>>>
rim_rs_covid1 = lmer(Area.sde ~ covidStatus + (1 + covidStatus|idPerson), REML=FALSE, data = data.rim.week.demograp)
print(rim_rs_covid1)
summary(rim_rs_covid1)
tidy(rim_rs_covid1, conf.int = T)

anova(rim_rs_base1, rim_rs_covid1) #NO SIGNIFICANT!!, p=0.1473

anova(rim_base1, rim_rs_covid1)


#covidStatus + university_individual Model
rim_rs_covid_Univerity1 = lmer(Area.sde ~ university_individual + covidStatus + (1 + covidStatus|idPerson), REML=FALSE, data = data.rim.week.demograp)
print(rim_rs_covid_Univerity1)
tidy(rim_rs_covid_Univerity1, conf.int = T)

anova(rim_rs_covid1, rim_rs_covid_Univerity1) #Be part of the University affected Area.sde (Chi2(1)=25.131, p=5.358e-07, 100%), decreasing 67.6 km2 ± 9.72 (se) ???? IMPOSSIBLE, MEAN =29.65 Km2



#2 Random Effects: Person + Week

#Base Model
rim_base2 = lmer(Area.sde ~ 1 + (1|idPerson) + (1 | idWeek), REML=FALSE, data=data.rim.week.demograp)
print(rim_base2)
tidy(rim_base2, conf.int = T) #

#Covid Model
rim_covid2 = lmer(Area.sde ~ covidStatus + (1 | idPerson) + (1 | idWeek), REML=FALSE, data = data.rim.week.demograp)
print(rim_covid2)
tidy(rim_covid2, conf.int = T)

anova(rim_base2, rim_covid2) #COVID affected Area.sde (Chi2(1)=7.95, p=0.00479, alpha=0.001 (99%)), decreasing it by about 6.82 Km2 ± 2.37 (standard errors)

#Random Intercepts
coef(rim_base2)
coef(rim_covid2)

plot(rim_base2)
plot(rim_covid2)


#Include Random Slopes

#Base Model
rim_rs_base2 = lmer(Area.sde ~ 1 + (1 + covidStatus|idPerson) + (1 + covidStatus|idWeek), REML=FALSE, data=data.rim.week.demograp)
print(rim_rs_base2)
tidy(rim_rs_base2, conf.int = T) #

#Covid Model
rim_rs_covid2 = lmer(Area.sde ~ covidStatus + (1 + covidStatus|idPerson) + (1 + covidStatus|idWeek), REML=FALSE, data = data.rim.week.demograp)
print(rim_rs_covid2)
tidy(rim_rs_covid2, conf.int = T)

anova(rim_rs_base2, rim_rs_covid2) #NO SIGNIFICANT!!, p=0.1673

plot(rim_rs_base2)
plot(rim_rs_covid2)


#Base Model (LOG)
rim_rs_base2.log = lmer(log(Area.sde) ~ 1 + (1 + covidStatus|idPerson) + (1 + covidStatus|idWeek), REML=FALSE, data=data.rim.week.demograp)
print(rim_rs_base2.log)
tidy(rim_rs_base2.log, conf.int = T) #

#Covid Model (LOG)
rim_rs_covid2.log = lmer(log(Area.sde) ~ covidStatus + (1 + covidStatus|idPerson) + (1 + covidStatus|idWeek), REML=FALSE, data = data.rim.week.demograp)
print(rim_rs_covid2.log)
tidy(rim_rs_covid2.log, conf.int = T)

anova(rim_rs_base2.log, rim_rs_covid2.log) #SIGNIFICANT!!, Chi2(1)=4.7566, p=0.02919, alpha=0.01, 99%. COVID affected log(Area.sde) ????




hist(data.rim.week.demograp$Area.sde)

hist(data.rim.week.demograp$Area.sde*data.rim.week.demograp$Area.sde)

hist(log(data.rim.week.demograp$Area.sde))




#idWeek

rim_rs_area = lmer(Area.sde ~ idWeek + (1 + idWeek|idPerson), data=data.rim.week)

rim_rs_area = lmer(Area.sde ~ idWeek + (1 + idWeek|idPerson), data=data.rim.week.demograp)





ggplot(data = data.rim.week, aes(x = idWeek, y = Area.sde, group = idPerson)) +
  geom_line()


ggplot(data = data.rim.week, aes(x = idPerson, y = Area.sde, group = idWeek)) +
  geom_line()














