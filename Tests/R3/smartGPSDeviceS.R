library(dplyr)
library(jsonlite)
library(VIM)
library(lubridate)
library(ggmap)
library(sqldf)
library(vcd)


urlData = "D:/tmp/"
urlFigures = "D:/RSoftware/RProjects/Tests/"


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


urlData = "D:/RSoftware/RProjects/SmartGPSAnalysis/data/"

fileName = "device293.json"

data1 <- readJsonFile("data1.json")
dataSource <- data1



archivo <- paste(urlData,fileName,sep = "")
con <- file(archivo, open="r")
dataJson <- readLines(con)
close(con)

data293 <- fromJSON(dataJson)




rm(a)


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




