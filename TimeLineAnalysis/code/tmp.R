
search()


activities

sapply(activities)

runif

numeric

a<-matrix(runif(6,1,10), 3, 2)

b<-list(runif(3,1,10))
b

x<- data.frame(append(a,b))
x <- data.frame(append(b,a))
append(1:5, 0:1)


l <- NULL
l<- append(l,3)
l<- append(l,5)
l<- append(l,NA)





listaTime <- NULL
for (i in 1:length(activities)) {
  if(is.null(activities[[i]])){
    ts <- NA
  }else{
    ts <- as.numeric(activities[[i]]$timestampMs[[1]])
  }
  listaTime <- append(listaTime, ts)
}


dataActivities1 <- NULL
i<-0
lTs <- NULL
lact1 <- NULL
lcon1 <- NULL
lact2 <- NULL
lcon2 <- NULL

for (i in 1:length(activities)) {
  if(is.null(activities[[i]])){
    dateActivity <- NA
    activity1 <- NA
    confidence1 <- NA
    activity2 <- NA
    confidence2 <- NA
  }else{
    timestampMs <- as.numeric(activities[[i]]$timestampMs[[1]])
    dateActivity <- as.POSIXct(timestampMs/1000, origin="1970-01-01")
    activity1 <- activities[[i]]$activity[[1]]$type[[1]]
    confidence1 <- activities[[i]]$activity[[1]]$confidence[[1]]
    if(length(activities[[i]]$activity[[1]]$type)==1){
      activity2 <- NA
      confidence2 <- NA
    }else{
      activity2 <- activities[[i]]$activity[[1]]$type[[2]]
      confidence2 <- activities[[i]]$activity[[1]]$confidence[[2]]  
    }
  }
  lTs <- append(lTs, dateActivity)
  lact1 <- append(lact1,activity1)
  lcon1 <- append(lcon1,confidence1)
  lact2 <- append(lact2,activity2)
  lcon2 <- append(lcon2,confidence2)
}

dataActivities1 = data.frame(lTs, lact1, lcon1, lact2, lcon2)



dataLocation <- data.frame(dateLocation, latitude, longitude, accuracy, verticalAccuracy, altitude, velocity, heading)
dataActivities = rbind(dataActivities, data.frame(dateActivity, activity1, confidence1, activity2, confidence2))


totalFrame <- data.frame(dataLocation,dataActivities)

dataSource <- totalFrame


