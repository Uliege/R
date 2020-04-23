summary(dataQuitoTotal)


dataQuitoTotal$contador = 1

library(ggplot2)

Y_W = dataQuitoTotal %>% 
  group_by(Y, M) %>% 
  count(Y, M)


Y_W2 = dataQuitoTotal %>% 
  group_by(Y, M) %>% 
  summarize(n())

data_Files = dataQuitoTotal %>% 
  group_by(id) %>% 
  count(id)


head(dataQuitoTotal)

library(dplyr)
dataTld0 = filter(dataQuitoTotal, id=="tld0")

dataTld100 = filter(dataQuitoTotal, id=="tld100")

dataTld160 = filter(dataQuitoTotal, id=="tld160")

str(dataTld0)

ggplot(dataTld0, aes(x = activity1)) +
  geom_histogram( stat = "count") +
  facet_wrap(~ Y)

diff(range(dataTld0$accuracy))

table(dataTld0$accuracy)

table(log(dataTld0$accuracy))

ggplot(dataTld0, aes(x = log(accuracy))) +
  geom_density()


library(openintro)

cor(dataTld0$accuracy, dataTld0$altitude)

cor(dataTld0$accuracy, dataTld0$altitude, use = "pairwise.complete.obs")

cor(dataTld0$accuracy, dataTld0$altitude, use = "complete.obs")

ggplot(dataTld0, aes(x = accuracy, y = altitude)) + 
  geom_point()




cor(dataTld0$accuracy, dataTld0$velocity, use = "pairwise.complete.obs")


ggplot(dataTld0, aes(x = dateTimeLine, y = accuracy)) +
  geom_point()

ggplot(dataTld0, aes(x = dateTimeLine, y = accuracy)) +
  geom_point(position = "jitter")


hclust()

cutree()




a1 = cutree(hclust(dist(USArrests)), k = 1:5) #k

a2 = cutree(hclust(dist(USArrests)), h = 250)

typeof(a2)

plot(hclust(dist(USArrests)))

library(dendextend)

hDen = as.dendrogram(hclust(dist(USArrests)))

plot(hDen)

plot(color_branches(hDen, h=200))

plot(color_branches(hDen, h=250))

plot(color_branches(hDen, h=100))

plot(color_branches(hDen, h=50))




##Para calcular k en modelo k-means

library(purrr)

xCoord = select(dataTld0, 2:3)

xCoord = select(dataTld100, 2:3)

xCoord = select(dataTld160, 2:3)

xCoord = select(dataQuitoTotal, 2:3)


tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = xCoord, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10 ,
  tot_withinss = tot_withinss
)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)




#####
##Supervised Learning

##knn
xData= NULL

xData = data.frame(c(1,2,5,6,4,10), c(3,4,6,6,8,13), c(4,6,11,12,12,23))

names(xData) = c('x','y','z')

xData


library(class)

xTrain = xData[-3]
xLabel = xData$z
xTest = c(3,20)

knn(xTrain, xTest, xLabel)



subset()


glm()


mean()

relevel()



##Visualizando con R el historial de ubicaciones de Google (parte I)

## H. Antonio Vazquez Brust

## https://bitsandbricks.github.io/post/visualizando-con-r-el-historial-de-ubicaciones-de-google-parte-i/

library(jsonlite)
raw <- fromJSON('D:/G/GitHub/Uliege/R/TimeLineAnalysis/data/tld0.json')

library(tidyverse)
library(lubridate)

locs <- raw$locations
locationdf <- data.frame(t=rep(0,nrow(locs)))

# convertimos lat y long a variables numéricas
locationdf$lat <- as.numeric(locs$latitudeE7/1E7)
locationdf$lon <- as.numeric(locs$longitudeE7/1E7)

# Nos llevamos los datos de precisión
locationdf$accuracy <- locs$accuracy

# Y la actividad más probable para cada lectura de posición
act <- map_df(locs$activity, 
              function(f) {
                if(is.null(f[[1]])) 
                  data.frame(activity=NA,confidence=NA,stringsAsFactors=F) 
                else 
                  data.frame(activity=f[[2]][[1]][[1]][1],
                             confidence=f[[2]][[1]][[2]][1],stringsAsFactors=F)
              })

# Agregar los datos de actividad a nuestro dataframe principal
locationdf$activity <- act$activity
locationdf$confidence <- act$confidence

# Velocity, altitude y heading también
locationdf$velocity <- locs$velocity
locationdf$altitude <- locs$altitude
locationdf$heading <- locs$heading

# Agregar un campo con fecha en calendario gregoriano, 
# y campos para día de la semana, mes y año
# El formato de la fecha es POSIX * 1000 (milliseconds) lo pasamos a una escala más útil...
locationdf$date <- as.numeric(locs$timestampMs)/1000
class(locationdf$date) <- 'POSIXct'
locationdf$weekday <- weekdays(as.Date(locationdf$date))
locationdf$month <- months(as.Date(locationdf$date))
locationdf$year <- year(as.Date(locationdf$date))

# En el campo "activity" convertimos valores NA en "UNKNOWN"
locationdf$activity = ifelse(is.na(locationdf$activity), "UNKNOWN", locationdf$activity)

# Agregar un indice y ordenarlo en reversa (el registro más reciente al final)
locationdf <- locationdf[rev(rownames(locationdf)),]

head(locationdf)

library(hrbrthemes)

# Renombrar activity como actividad, crear campo con mes y año, agrupar por actividad + fecha
locationdf %>% 
  mutate(fecha = ymd(date(date))) %>% 
  group_by(activity, fecha) %>% 
  summarise(total = n()) %>% 
  arrange(fecha, activity, desc(total)) %>% 
  ggplot(aes(x=fecha, y=total)) + 
  geom_area(aes(fill=activity), position="stack") +
  scale_x_date() +
  ylim(c(0, 1750)) +
  labs(y="registros",
       title="Historial de ubicaciones de Google",
       subtitle="Cantidad de registros por dia y por actividad") +
  scale_fill_brewer(palette = "Set3") +
  theme_ipsum()

library(waffle)

top5 <- locationdf %>% 
  filter(activity != "UNKNOWN") %>% 
  group_by(activity) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  slice(1:5)

# 1 cuadradito del waflle = 10000 registros
# De paso, conertimso el dataframe en un vector nombrado como quiere la funcion waffle()
top5 <- structure(top5[[2]] / 10000, names = top5[[1]])

waffle(top5,
       rows = 4,
       legend_pos = "bottom",
       xlab = "1 cuadradito == 10.000 registros",
       title = "Top 5 actividades identificadas")

head(locationdf)

locationdf %>% 
  mutate(weekday = ordered(weekday, 
                           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                      "Friday", "Saturday", "Sunday"))) %>% 
  filter(activity == "ON_FOOT") %>% 
  count(weekday) %>% 
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(weekday, pct)) +
  geom_col() +
  scale_y_percent() +
  labs(x="", 
       y="Porporción de los registros capturados por Google (%)",
       title="Movimiento a pie registrado",
       subtitle="según el día de la semana") + 
  theme_ipsum(grid="Y")


locationdf %>% 
  mutate(weekday = ordered(weekday, 
                           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                      "Friday", "Saturday", "Sunday"))) %>% 
  filter(activity == "ON_FOOT") %>% 
  count(weekday, year) %>% 
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(weekday, pct)) +
  geom_col() +
  facet_wrap(~year, scales = "free") +
  labs(x="", 
       y="Porporción anual de los registros capturados por Google",
       title="Movimiento a pie registrado",
       subtitle="según el día de la semana") + 
  theme_ipsum(grid="Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y=element_blank())

cities <- read.csv('C:/Users/gmonc/Downloads/World_Cities.csv', stringsAsFactors = F) %>% 
  filter(POP > 100)

library(SearchTrees)

# Creamos un árbol de búsqueda con las posiciones definidas por las columnas de latitud y longitud 
tree <- createTree(cities, columns=c(2,1)) 

# Funcion para encontrar la ciudad mas cercanas a un punto dado

findMetro <- function(lat, lon, tree, cities) {
  return(cities[knnLookup(tree, lat, lon, k=1), c("CITY_NAME", "CNTRY_NAME")])
}


# Encontrar el area metropolitana para cada registro

locationdf <- cbind(locationdf,
                    map2_df(locationdf$lat, locationdf$lon, 
                            findMetro, tree = tree, cities = cities))

head(locationdf)

runs <- rle(locationdf$CITY_NAME)
estadias <- locationdf %>% 
  mutate(run_id = rep(seq_along(runs$lengths), runs$lengths)) %>% 
  group_by(CITY_NAME, CNTRY_NAME, run_id) %>% 
  summarise(date_in = min(date),
            date_out = max(date)) %>% 
  arrange(date_in) %>% 
  select(-run_id) 

estadias

ranking <- estadias %>% 
  mutate(semanas = difftime(date_out,
                            date_in,
                            units = "weeks")) %>% 
  group_by(CITY_NAME, CNTRY_NAME) %>% 
  summarise(total_semanas = round(sum(semanas), 1)) %>% 
  arrange(desc(total_semanas))

ranking

library(rworldmap)
# Preparar la data

toMap <- joinCountryData2Map(ungroup(ranking),
                             joinCode = "NAME",
                             nameJoinColumn = "CNTRY_NAME")

# A mapear!
library(RColorBrewer)

mapCountryData(toMap, 
               nameColumnToPlot = "total_semanas",
               catMethod = "pretty",
               colourPalette = brewer.pal(5, "YlGn"), 
               oceanCol= "lightblue", 
               missingCountryCol= "grey40",
               mapTitle= "Estadía total (semanas)")


library(timeline)

estadias <- estadias %>% 
  mutate(tipo = ifelse(difftime(date_out, date_in, units = "weeks") >= 3,
                       "residencia",
                       "visita")) %>% 
  as.data.frame

timeline(filter(estadias, tipo == "residencia"),
         filter(estadias, tipo == "visita" )[c(1,3)],
         text.size = 0, group.col = "tipo", 
         event.label.method = 2, event.text.size = 2) + 
  theme_ipsum() + 
  scale_fill_ipsum(name = "Residencia") + 
  labs(x="año", 
       y="",
       title="Ciudad de residencia y ciudades visitadas",
       subtitle = "Inferencia según registros de ubicación de Google",
       caption = "cada punto representa una visita") +
  theme(axis.text.y=element_blank())


library(tidyverse)
library(ggmap)
library(forcats)
library(hrbrthemes)
library(RColorBrewer)

register_google(key = "AIzaSyDM3eTYsoIVaAdZJPMyEy9xO-pqXKryVjE")
NY  <- get_map("Central Park, NY", source = "stamen", maptype = "toner-lite", zoom = 12)

# Proyectamos el mapa
ggmap(NY) +
  # y trazamos por encima las coordenadas de cada ubicación del usuario registrada en el área
  geom_point(data = filter(locationdf, CITY_NAME == "New York"),
             aes(x = lon, y = lat, color = fct_inorder(paste(month, year))), 
             alpha = .5) +
  theme_ipsum() +
  scale_color_brewer(type = "qual", palette = "Set2") +
  labs(title="Paradero del usuario",
       color = "Fecha") 

QUI  <- get_map("Quito, Ecuador", source = "stamen", maptype = "toner-lite", zoom = 11)

ggmap(QUI) + 
  geom_point(data = filter(locationdf, CITY_NAME == "Quito"),
             aes(x = lon, y = lat), 
             alpha = .1, color = "orange") +
  theme_ipsum() +
  labs(y = "", x = "",
       title="¿Dónde vive el usuario?",
       subtitle = "versión difícil") +
  # Eliminamos las etiquetas de latitud y longitud de los ejes 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  coord_map()

#Lugar de residencia del usuario

library(hexbin)

myPalette <- colorRampPalette(brewer.pal(7,'Oranges'))

ggmap(QUI) + 
  coord_equal() +
  stat_binhex(aes(x = lon, y = lat), 
              binwidth = c(0.0075,0.0075), 
              alpha = .5, 
              data = locationdf) + 
  scale_fill_gradientn(colours=myPalette(7), name = "Cantidad de registros") + 
  theme_ipsum() +
  labs(y = "", x = "",
       title="¿Dónde vive el usuario?",
       subtitle = "versión fácil",
       fill = "Cantidad de registros") +
  # Eliminamos las etiquetas de latitud y longitud de los ejes 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


head(wDataSDEatt)

wDataSDEatt %>% filter(Major == 'SigmaX')

wDataSDEatt %>% group_by(Major) %>% summarise(n())


library(VIM)

head(xDataTld0)
str(xDataTld0)

names(xDataTld0)

yDataTld0 = xDataTld0 %>% select(dateTimeLine, latitude, longitude, accuracy, verticalAccuracy, altitude, velocity, heading)

head(yDataTld0)

summary(aggr(yDataTld0, only.miss = TRUE, sortVars = FALSE, plot = TRUE))

str(yDataTld0)

yDataTld0Act = xDataTld0 %>% select(dateTimeLine, latitude, longitude, accuracy, dateActivity, activity1, confidence1)

head(yDataTld0Act)

summary(aggr(yDataTld0Act, only.miss = TRUE, sortVars = FALSE, plot = TRUE))



yDataTldTotal = dataQuitoTotal %>% select(dateTimeLine, latitude, longitude, accuracy, velocity, heading, id)

head(yDataTldTotal)


library(dplyr)
dataQuitoTotal %>% group_by(id) %>% summarise(n())


summarise(total = sum(contador))

head(dataQuitoTotal)

rlang::last_error()

summary(aggr(yDataTldTotal, only.miss = TRUE, sortVars = FALSE, plot = TRUE))

##Lugares de mayor concentración de las ubicaciones del usuario 

bin <- hexbin(x = dataTld0$latitude, y = dataTld0$longitude)

show(bin)
summary(bin)
erode(bin)



celdas1 = data.frame('celda'=bin@cell, 'n'=bin@count, 'x'=bin@xcm, 'y'=bin@ycm)

celdas1 = arrange(celdas1, desc(n))

sum(celdas1$n)

head(celdas1, 5)

xDataTld0 =NULL

head(xDataTld0)

xDataTld0 = dataTld0

head(xDataTld0)

xDataTld0 = select(xDataTld0, -contador, -id)

head(xDataTld0)


xDataTld0 = xDataTld0 %>% mutate(dayType = as.factor(ifelse((Wd %in% 2:6), "Weekday", "Weekend")))

xDataTld0 = xDataTld0 %>% mutate(dayTime = as.factor(ifelse((Ho %in% 5:11), "Morning", ifelse((Ho %in% 12:18),"Afternoon", "Night"))))

xDataTld0 = xDataTld0 %>% mutate(place = " ")


head(xDataTld0)
str(xDataTld0)


for(i in 1:nrow(xDataTld0)){
  xDataTld0$place[i] = ifelse((abs(xDataTld0$latitude[i] - celdas1$x[1]) < 0.001) & (abs(xDataTld0$longitude[i] - celdas1$y[1]) < 0.001), "Home", 
                              (ifelse((abs(xDataTld0$latitude[i] - celdas1$x[2]) < 0.001) & (abs(xDataTld0$longitude[i] - celdas1$y[2]) < 0.001), "Work/School", "Other")))   
}

xDataTld0$place = as.factor(xDataTld0$place)


head(xDataTld0)
str(xDataTld0)

head(wDataSDEatt)
str(wDataSDEatt)

head(wDataSDDatt)
str(wDataSDDatt)

head(wDataBOXatt)
str(wDataBOXatt)

head(wDataCluster)

aspace


table(xDataTld0$place)

xDataTld0WD = filter(xDataTld0, dayType == 'Weekday')

xDataTld0WE = filter(xDataTld0, dayType == 'Weekend')

xDataTld0Home = filter(xDataTld0, place == 'Home')

xDataTld0Work = filter(xDataTld0, place == 'Work/Study')

xDataTld0Other = filter(xDataTld0, place == 'Other')


binWD <- hexbin(x = xDataTld0WD$latitude, y = xDataTld0WD$longitude)
celdasWD = data.frame('celda'=binWD@cell, 'n'=binWD@count, 'x'=binWD@xcm, 'y'=binWD@ycm)
celdasWD = arrange(celdasWD, desc(n))

binWE <- hexbin(x = xDataTld0WE$latitude, y = xDataTld0WE$longitude)

celdasWE = data.frame('celda'=binWE@cell, 'n'=binWE@count, 'x'=binWE@xcm, 'y'=binWE@ycm)

celdasWE = arrange(celdasWE, desc(n))


head(celdas1)

head(celdasWD)

head(celdasWE)


dist(celdasWD)

round(abs(xDataTld0$latitude[34] - celdas1$x[1]), 4)

round(abs(xDataTld0$latitude[18165] - celdas1$x[1]), 4)

round(abs(xDataTld0$latitude[18938] - celdas1$x[1]), 4)

round(abs(xDataTld0$latitude[18945] - celdas1$x[1]), 4)

round(abs(xDataTld0$latitude[18663] - celdas1$x[1]), 4) > 0.001

round(abs(xDataTld0$latitude[18350] - celdas1$x[1]), 4) > 0.001



round(abs(xDataTld0$latitude[18341] - celdas1$x[1]), 4) > 0.001

round(abs(xDataTld0$latitude[18342] - celdas1$x[1]), 4) > 0.001

round(abs(xDataTld0$latitude[18343] - celdas1$x[1]), 4) > 0.001

round(abs(xDataTld0$latitude[18344] - celdas1$x[1]), 4) > 0.001


-0.121856 -(-0.122559) 

-78.505725 -(-78.504888)




-0.121856 -(-0.121084) 

-78.505725 -(-78.504754)


casa
-0.1218074 -78.50567


drouet
-0.1218074 - (-0.122849) < 0.001
-78.50567 - (-78.504893) < 0.001

gasolinera
-0.1218074 - (-0.121331) < 0.001
-78.50567 - (-78.505070) < 0.001

tienda
-0.1218074 - (-0.122334) < 0.001
-78.50567 - (-78.505794) < 0.001

cerca
-0.1218074 - (-0.122028) < 0.001
-78.50567 - (-78.505762) < 0.001

puente
-0.1218074 - (-0.119668) < 0.001
-78.50567 - (-78.506695) < 0.001



library(aspace)
calc_mcp(id=nameOutput, filename = paste("SDEloc_",nameOutput,"_Output.txt",sep=""), centre.xy=NULL, 
         calccentre=TRUE, weighted=FALSE, weights=NULL, points=xCoord, verbose=FALSE)


data(activities)
str(activities)
plot(activities)














# Patrones de actividad diurna entre 9 a 18 h

library(lubridate)

ggmap(QUI) + 
  coord_equal() +
  stat_binhex(data = filter(locationdf, 
                            CITY_NAME == "Quito", 
                            hour(with_tz(date, "America/Quito")) > 9 &
                              hour(with_tz(date, "America/Quito")) < 18,
                            wday(with_tz(date, "America/Quito")) %in% 2:6),
              aes(x = lon, y = lat), 
              binwidth = c(0.0075, 0.0075), 
              alpha = .7) + 
  # Aplicamos una transformación logarítmica
  scale_fill_gradientn(colours=myPalette(9), trans = "log10", name = "Location frequency") +
  theme_ipsum() +
  labs(y = "", x = "",
       title="Patrones de actividad diurna") +
  # Eliminamos las etiquetas de latitud y longitud de los ejes 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  # y eliminamos la leyenda
  guides(fill = FALSE) +
  facet_wrap(~year)  




ggmap(QUI) + 
  coord_equal() +
  stat_binhex(data = filter(locationdf, 
                            CITY_NAME == "Quito", 
                            hour(with_tz(date, "America/Quito")) > 9 &
                              hour(with_tz(date, "America/Quito")) < 18,
                            wday(with_tz(date, "America/Quito")) == 1 | 
                              wday(with_tz(date, "America/Quito")) == 7),
              aes(x = lon, y = lat), 
              binwidth = c(0.0075, 0.0075), 
              alpha = .7) + 
  # Aplicamos una transformación logarítmica
  scale_fill_gradientn(colours=myPalette(9), trans = "log10", name = "Location frequency") +
  theme_ipsum() +
  labs(y = "", x = "",
       title="Patrones de actividad diurna") +
  # Eliminamos las etiquetas de latitud y longitud de los ejes 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  # y eliminamos la leyenda
  guides(fill = FALSE) +
  facet_wrap(~year) 





###########















library(dplyr)
dataTld0 = filter(dataQuitoTotal, id=="tld0")

names(dataTld0)

xDataTld0 = dataTld0

head(xDataTld0,40)

xDataTld0 = xDataTld0 %>% mutate(dayType = ifelse((Wd == 7 | Wd == 1), "Weekend", "Weekday"))

xDataTld0 = xDataTld0 %>% mutate(dayTime = ifelse((Ho >= 5 & Ho < 12), "Morning", ifelse((Ho >= 12 & Ho < 18),"Afternoon", "Night")))










