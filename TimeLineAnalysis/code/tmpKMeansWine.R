data(wine, package='rattle')
head(wine)

head(wine[-1])
head(wine[1:2])

str(wine)

wine.stand <- scale(wine[-1])  # To standarize the variables

# K-Means
k.means.fit <- kmeans(wine.stand, 3) # k = 3

attributes(k.means.fit)

k.means.fit$centers

k.means.fit$cluster

hist(k.means.fit$cluster)

k.means.fit$size

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(wine.stand, nc=6) 


library(cluster)
clusplot(wine.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

clusplot(wine.stand, k.means.fit$cluster)










k=0
rawNumber = k
fileRawData = paste(rawName,rawNumber,".RData",sep="")
load(file=paste(urlRawData,fileRawData,sep = ""))

dataQuito.stand = scale(dataQuito[2:3])

wssplot(dataQuito.stand, nc=6) 


xCoord = select(dataQuito, 2:3)
numCenters = 3
numStart = 50
km_clusters <- kmeans(x = xCoord, centers = numCenters, nstart = numStart)

clusplot(dataQuito, km_clusters$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

ggplot(dataQuito, aes(x=longitude, y=latitude)) + geom_point()

ggmap(roadMapQuito) + 
  geom_point(data = dataQuito,
             aes(x = longitude[], y = latitude[]), 
             alpha = .5, 
             color="darkred", 
             size = .0001) +
  ggtitle("") + 
  xlab("Longitude") +
  ylab("Latitude")





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
  filter(POP > 100000)

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



runs <- rle(locationdf$CITY_NAME)

ggplot(locationdf, aes(locationdf$CITY_NAME)) + geom_histogram(stat = 'count')



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



###################################


library(tidyverse)
library(ggmap)
library(forcats)
library(hrbrthemes)
library(RColorBrewer)

class(locationdf$date) <- 'POSIXct'

# Bajamos un mapa centrado en el Central Park de NY
register_google(key = "AIzaSyDM3eTYsoIVaAdZJPMyEy9xO-pqXKryVjE")
NY  <- get_map("Central Park, NY", source = "stamen", maptype = "toner-lite", zoom = 12)

ggmap(NY) +
  # y trazamos por encima las coordenadas de cada ubicación del usuario registrada en el área
  geom_point(data = filter(locationdf, CITY_NAME == "New York"),
             aes(x = lon, y = lat, color = fct_inorder(paste(month, year))), 
             alpha = .5) +
  theme_ipsum() +
  scale_color_brewer(type = "qual", palette = "Set2") +
  labs(title="Paradero del usuario",
       color = "Fecha") 

library(tidyverse)
library(geosphere)

distance <- distGeo(locationdf[-nrow(locationdf), c("lat", "lon")], 
                    locationdf[-1, c("lat", "lon")])



