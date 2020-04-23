
search()

getwd()



#Borrar variables que ya no se utilizarán
rm(activities, dataFile, accuracy, act1, act2, activity1, activity2, altitude, confidence1, confidence2, conf1, conf2, da, dateActivity, 
   dateTimeLine, fileRawData, fileRawData, heading, latitude, latMax, latMin, longitude, lonLef, lonRig, timestampMs, 
   urlRawData, urlSaveRawData, velocity, verticalAccuracy)






ggmap(roadMapQuito) + 
  geom_point(data = sddlocWeek,
             aes(x = sddlocWeek$x[], y = sddlocWeek$y[]), 
             alpha = .5, 
             color="darkred", 
             size = .1) +
  ggtitle("GPS Point Track Centers") + 
  xlab("Longitude") +
  ylab("Latitude")


ggmap(roadMapQuito) + 
  geom_point(data = sdelocDay,
             aes(x = sdelocDay$x[], y = sdelocDay$y[]), 
             alpha = .5, 
             color="darkred", 
             size = .0001) +
  ggtitle("GPS Point Track Centers") + 
  xlab("Longitude") +
  ylab("Latitude")




ggmap(roadMapQuito) + 
  geom_point(data = sddattWeek,
             aes(x = sddattWeek$CENTRE.x[], y = sddattWeek$CENTRE.y[]), 
             alpha = .5, 
             color="darkred", 
             size = 3) +
  ggtitle("GPS Point Track Centers") + 
  xlab("Longitude") +
  ylab("Latitude")


calc_sde(id=1,  centre.xy=NULL, 
         calccentre=TRUE, weighted=FALSE, weights=NULL, points=sddattWeek[,4:5], verbose=FALSE)

plot_sde(plotnew=TRUE, plotSDEaxes=TRUE, 
         plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
         plotpoints=TRUE, points.col='green', points.pch=1, 
         plotcentre=TRUE, centre.col='red', centre.pch=19, 
         titletxt=paste("SDE Year ","Centers",sep = ""), 
         xaxis="Longitude", yaxis="Latitude", sde.col='red', sde.lwd=2, 
         jpeg=FALSE)


calc_sdd(id=2, centre.xy=NULL, 
         calccentre=TRUE, weighted=FALSE, weights=NULL, points=sddattWeek[,4:5], verbose=FALSE)

plot_sdd(plotnew=TRUE, plothv=TRUE,
         plotweightedpts=TRUE, weightedpts.col='blue', weightedpts.pch=19, 
         plotpoints=TRUE, points.col='green', points.pch=1, 
         plotcentre=TRUE, centre.col='red', centre.pch=19, 
         titletxt=paste("SDD Year ","Centers",sep = ""),
         xaxis="Longitude", yaxis="Latitude", 
         jpeg = FALSE)













aggr(dataQuitoYear, col = c("yellow", "green", "gray"), only.miss = TRUE, sortVars = TRUE, numbers = TRUE)

aggr(dataQuitoWeek, col = c("yellow", "green", "gray"), only.miss = TRUE, sortVars = TRUE, numbers = TRUE)

aggr(dataQuitoWeekDay, col = c("yellow", "green", "gray"), only.miss = TRUE, sortVars = TRUE, numbers = TRUE)




########


calc_box(id=1, filename="BOX_Output.txt", centre.xy=NULL, calccentre=TRUE, 
         weighted=FALSE, weights=NULL, points=coordenadasLongLat, verbose=FALSE)


plot_box(plotnew=TRUE, plothv=FALSE, plotweightedpts=FALSE, 
         plotpoints=TRUE, plotcentre=TRUE, titletxt="Title", 
         xaxis="Easting (m)", yaxis="Northing (m)")





aspace


TraMineR 


TrackReconstruction




library(TraMineR)
data(mvad)

## create a state sequence object from columns 17 to 86
mvad.seq <- seqdef(mvad[,17:86])

## distribution plot by sex (male)
seqdplot(mvad.seq, group=mvad$male, border=NA)

## compute the LCS pairwise distance matrix
## among the first 10 sequences
mvad.lcs <- seqdist(mvad.seq[1:10,], method="LCS")




library(readr)


library(anytime)

library(ggplot2)



str(dataQuito)

plot1 = ggplot(dataQuito, aes(x=accuracy, y=confidence1)) 

plot1 + geom_point()  

plot1 + geom_smooth()

library(tidyr)


plot2 = ggplot(dataQuito, aes(x=longitude, y=latitude)) 

plot2 + geom_point()

plot2 + geom_smooth()


plot3 = ggplot(dataQuito, aes(activity1)) 
plot3 + geom_histogram(stat = 'count')

plot4 = ggplot(dataQuito, aes(activity2)) 
plot4 + geom_histogram(stat = 'count')



t1 = dataQuito$dateTimeLine[1]
t2 = dataQuito$dateTimeLine[2]
t3 = dataQuito$dateTimeLine[3]
t4 = dataQuito$dateTimeLine[4]

t1-t2

difftime(t2,t1, units = "sec")
difftime(t3,t2, units = "sec")
difftime(t4,t3, units = "sec")

t29 = dataQuito$dateTimeLine[29]
t30 = dataQuito$dateTimeLine[30]
t31 = dataQuito$dateTimeLine[31]

difftime(t30,t29, units = "sec")
difftime(t31,t30, units = "sec")


difftime(t1,t2, units = "min")

diff.POSIXt(dataQuito$dateTimeLine)


table(round(diff.POSIXt(dataQuito$dateTimeLine), 0)<1)


table(round(diff.POSIXt(aaa$dateTimeLine), 0))

diff.POSIXt(aaa$dateTimeLine)
diff.POSIXt(aaa$dateTimeLine)/60

Sys.Date()
today()

Sys.time()
now()

now()+days(1)
today()+days(1)

now()+ddays(1)
today()+ddays(1)

str(dataQuito)

if((dataQuito$dateTimeLine[2] - dataQuito$dateTimeLine[1]) > 30){
  print("aaa")
}



Sys.timezone()
OlsonNames()


Sys.Date()
Sys.time()
Sys.timezone()


filter

library(lubridate)


year(dataQuito$dateTimeLine) %>% table()
table(year(dataQuito$dateTimeLine))
table(wday(dataQuito$dateTimeLine, label = TRUE))

wday(dataQuito$dateTimeLine[1], label = TRUE)  #Viernes = 6
days_in_month(dataQuito$dateTimeLine[1])



plot5 = ggplot(dataQuito, aes(wday(dataQuito$dateTimeLine, label = TRUE))) 
plot5 + geom_histogram(stat = 'count')








library(tidyverse)
library(ggpubr)
set.seed(101)

# Se simulan datos aleatorios con dos dimensiones
datos <- matrix(rnorm(n = 100*2), nrow = 100, ncol = 2,
                dimnames = list(NULL,c("x", "y")))
datos <- as.data.frame(datos)

# Se determina la media que va a tener cada grupo en cada una de las dos
# dimensiones. En total 2*4 medias. Este valor se utiliza para separar
# cada grupo de los demás.
media_grupos <- matrix(rnorm(n = 8, mean = 0, sd = 4), nrow = 4, ncol = 2,
                       dimnames = list(NULL, c("media_x", "media_y")))
media_grupos <- as.data.frame(media_grupos)
media_grupos <- media_grupos %>% mutate(grupo = c("a","b","c","d"))

# Se genera un vector que asigne aleatoriamente cada observación a uno de
# los 4 grupos
datos <- datos %>% mutate(grupo = sample(x = c("a","b","c","d"),
                                         size = 100,
                                         replace = TRUE))

# Se incrementa el valor de cada observación con la media correspondiente al
# grupo asignado.
datos <- left_join(datos, media_grupos, by = "grupo")
datos <- datos %>% mutate(x = x + media_x,
                          y = y + media_y)

ggplot(data = datos, aes(x = x, y = y, color = grupo)) +
  geom_point(size = 2.5) +
  theme_bw()














### Ejemplo - Antonio Vasquez Bruzt (Arg)



mortalidad <- read.csv('https://bitsandbricks.github.io/data/mortalidad_infantil_caba_2016.csv')

ggplot(mortalidad) +
  geom_col(aes(x = factor(Comuna), y = Tasa2016))

library(sf)

comunas <- st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')

ggplot(comunas) +
  geom_sf()

ggplot(comunas) +
  geom_sf(aes(fill = comunas))

rivadavia <- st_read('https://bitsandbricks.github.io/data/avenida_rivadavia.geojson')

ggplot(comunas) +
  geom_sf(aes(fill = comunas)) +
  geom_sf(data = rivadavia, color = "white")

nueva_columna <- c("Sur", "Norte", "Sur", "Sur", "Sur", "Norte", "Sur", "Sur", 
                   "Sur", "Norte", "Norte", "Norte", "Norte", "Norte", "Norte")

nueva_columna

comunas <- mutate(comunas, ubicacion = nueva_columna)

ggplot(comunas) +
  geom_sf(aes(fill = ubicacion)) +
  geom_sf(data = rivadavia, color = "white")

mortalidad <- mutate(mortalidad, ubicación = nueva_columna)

head(mortalidad)

ggplot(comunas) +
  geom_sf(aes(fill = mortalidad$Tasa2016)) +
  geom_sf(data = rivadavia, color = "red") +
  scale_fill_distiller(palette = "Spectral")

ggplot(mortalidad) +
  geom_col(aes(x = Comuna, y = Tasa2016, fill = ubicación)) +
  labs(title = "Mortalidad infantil en la Ciudad Autónoma de Buenos Aires",
       subtitle = "Año 2016",
       y = "tasa") 



atencion_ciudadano <- read.csv("http://bitsandbricks.github.io/data/gcba_suaci_barrios.csv")

barrios_comunas <- read.csv("http://bitsandbricks.github.io/data/barrios_comunas.csv")
str(barrios_comunas)

atencion_ciudadano <- left_join(atencion_ciudadano, barrios_comunas)
str(atencion_ciudadano)

seleccion <- select(atencion_ciudadano, 2, 5)

head(seleccion)

seleccion <- select(atencion_ciudadano, RUBRO:BARRIO)

head(seleccion)

seleccion <- select(atencion_ciudadano, -RUBRO)

head(seleccion)

seleccion <- select(atencion_ciudadano, -(TIPO_PRESTACION:total))
seleccion <- select(atencion_ciudadano, -(2:5))
seleccion <- select(atencion_ciudadano, -2, -5, -1)

head(seleccion)

seleccion <- select(atencion_ciudadano, -RUBRO, -BARRIO)

head(seleccion)

seleccion <- filter(atencion_ciudadano, BARRIO == "RETIRO", PERIODO == 201401)
head(seleccion)

seleccion <- filter(atencion_ciudadano, total > 100)
head(seleccion)

seleccion <- filter(atencion_ciudadano, PERIODO == 201508,  RUBRO == "SALUD")
head(seleccion)

seleccion <- filter(atencion_ciudadano, BARRIO == "RETIRO" & BARRIO == "PALERMO")
head(seleccion)

seleccion <- filter(atencion_ciudadano, BARRIO == "RETIRO" | BARRIO == "PALERMO")
head(seleccion)

filter(atencion_ciudadano, TIPO_PRESTACION == "TRAMITE" & !(RUBRO == "REGISTRO CIVIL"))

seleccion <- filter(atencion_ciudadano, !(TIPO_PRESTACION == "DENUNCIA" & RUBRO == "SEGURIDAD E HIGIENE"))

head(seleccion)

ordenado <- arrange(atencion_ciudadano, total)

head(ordenado)

ordenado <- arrange(atencion_ciudadano, total, BARRIO)

head(ordenado)

ordenado <- arrange(atencion_ciudadano, desc(total))

head(ordenado)


circulos <- data.frame(nombre = c("Círculo 1", "Círculo 2", "Círculo 3"),
                       tamaño = c("Pequeño", "Mediano", "Grande"),
                       radio  = c(1, 3, 5))

circulos

mutate(circulos, area = 3.1416 * radio^2)

atencion_ciudadano <- mutate(atencion_ciudadano,
                             AÑO = substr(PERIODO, 1, 4),
                             MES = substr(PERIODO, 5, 6))

head(atencion_ciudadano) 


a = summarise(atencion_ciudadano, promedio = mean(total))

agrupado <- group_by(atencion_ciudadano, AÑO)

a1 = summarise(agrupado, promedio = mean(total))

a2 = summarise(agrupado, promedio = mean(total))

b = summarise(agrupado, promedio_totales = mean(total))

b = summarise(agrupado, totales = sum(total))

agrupado1 <- group_by(atencion_ciudadano, AÑO, MES)

sumario <- summarise(agrupado1, promedio = mean(total))

head(sumario)

agrupado <- group_by(atencion_ciudadano, AÑO, MES, BARRIO)

sumario <- summarise(agrupado, promedio = mean(total))

head(sumario)


solo2014 <- filter(atencion_ciudadano, AÑO == 2014)
solo2014_agrupado_barrio <- group_by(solo2014, BARRIO)
total_por_barrio_2014 <- summarise(solo2014_agrupado_barrio, total = sum(total))
total_por_barrio_2014_ordenado <- arrange(total_por_barrio_2014, desc(total))
head(total_por_barrio_2014_ordenado, 5)


a3 = atencion_ciudadano %>% 
  filter(AÑO == 2014) %>% 
  group_by(BARRIO) %>% 
  summarise(total = sum(total)) %>% 
  arrange(desc(total))

a4 <- arrange(summarise(group_by(filter(atencion_ciudadano, AÑO == 2014), BARRIO), total = sum(total)), desc(total))

a5 = atencion_ciudadano %>% 
  group_by(BARRIO) %>% 
  filter(AÑO == 2014) %>% 
  summarise(total = sum(total)) %>% 
  arrange(desc(total))






rawNumber = 160
fileRawData = paste(rawName,rawNumber,".RData",sep="")
load(file=paste(urlRawData,fileRawData,sep = ""))

dataQuito$contador = 1

Y_W = dataQuito %>% 
  group_by(Y,W) %>% 
  summarise(total = sum(contador)) %>%
  filter(total >= minSamplesWeek)


tm = filter(dataQuito, Y==2019 & W==5)



tm = dataQuito %>% 
  group_by(Y,W) %>% 
  summarise(total = sum(contador))

