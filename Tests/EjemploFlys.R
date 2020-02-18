
##http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/

install.packages("rworldmap")

library(rworldmap)

newmap <- getMap(resolution = "low")

plot(newmap, xlim = c(-10, 0), ylim = c(30, 70), asp = 1)






library(ggmap)
ggmap(QuitoMap12)




library(ggmap)

mapaPichincha <- geocode(c("-0.41928, -78.58442", "-0.41928, -78.23896", "0.05589, -78.23896", "0.05589, -78.58442"))


mapaPichincha

