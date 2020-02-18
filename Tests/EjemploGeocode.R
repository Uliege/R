
##http://bcb.dfci.harvard.edu/~aedin/courses/R/CDC/maps.html

install.packages("mapproj")
install.packages("ggmap")
install.packages("DeducerSpatial")

require(maps)
require(ggmap)

par(mfrow = c(2, 1))
map("usa")

map("county")

map("world", "China")
map.cities(country = "China", capitals = 2)

data(unemp)
data(county.fips)

# Plot unemployment by country
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", 
           "#980043")
head(unemp)

head(county.fips)

unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 
                                                    10, 100)))
colorsmatched <- unemp$colorBuckets[match(county.fips$fips, unemp$fips)]

map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")

# Add border around each State
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")
title("unemployment by county, 2009")

leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
legend("topright", leg.txt, horiz = TRUE, fill = colors)






violent_crimes <- subset(crime, offense != "auto theft" & offense != 
                           "theft" & offense != "burglary")

# rank violent crimes
violent_crimes$offense <- factor(violent_crimes$offense, levels = c("robbery", 
                                                                    "aggravated assault", "rape", "murder"))

# restrict to downtown
violent_crimes <- subset(violent_crimes, -95.39681 <= lon & lon <= 
                           -95.34188 & 29.73631 <= lat & lat <= 29.784)

HoustonMap <- qmap('houston', zoom = 14,color = 'bw', legend = 'topleft')

HoustonMap +geom_point(aes(x = lon, y = lat, 
                           size = offense,colour = offense), data = violent_crimes )

houston <- get_map('houston', zoom = 14) 
HoustonMap <- ggmap(houston, extent = 'device', legend = 'topleft')

HoustonMap + stat_density2d(aes(x = lon, y = lat, 
                                fill = ..level.. , alpha = ..level..),size = 2, bins = 4, 
                            data = violent_crimes, geom = 'polygon') 

HoustonMap + 
  stat_density2d(aes(x = lon, y = lat, fill = ..level.. , alpha = ..level..),
                 size = 3, 
                 bins = 4, 
                 data = violent_crimes, 
                 geom = 'polygon') +
  scale_fill_gradient('Violent\nCrime\nDensity') +
  scale_alpha(range = c(.4, .75), guide = FALSE) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))



HoustonMap +  
  stat_density2d(aes(x = lon, y = lat, alpha = ..level.., fill = ..level..),
                 size = 2, 
                 bins = 4, 
                 data = violent_crimes, 
                 geom = 'polygon') +
  scale_fill_gradient('Violent\nCrime\nDensity') +
  scale_alpha(range = c(.4, .75)) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))


