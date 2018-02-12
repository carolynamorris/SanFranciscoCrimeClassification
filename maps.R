setwd('~/Desktop/SanFranciscoCrimeClassification/')

#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('rleafmap')
#install.packages('sp')
#install.packages('rgdal')
#install.packages('maptools')
#install.packages('spatstat')
#install.packages('ggmap')
#install.packages('magrittr')
#install.packages('gridExtra')

library(dplyr)
library(ggplot2)
require(rleafmap)
library(sp)
library(rgdal)
library(maptools)
library(spatstat)
library(ggmap)
library(magrittr)
library(gridExtra)

train = read.csv('data/train.csv')
train = filter(train, Y<90)

# Static small multiple heatmaps

sf = c(lon = -122.447217, lat = 37.759036)
sf.map = get_map(location = sf, zoom = 12, color = "bw")

makeMap = function(category, city, mydata) {
  m = ggmap(city, extent = "panel", maprange=FALSE) %+% mydata + aes(X, Y) + 
    geom_density2d() + 
    stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') + 
    scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
    theme(legend.position = "none", axis.title = element_blank(), 
          text = element_text(size = 8), axis.ticks = element_blank(),
          axis.text = element_blank()) + 
    ggtitle(category)
  return(m)
}

categories.1 = as.vector(unique(train$Category)[1:20])
categories.2 = as.vector(unique(train$Category)[21:39])

gplots.1 <- lapply(categories.1, function(i) {
  d = filter(train, Category == i)
  m = makeMap(i, sf.map, d)
  m
})

gplots.2 <- lapply(categories.2, function(i) {
  d = filter(train, Category == i)
  m = makeMap(i, sf.map, d)
  m
})

jpeg('maps/CrimeMap1.jpeg')
do.call(grid.arrange, c(gplots.1, list(ncol=4)))
dev.off()

jpeg('maps/CrimeMap2.jpeg')
do.call(grid.arrange, c(gplots.2, list(ncol=4)))
dev.off()


# Interactive map of Drug/Narcotic crime occurrences in January 2015

category = 'DRUG/NARCOTIC'
yr = '2015'
mo = '01'

year = substring(train$Dates,1,4)
month = substring(train$Dates,6,7)
train$Year = year
train$Month = month
data = filter(train, Category==category, Year==yr, Month==mo)
data = na.omit(data)

# Convert the points data frame to class SpatialPointsDataFrame
coordinates(data) = ~X+Y

# Assign a coordinate system if one is missing
proj4string(data) = CRS("+proj=longlat +datum=WGS84")

stamen_bm = basemap("stamen.toner")
category = spLayer(data, stroke=FALSE)
long = mean(data$X)
lat = mean(data$Y)
writeMap(stamen_bm, category, width=600, height=700, 
         setView=c(lat, long), setZoom=12, directView='browser')

