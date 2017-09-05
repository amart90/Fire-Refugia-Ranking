#set up WD
setwd("C:/Users/PyroGeo/Refugia/Ranking") # school computer
#setwd("D:/Refugia/Ranking") # home computer
#setwd("C:/Users/Anthony/Refugia/Ranking") # laptop

#load libraries
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(matrixStats)
library(plyr)
library(gdistance)
library(dismo)

# Load Fire Perimeter
fire.perim <- readOGR("Datasets/Table Mountain/TableMountainComplex.shp")

# Load UI
ui <- readOGR("Datasets/Table Mountain/TableUI.shp")

# Load states
us <- getData("GADM", country="USA", level=1)

# Extract states
pnwstates <- c("Washington", "Oregon", "Idaho")
pnw <- us[match(toupper(pnwstates),toupper(us$NAME_1)),]
pnw <- spTransform(pnw, projection(fire.perim))

# Create score dataframe
scores.df <- data.frame(ID = ui@data$ID)

# Setup plot layout
plot(pnw, main = "Fire location")
plot(fire.perim, add = T, col = 'red', border = 'red')

X11()
map.matrix = dev.cur()
layout(matrix(c(1:9), 3, 3, byrow = TRUE), widths = 1, heights = 1)
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("center", title= "Number of buildings", legend = c("Low", "", "", "High"), 
       fill = c("green", "yellow", "orange", "red"))
dev.set(which = 2)

# Cleanup intermediates
rm(pnwstates, us)

# Write PNW
#writeOGR(pnw, dsn= paste0(getwd(), "/Output"), layer = "pnw", driver = "ESRI Shapefile", overwrite_layer = T)