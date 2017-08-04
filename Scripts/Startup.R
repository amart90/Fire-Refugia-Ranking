#set up WD
#setwd("C:/Users/PyroGeo/Refugia/Ranking") # school computer
#setwd("D:/Refugia/Ranking") # home computer

#load libraries
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(matrixStats)

# Default projection
prj <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Load states
us <- getData("GADM", country="USA", level=1)

# Extract states
pnwstates <- c("Washington", "Oregon", "Idaho")
pnw <- us[match(toupper(pnwstates),toupper(us$NAME_1)),]
pnw <- spTransform(pnw, prj)
plot(pnw)

# Load Fire Perimeter
fire.perim <- readOGR("Datasets/Table Mountain/TableMountainComplex.shp")

# Load UI
ui <- readOGR("Datasets/Table Mountain/TableUI.shp")

# Cleanup intermediates
rm(pnwstates, us, prj)
