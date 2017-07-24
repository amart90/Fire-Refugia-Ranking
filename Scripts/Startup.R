#set up WD
setwd("D:/Refugia/Ranking")

#load libraries
library(sp)
library(rgeos)
library(raster)
library(rgdal)

# Default projection
prj <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Load states
us <- getData("GADM", country="USA", level=1)

# Extract states
pnwstates <- c("Washington", "Oregon", "Idaho")
PNW <- us[match(toupper(pnwstates),toupper(us$NAME_1)),]
PNW <- spTransform(PNW, prj)
plot(PNW)

# Load Fire Perimeter
FirePerim <- readOGR("Datasets/Table Mountain/TableMountainComplex.shp")

# Load UI
UI <- readOGR("Datasets/Table Mountain/TableUI.shp")

# Cleanup intermediates
rm(pnwstates, us, prj)
