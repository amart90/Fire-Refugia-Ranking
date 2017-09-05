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
fire.perim <- readOGR("Datasets/East Zone/EastZone_perimeter.shp")

# Load UI
ui <- readOGR("Datasets/East Zone/EastZone_UI.shp")

# Create score dataframe
scores.df <- data.frame(ID = ui@data$ID)

# Cleanup intermediates
rm(pnwstates, us)

#writeOGR(pnw, dsn= paste0(getwd(), "/Output"), layer = "pnw", driver = "ESRI Shapefile", overwrite_layer = T)
