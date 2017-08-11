
# Remove "score" from column names for ESRI shapefiles
names(ui@data)[grepl("score", names(ui@data))] <- gsub("score.", "", names(ui@data)[grepl("score", names(ui@data))])

# Write to shapefile
writeOGR(obj = ui, dsn= paste0(getwd(), "/Output"), layer = "2-UI", driver = "ESRI Shapefile", overwrite_layer = T)

