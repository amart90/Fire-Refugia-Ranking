# Load dataset
resil <- raster("Datasets/Resilience/resil_clip.txt")

# Transform projection
fire.proj <- spTransform(fire.perim, projection(resil))
ui.proj <- spTransform(ui, projection(resil))

# Clip to fire extent
resil <- crop(resil, extent( fire.proj))

# Mask resilience to UIs
resil.ui <- mask(resil, ui.proj)

# Extract mean resilience values for each UI
ui.resil2 <- extract(resil, ui.proj, method='bilinear', small = T, fun = mean, buffer = 45, df=T)
ui.resil[is.na(ui.resil)] <- 0

# Write UI mean resilience values to each UI
scores.df$score.resil <- as.numeric(ui.resil)
ui@data$score.resil <- as.numeric(ui.resil)

# Plot resilience 
plot(fire.perim, main = "Resilience")
id <- as.numeric(as.character(ui@data$ID))
col1 <- colorRampPalette(c("red", "orange", "yellow", "green"))
col2 <- col1(length(unique(ui.resil)))
col3 <- cbind(as.numeric(as.character(ui@data$ID)), ui.resil)
col4 <- cbind(sort(unique(ui.resil)), col2)
col5 <- merge(col3, col4, by.x = "V2", by.y = "V1")
col5$col2 <- as.character(col5$col2)
col6 <- col5[order(col5$V1),]
col7 <- as.character(col6$col2)
plot(ui, add = T, col = col7, border=col7)
legend("topleft", title = "Mean Resilience", legend = c("More resilient", "", "", "Less resilient"), 
       fill = c("green", "yellow", "orange", "red"), cex = 0.9)

# Plot score distribution
hist(ui.resil, main = "Distribution of Scores", xlab = "Resilience")

# Cleanup intermediates
rm(resil, resil.ui, col1, col2, col3, col4, col5, col6, col7, fire.proj, ui.proj, id, ui.resil)

writeOGR(obj = ui, dsn = paste0(getwd(), "/Output"), layer = "2-UI", driver = "ESRI Shapefile", overwrite_layer = T)
