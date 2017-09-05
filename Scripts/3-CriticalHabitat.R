# Load Critical Habitat data
hab.crit <- readOGR("Datasets/Critical Habitat/CritHab.clip.shp")

# Transform projections
hab.crit <- spTransform(hab.crit, projection(fire.perim))

# Clip to fire perimeter
hab.crit.perim <- crop(hab.crit, fire.perim)

# Identify UIs with Critical Habitat
hab.crit.ui <- intersect(hab.crit, ui.3)

# Remove unnecessary columns
keep <- c("ID", "sciname", "listing_st")
hab.crit.ui <- hab.crit.ui[, names(hab.crit.ui) %in% keep]

# Add score based upon type of species
h.score <- data.frame(listing_st= c("Endangered", "Threatened", "Recovery", "Proposed Endangered"),
                       score.habitat= c(1,.5,.2,.2))
h.score.ui <- merge(hab.crit.ui@data, h.score, by.x= 'listing_st', by.y= 'listing_st')
a <- h.score.ui[, names(h.score.ui) %in% c("ID", "score.habitat")]
b <- merge(ui@data, a, by="ID", all=T)
b$score.habitat[is.na(b$score.habitat)] <- 0

# Write score to DF and UI
ui@data$score.habitat <- b$score.habitat
scores.df$score.habitat <- b$score.habitat

# Plot UI and Critical Habitat
hab.crit.proj <- spTransform(hab.crit.perim, projection(fire.perim))
hab.crit.ui.proj <- spTransform(hab.crit.ui, projection(fire.perim))
plot(fire.perim, main = "Critical Habitat")
plot(hab.crit.proj, add=T, col="red")
plot(hab.crit.ui.proj, add=T, col="blue", border="blue")
legend("topleft", legend = c("Critical Habitat", "UI with Critical Habitat"), 
       fill = c("red", "blue"), cex = 0.9)

# Plot UI importance by critical habitat
dev.set(which = map.matrix)
col1 <- data.frame(score.habitat = c(1,.5,.2,0),
                   color = c("red", "orange", "yellow", "green"))
col2 <- merge(scores.df, col1, by = 'score.habitat')
col2 <- col2[order(col2$ID),]
col3 <- as.character(col2$color)
plot(fire.perim, main="Critical Habitat")
plot(ui, add=T, col=col3, border=col3)
dev.set(which = 2)

# Plot score distribution
hist(ui@data$score.habitat, main="Distribution of Scores", xlab="Critical Habitat score", breaks=seq(from=0, to=1, by=0.1))

# Cleanup intermediates
rm(hab.crit, hab.crit.perim, hab.crit.ui, h.score, h.score.ui, a, b, keep)

###
hab.crit.ui2 <- union(ui, hab.crit)
hab.crit.ui2 <- gIntersection(ui, hab.crit, byid=T)
writeOGR(hab.crit.ui, dsn= paste0(getwd(), "/Output"), layer = "hab.crit.perim", driver = "ESRI Shapefile", overwrite_layer = T)
ui.2 <- buffer(ui, width = 0.0, dissolve=F)
gIsValid(ui.2)
plot(ui)
plot(ui.2, col='red', border='red', add=T)
ui.2@data <- ui@data
ui.3 <- as(ui.2, 'SpatialPolygonsDataFrame')
ui.3@data <- ui@data
plot(ui.3)
ui.3@data
all.equal(ui@polygons, ui.3@polygons)
length(ui.3@data)
