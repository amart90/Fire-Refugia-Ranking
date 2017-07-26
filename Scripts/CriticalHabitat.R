# Load Critical Habitat data
hab.crit <- readOGR("Datasets/Critical Habitat/CritHab.clip.shp")

# Transform projections
fire.proj <- spTransform(fire.perim, projection(hab.crit))
ui.proj <- spTransform(ui, projection(hab.crit))

# Clip to fire perimeter
hab.crit.perim <- crop(hab.crit, fire.proj)

# Identify UIs with Critical Habitat
hab.crit.ui <- intersect(hab.crit, ui.proj)

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
ui@data$score.habitat <- b$score.habitat

# Setup plot layout
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot UI and Critical Habitat
hab.crit.proj <- spTransform(hab.crit.perim, projection(fire.perim))
hab.crit.ui.proj <- spTransform(hab.crit.ui, projection(fire.perim))
plot(fire.perim, main = "Critical Habitat")
plot(hab.crit.proj, add=T, col="red")
plot(hab.crit.ui.proj, add=T, col="blue", border="blue")
legend("topleft", legend = c("Critical Habitat", "UI with Critical Habitat"), 
       fill = c("red", "blue"), cex = 0.9)

# Scoring function
plot(1,1, type="n", xlab="", ylab="", xlim=c(0,1), ylim=c(0,1), main="Scoring function")
text(.5,.5, labels="Not Used")

# Plot score distribution
hist(ui@data$score.habitat, main="Distribution of Scores", xlab="Critical Habitat score", breaks=seq(from=0, to=1, by=0.1))

# Cleanup intermediates
rm(hab.crit, fire.proj, ui.proj, hab.crit.perim, hab.crit.ui, h.score, h.score.ui, a, b, keep, hab.crit.proj, hab.crit.ui.proj)

