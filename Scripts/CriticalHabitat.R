# Load Critical Habitat data
CritHab <- readOGR("Datasets/Critical Habitat/CritHab.clip.shp")

# Transform projections
Fire.proj <- spTransform(FirePerim, projection(CritHab))
UI.proj <- spTransform(UI, projection(CritHab))

# Clip to fire perimeter
CritHab.perim <- crop(CritHab, Fire.proj)

# Identify UIs with Critical Habitat
CritHab.UI <- intersect(CritHab, UI.proj)

# Remove unnecessary columns
keep <- c("ID", "sciname", "listing_st")
CritHab.UI <- CritHab.UI[, names(CritHab.UI) %in% keep]

# Add score based upon type of species
h.score <- data.frame(listing_st= c("Endangered", "Threatened", "Recovery", "Proposed Endangered"),
                       score.habitat= c(1,.5,.2,.2))
h.score.ui <- merge(CritHab.UI@data, h.score, by.x= 'listing_st', by.y= 'listing_st')
a <- h.score.ui[, names(h.score.ui) %in% c("ID", "score.habitat")]
b <- merge(UI@data, a, by="ID", all=T)
b$score.habitat[is.na(b$score.habitat)] <- 0
UI@data$score.habitat <- b$score.habitat

# Setup plot layout
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot UI and Critical Habitat
plot(Fire.proj, main = "Critical Habitat")
plot(CritHab.perim, add=T, col="red")
plot(UI.proj, add=T)
plot(CritHab.UI, add=T, col="blue", border="blue")
legend("topleft", legend = c("Critical Habitat", "UI with Critical Habitat"), 
       fill = c("red", "blue"), cex = 0.9)

# Scoring function
plot(1,1, type="n", xlab="", ylab="", xlim=c(0,1), ylim=c(0,1), main="Scoring function")
text(.5,.5, labels="Not Used")

# Plot score distribution
hist(UI@data$score.habitat, main="Distribution of Scores", xlab="Critical Habitat score", breaks=seq(from=0, to=1, by=0.1))

# Cleanup intermediates
rm(CritHab, Fire.proj, UI.proj, CritHab.perim, CritHab.UI, h.score, h.score.ui, a, b, keep)

