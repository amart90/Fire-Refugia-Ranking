# Load data
#Inv.PNW <- readOGR("Datasets/Invasive/Invasive.clip.shp")
Inv <- readOGR("Datasets/Invasive/tbl/Inv.shp")
Inv.PNW <- Inv

# Transform projections
Fire.proj <- spTransform(FirePerim, projection(Inv.PNW))
UI.proj <- spTransform(UI, projection(Inv.PNW))

# Clip data to fire perimeter
#Inv <- crop(Inv.PNW, Fire.proj)
#writeOGR(obj = Inv, dsn= paste0(getwd(), "/Datasets/Invasive/tbl"), layer = "Inv", driver = "ESRI Shapefile", overwrite_layer = T)


# Find area of total invasive species range (considering multiple species) within UI
Inv.UI <- intersect(UI.proj, Inv)
Inv.UI.area <- gArea(Inv.UI, byid = T)
Inv.UI2 <- data.frame(Inv.UI@data$ID, Inv.UI.area)
Inv.UI3 <- aggregate(Inv.UI2$Inv.UI.area, by=list(Inv.UI2$Inv.UI.data.ID), FUN=sum)

# Calculate proportion of each UI that is covered by invasive species
UI.area <- gArea(UI.proj, byid=T)
h <- merge(UI@data, Inv.UI3, by.x= "ID", by.y= "Group.1", all=T)
h <- cbind(h, UI.area)
h$x[is.na(h$x)] <- 0
Inv.prop <- h$x / h$UI.area

# Calculate scoring function
f.inv <- function(x) {
  exp(-5*x)}
score.invasive <- f.inv(Inv.prop)
UI@data$score.invasive <- score.invasive

# Setup plot layout
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot invasive species
plot(Fire.proj, main="Invasive Species Cover")
plot(Inv, add=T, col="red", border="red")
plot(UI.proj, add=T, col=4*as.numeric(Inv.prop > 0), border=4*as.numeric(Inv.prop > 0))
plot(Fire.proj, add=T)
legend("topleft", legend = c("Invasive plant species", "UI with invasive sp."), 
       fill = c("red", "blue"), cex = 0.9)

# Plot scoring function
x <- seq(0, 1.5, by=0.1)
plot(f.inv(x) ~ x, main="Scoring function", xlab="Proportion of invasive species cover", ylab="Score", type="n")
lines(loess(f.inv(x) ~ x))

# Plot score distribution
hist(score.invasive, main="Distribution of Scores", xlab="Invasive Species score")

# Cleanup intermediates
rm(Inv.PNW, Inv, Fire.proj, UI.proj, Inv.UI, Inv.UI.area, Inv.UI2, Inv.UI3, b)
