# Load data
#inv.pnw <- readOGR("Datasets/Invasive/Invasive.clip.shp")
inv <- readOGR("Datasets/Invasive/tbl/Inv.shp")
inv.pnw <- inv

# Transform projections
fire.proj <- spTransform(fire.perim, projection(inv.pnw))
ui.proj <- spTransform(ui, projection(inv.pnw))

# Clip data to fire perimeter
#inv <- crop(inv.pnw, fire.proj)
#writeOGR(obj = inv, dsn= paste0(getwd(), "/Datasets/Invasive/tbl"), layer = "inv", driver = "ESRI Shapefile", overwrite_layer = T)


# Find area of total invasive species range (considering multiple species) within UI
inv.ui <- intersect(ui.proj, inv)
inv.ui.area <- gArea(inv.ui, byid = T)
inv.ui2 <- data.frame(inv.ui@data$ID, inv.ui.area)
inv.ui3 <- aggregate(inv.ui2$inv.ui.area, by=list(inv.ui2$inv.ui.data.ID), FUN=sum)

# Calculate proportion of each UI that is covered by invasive species
ui.area <- gArea(ui.proj, byid=T)
h <- merge(ui@data, inv.ui3, by.x= "ID", by.y= "Group.1", all=T)
h <- cbind(h, ui.area)
h$x[is.na(h$x)] <- 0
inv.prop <- h$x / h$ui.area

# Calculate scoring function
fInv <- function(x) {
  exp(-5*x)}
score.invasive <- fInv(inv.prop)
ui@data$score.invasive <- score.invasive

# Setup plot layout
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot invasive species
inv.proj <- spTransform(inv, projection(fire.perim))
plot(fire.perim, main="Invasive Species Cover")
plot(inv.proj, add=T, col="red", border="red")
plot(ui, add=T, col=4*as.numeric(inv.prop > 0), border=4*as.numeric(inv.prop > 0))
plot(fire.perim, add=T)
legend("topleft", legend = c("Invasive plant species", "UI with invasive sp."), 
       fill = c("red", "blue"), cex = 0.9)

# Plot scoring function
x <- seq(0, 1.5, by=0.1)
plot(fInv(x) ~ x, main="Scoring function", xlab="Proportion of invasive species cover", ylab="Score", type="n")
lines(loess(fInv(x) ~ x))

# Plot score distribution
hist(score.invasive, main="Distribution of Scores", xlab="Invasive Species score")

# Cleanup intermediates
rm(inv.pnw, inv, fire.proj, ui.proj, inv.ui, inv.ui.area, inv.ui2, inv.ui3, inv.proj, score.invasive, ui.area, x, inv.prop, h)
