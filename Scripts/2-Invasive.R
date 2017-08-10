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
inv.ui <- gIntersection(ui.proj, inv, byid = T)
ui.over <- over(inv.ui, ui.proj, returnList = F)
inv.ui.area <- sapply(slot(inv.ui, "polygons"), function(i) slot(i, "area"))

inv.df <- data.frame(ID = ui.over$ID,
                     inv.area = inv.ui.area)
inv.df <- aggregate(inv.area ~ ID, data = inv.df, FUN = sum)

# Calculate proportion of each UI that is covered by invasive species
ui.inv <- ui.proj[ui.proj@data$ID %in% inv.df$ID,]
ui.area <- sapply(slot(ui.inv, "polygons"), function(i) slot(i, "area"))
inv.df$ui.area <- ui.area
inv.df$prop.inv <- inv.df$inv.area / inv.df$ui.area
inv.df <- merge(scores.df, inv.df[, c(1,4)], all = T)
inv.df$prop.inv[is.na(inv.df$prop.inv)] <- 0

# Write to DF and UI
scores.df$score.invasive <- inv.df$prop.inv
ui@data$score.invasive <- inv.df$prop.inv

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
