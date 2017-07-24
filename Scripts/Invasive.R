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

# Score proportinately to the relative area of Invasive species cover
Inv.UI3$score.invasive <-  1 - (Inv.UI3$x / max(Inv.UI3$x))
b <- merge(UI@data, Inv.UI3, by.x="ID", by.y="Group.1", all=T)
b$score.invasive[is.na(b$score.invasive)] <- 0
UI@data$score.invasive <- b$score.invasive

# Plot invasive species
plot(Fire.proj, main="Invasive Species")
plot(Inv, add=T, col="red", border="red")
plot(UI.proj, add=T, col=4*as.numeric(UI@data$score.invasive > 0), border=4*as.numeric(UI@data$score.invasive > 0))
plot(Fire.proj, add=T)
legend("topleft", legend = c("Invasive plant species", "UI with invasive sp."), 
       fill = c("red", "blue"), cex = 0.9)

# Cleanup intermediates
rm(Inv.PNW, Inv, Fire.proj, UI.proj, Inv.UI, Inv.UI.area, Inv.UI2, Inv.UI3, b)
