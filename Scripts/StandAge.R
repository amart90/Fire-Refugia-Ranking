# Load Stand age raster
StAge <- raster("Datasets/Stand Age/StandAge.clip.tif")

# Transform projection
Fire.proj <- spTransform(FirePerim, projection(StAge))
UI.proj <- spTransform(UI, projection(StAge))

# Clip to fire extent
StAge <- crop(StAge, extent( Fire.proj))

# Mask stand age to UIs
StAge.UI <- mask(StAge, UI.proj)

# Extract mean stand age values for each UI
r <- extract(StAge.UI, UI.proj, small=T, fun=mean, na.rm=T)
r[is.na(r)] <- 0

# Calculate scoring function
f.age <- function(x){
  1/(1+50*exp(-0.025*x))
}
score.age <- f.age(r)
UI@data$score.age <- score.age

# Setup plot layout
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot stand age 
plot(Fire.proj, main= "Stand Age")
id <- as.numeric(as.character(UI@data$ID))
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(unique(r)))
col3 <- cbind(as.numeric(as.character(UI@data$ID)), r)
col4 <- cbind(sort(unique(r)), col2)
col5 <- merge(col3, col4, by.x="V2", by.y="V1")
col5$col2 <- as.character(col5$col2)
col6 <- col5[order(col5$V1),]
col7 <- as.character(col6$col2)
plot(UI.proj, add=T, col=col7, border=col7)
legend("topleft", title= "Mean Stand Age (yrs)", legend = c(min(r, na.rm = T), "", "", max(r, na.rm=T)), 
       fill = c("green", "yellow", "orange", "red"), cex = 0.9)

# Plot scoring function
x <- seq(0, 500, by= 1)
plot(f.age(x) ~ x, main="Scoring function", xlab="Stand age (yrs)", ylab="Score", type="n", cex.axis=0.8, cex.lab=0.9)
lines(loess(f.age(x) ~ x))

# Plot score distribution
hist(score.age, main="Distribution of Scores", xlab="Stand age score", breaks=seq(from=0, to=1, by=0.1))
