# Create dataframe from UI
score <- data.frame(UI@data)

#insert dummy data
#score$score.a <- 1:14
#score$score.b <- c(100,12,386,35,3,87,5,7,45,43,12,57,12,56)
#score$score.d <- 3:16
#score$test.a <- 1:14
#score$test.b <- 2:15


# Weight data?

# Sum scores, relative them, and rank them
score$Total <- as.numeric(apply(score[, grepl("score", names(score))], 1, FUN=sum))
score$RelativeTotal <- round((score$Total / sum(score$Total)) * 100, 2)
score$Rank <- rank(-(score$Total))

# Add to SpatialPolygonsDataFrame
UI@data$Total <- score$Total
UI@data$RelativeTotal <- score$RelativeTotal
UI@data$Rank <- score$Rank

# Setup plot layout
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot UIs and color by rank
plot(FirePerim, main="Unburned Islands Importance rank")
col1 <- colorRampPalette(c("red", "orange", "yellow", "green"))
col2 <- col1(length(score$Rank))
col3 <- col2[rank(score$Rank, ties.method = "min")]
plot(UI, col=col3, border=col3, add=T)
legend("topleft", title= "Unburned Island Importance", legend = c("Highly important", "", "", "Less important"), 
       fill = c("red", "orange", "yellow", "green"), cex = 0.9)

# Plot top 10%
plot(FirePerim, main="Most important (10%)")
plot(UI[UI$Rank < round(length(UI@data$Total)/10),], col="red", border="red", add=T)

# Plot score distribution
hist(score$Total, main="Distribution of Final Scores", xlab="Importance score")

# Plot Distributions
# Setup plot layout
layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))
hist(UI@data$score.isol, main="Isolation", xlab="Isolation score")
hist(UI@data$score.habitat, main="Critical Habitat", xlab="Critical Habitat score", breaks=seq(from=0, to=1, by=0.1))
hist(UI@data$score.invasive, main="Invasive Species", xlab="Invasive Species score")
hist(UI@data$score.RelAbundance, main="Cover TypeAbundance", xlab="Relative Abundance score", breaks=seq(from=0, to=1, by=0.1))
hist(UI@data$score.building, main="Buildings", xlab="Building score", breaks=seq(from=0, to=1, by=0.1))
hist(UI@data$score.age, main="Stand Age", xlab="Stand age score", breaks=seq(from=0, to=1, by=0.1))


# Write to file
writeOGR(obj = UI, dsn= paste0(getwd(), "/Output"), layer = "UI.shp", driver = "ESRI Shapefile", overwrite_layer = T)
write.csv(score, file="Output/RefugiaRanking.csv", row.names=F)

# Cleanup intermediates
rm(score, col1, col2, col3)
