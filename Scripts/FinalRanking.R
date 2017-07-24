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

# Plot UIs and color by rank
plot(FirePerim, main="Unburned Islands Importance rank")
col1 <- colorRampPalette(c("red", "orange", "yellow", "green"))
col2 <- col1(length(score$Rank))
col3 <- col2[rank(score$Rank, ties.method = "min")]
plot(UI, col=col3, border=col3, add=T)
legend("topleft", title= "Unburned Island Importance", legend = c("Highly important", "", "", "Less important"), 
       fill = c("red", "orange", "yellow", "green"), cex = 0.9)

# Write to file
writeOGR(obj = UI, dsn= paste0(getwd(), "/Output"), layer = "UI.shp", driver = "ESRI Shapefile", overwrite_layer = T)
write.csv(score, file="Output/RefugiaRanking.csv", row.names=F)

# Cleanup intermediates
rm(score, col1, col2, col3)
