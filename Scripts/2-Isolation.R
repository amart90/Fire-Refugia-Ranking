# Calculate distance to fire perimeter for each UI
s.dist.u <- as.matrix(gDistance(ui, byid=T))
diag(s.dist.u) <- NA
s.min.u <- rowMins(s.dist.u, na.rm = T)

# Calculate distance to nearest UI for each UI
s.min.p <- as.numeric(gDistance(ui, as(fire.perim, "SpatialLines"), byid=T))

# Calculate distance to nearest live tree edge
s.min <- as.matrix(cbind(s.min.u, s.min.p))
s.min <- rowMins(as.matrix(s.min))

# Write to UI and DF
scores.df$score.isol <- s.min
ui@data$score.isol <- s.min

# Plot relative isolation
plot(fire.perim, main="Relative Isolation")
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(scores.df$score.isol))
col3 <- col2[rank(scores.df$score.isol, ties.method = "min")]
plot(ui, add=T, col=col3, border=col3)
legend("topleft", title= "Distance to intact forest (m)", legend = c(round(min(s.min),1), "", "", round(max(s.min),1)), 
       fill = c("green", "yellow", "orange", "red"), cex = 0.9)

# Plot score distribution
hist(scores.df$score.isol, main="Distribution of Scores", xlab="Isolation score")

# Cleanup intermediates
rm(s.dist.u, s.min.u, s.min.p, s.min, col1, col2, col3)
