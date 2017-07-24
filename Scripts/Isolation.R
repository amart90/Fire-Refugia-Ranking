# Calculate distance to fire perimeter for each UI
s.dist.u <- data.frame(gDistance(UI, byid=T))
s.min.u <- as.numeric(apply(s.dist.u, 1, function(x) min(x[x>0])))

# Calculate distance to nearest UI for each UI
s.min.p <- as.numeric(gDistance(UI, as(FirePerim, "SpatialLines"), byid=T))

# Calculate distance to nearest live tree edge
s.min <- data.frame(UI=s.min.u, Perim=s.min.p)
s.min <- apply(s.min, 1, FUN=min)

# Assign score proportional to relative isolation
score.isol <- s.min / max(s.min)
UI@data$score.isol <- score.isol

# Plot relative isolation
plot(FirePerim, main="Relative Isolation")
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(score.isol))
col3 <- col2[rank(score.isol, ties.method = "min")]
plot(UI, add=T, col=col3, border=col3)
legend("topleft", title= "Distance to intact forest (m)", legend = c(round(min(s.min),1), "", "", round(max(s.min),1)), 
       fill = c("green", "yellow", "orange", "red"), cex = 0.9)

# Cleanup intermediates
rm(s.dist.u, s.min.u, s.min.p, s.min, score.isol, col1, col2, col3)

