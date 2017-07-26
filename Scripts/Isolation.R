# Calculate distance to fire perimeter for each UI
s.dist.u <- as.matrix(gDistance(UI, byid=T))
dist(s.dist.u) <- NA
s.min.u <- rowMins(t, na.rm = T)

# Calculate distance to nearest UI for each UI
s.min.p <- as.numeric(gDistance(UI, as(FirePerim, "SpatialLines"), byid=T))

# Calculate distance to nearest live tree edge
s.min <- as.matrix(cbind(s.min.u, s.min.p))
s.min <- rowMins(as.matrix(s.min))

# Calculate scoring function
f.isol <- function(x) {
  (1.1/ (1+9*exp(-0.01*x)))-0.1}
score.isol <- f.isol(s.min)
UI@data$score.isol <- score.isol

# Setup plot layout
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot relative isolation
plot(FirePerim, main="Relative Isolation")
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(score.isol))
col3 <- col2[rank(score.isol, ties.method = "min")]
plot(UI, add=T, col=col3, border=col3)
legend("topleft", title= "Distance to intact forest (m)", legend = c(round(min(s.min),1), "", "", round(max(s.min),1)), 
       fill = c("green", "yellow", "orange", "red"), cex = 0.9)

# Plot scoring function
x <- seq(1:500)
plot(x, f.isol(x), ylim=c(0,1), main="Scoring Function", xlab="Distance to nearest intact forest (m)", ylab="Score", type="n")
lines(loess(f.isol(x)~x))

# Plot score distribution
hist(score.isol, main="Distribution of Scores", xlab="Isolation score")


# Cleanup intermediates
rm(s.dist.u, s.min.u, s.min.p, s.min, score.isol, col1, col2, col3)

#####

