# Load buildings
usfs.bldg <- readOGR("Datasets/Buildings/USFS.clip.shp")
nps.bldg <- readOGR("Datasets/Buildings/NPS.clip.shp")
comm <- readOGR("Datasets/Buildings/CommTwr.clip.shp")

# Transform projections
fire.proj <- spTransform(fire.perim, projection(usfs.bldg))
ui.proj <- spTransform(ui, projection(usfs.bldg))

# Test if UI contains a building
usfs.ui <- gIntersects(usfs.bldg, ui.proj, byid=T)
nps.ui <- gIntersects(nps.bldg, ui.proj, byid=T)
comm.ui <- gIntersects(comm, ui.proj, byid=T)

# Sum number of buildings
usfs.sum <- rowSums(usfs.ui)
nps.sum <- rowSums(nps.ui)
comm.sum <- rowSums(comm.ui)
bldg.sums <- usfs.sum + nps.sum + comm.sum

# Weight buildings? (comm = .5, building = 1)?

# Calculate scoring function
fBldg <- function(x){
  1/(1+30*exp(-1.7*x))
}
score.building <- fBldg(bldg.sums)
score.building[score.building == fBldg(0)] <- 0

# Assign score to UI
ui@data$score.building <- score.building

# Setup plot layout
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot UIs and Buildings
usfs.proj <- spTransform(usfs.bldg, projection(fire.perim))
nps.proj <- spTransform(nps.bldg, projection(fire.perim))
comm.proj <- spTransform(comm, projection(fire.perim))
plot(fire.perim, main = "Buildings - Values at Risk")
plot(ui, col=(score.building > 0)*4, border=(score.building>0)*4, add=T)
plot(fire.perim, add=T)
plot(usfs.proj, pch=18, col="green", add=T)
plot(nps.proj, col="pink", pch=18, add=T)
plot(comm.proj, col="orange", pch=18, add=T)
legend("topleft", 
       legend = c("USFS buildings", "NPS buildings", "Comm. towers", "UI with buildings"), 
       fill = c("green", "pink", "orange", 4), cex = 0.9)

# Plot scoring function
x <- seq(0, 10, by= 0.1)
plot(fBldg(x) ~ x, main="Scoring function", xlab="Number of buildings", 
     ylab="Score", type="n", cex.axis=0.8, cex=0.9)
lines(loess(fBldg(x) ~ x))


# Plot score distribution
hist(ui@data$score.building, main="Distribution of Scores", 
     xlab="Building score", breaks=seq(from=0, to=1, by=0.1), cex.axis=0.8, cex=0.9)


# Cleanup intermediates
rm(usfs.bldg, nps.bldg, comm, fire.proj, ui.proj, usfs.ui, nps.ui, comm.ui, 
   usfs.sum, nps.sum, comm.sum, bldg.sums, score.building, usfs.proj, nps.proj, comm.proj, x)
