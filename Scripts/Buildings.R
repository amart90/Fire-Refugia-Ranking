# Load buildings
USFSbldg <- readOGR("Datasets/Buildings/USFS.clip.shp")
NPSbldg <- readOGR("Datasets/Buildings/NPS.clip.shp")
Comm <- readOGR("Datasets/Buildings/CommTwr.clip.shp")

# Transform projections
FirePerim.proj <- spTransform(FirePerim, projection(USFSbldg))
UI.proj <- spTransform(UI, projection(USFSbldg))

# Test if UI contains a building
USFS.UI <- gIntersects(USFSbldg, UI.proj, byid=T)
NPS.UI <- gIntersects(NPSbldg, UI.proj, byid=T)
Comm.UI <- gIntersects(Comm, UI.proj, byid=T)

# Sum number of buildings
USFS.sum <- rowSums(USFS.UI)
NPS.sum <- rowSums(NPS.UI)
Comm.sum <- rowSums(Comm.UI)
bldg.sums <- USFS.sum + NPS.sum + Comm.sum

# Weight buildings? (Comm = .5, building = 1)?

# Calculate scoring function
f.bldg <- function(x){
  1/(1+30*exp(-1.7*x))
}
score.building <- f.bldg(bldg.sums)
score.building[score.building == f.bldg(0)] <- 0

# Assign score to UI
UI@data$score.building <- score.building

# Setup plot layout
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot UIs and Buildings
plot(FirePerim.proj, main = "Buildings - Values at Risk")
plot(UI.proj, col=(score.building > 0)*4, border=(score.building>0)*4, add=T)
plot(FirePerim.proj, add=T)
plot(USFSbldg, pch=18, col="green", add=T)
plot(NPSbldg, col="pink", pch=18, add=T)
plot(Comm, col="orange", pch=18, add=T)
legend("topleft", 
       legend = c("USFS buildings", "NPS buildings", "Comm. towers", "UI with buildings"), 
       fill = c("green", "pink", "orange", 4), cex = 0.9)

# Plot scoring function
x <- seq(0, 10, by= 0.1)
plot(f.bldg(x) ~ x, main="Scoring function", xlab="Number of buildings", ylab="Score", type="n", cex.axis=0.8, cex=0.9)
lines(loess(f.bldg(x) ~ x))


# Plot score distribution
hist(UI@data$score.building, main="Distribution of Scores", xlab="Building score", breaks=seq(from=0, to=1, by=0.1))


# Cleanup intermediates
rm(USFSbldg, NPSbldg, Comm, FirePerim.proj, UI.proj, USFS.UI, NPS.UI, Comm.UI, 
   USFS.sum, NPS.sum, Comm.sum, bldg.sums, score.building)
