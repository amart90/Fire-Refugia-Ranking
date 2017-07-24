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

# Calculate score
score.building <- bldg.sums / ifelse(max(bldg.sums) > 0, max(bldg.sums), 1)

# Assign score to UI (0 or 1, is building present)
UI@data$score.building <- score.building

# Plot UIs and Buildings
plot(FirePerim.proj, main = "Buildings")
plot(UI.proj, col=4*bldg.sums, border=(score.building>0)*4, add=T)
plot(FirePerim.proj, add=T)
plot(USFSbldg, pch=18, col="green", add=T)
plot(NPSbldg, col="pink", pch=18, add=T)
plot(Comm, col="orange", pch=18, add=T)
legend("topleft", legend = c("USFS buildings", "NPS buildings", "Comm. towers", "UI with buildings"), 
       fill = c("green", "pink", "orange", 4), cex = 0.9)

# Cleanup intermediates
rm(USFSbldg, NPSbldg, Comm, FirePerim.proj, UI.proj, USFS.UI, NPS.UI, Comm.UI, 
   USFS.sum, NPS.sum, Comm.sum, bldg.sums, score.building)
