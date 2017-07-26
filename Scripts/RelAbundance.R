# Load land cover data
lcover.WA <- raster("Datasets/Land Cover/gaplandcov_wa/gaplndcov_WA.img")
lcover.OR <- raster("Datasets/Land Cover/gaplandcov_or/gaplndcov_OR.img")
lcover.ID <- raster("Datasets/Land Cover/gaplandcov_id/gaplndcov_ID.img")

# Transform projections
Fire.proj <- spTransform(FirePerim, projection(lcover.WA))
UI.proj <- spTransform(UI, projection(lcover.WA))

# Clip land cover data to fire perimeter
stop("Change 'lcover.WA' to the correct state (WA, OR, ID)")
lcover.fire <- crop(lcover.WA, extent( Fire.proj))
lcover.fire <- round(mask(lcover.fire, Fire.proj))

# Get all land cover IDs present in UIs
lcover.UI <- mask(lcover.fire, UI.proj)
lcover.freq <- as.data.frame(freq(lcover.UI))
lcover.freq <- lcover.freq[!is.na(lcover.freq$value),]
lcover.sort <- lcover.freq[order(lcover.freq$count),]

# Find mode land cover ID for each UI
#   Create mode (central tendency) function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#   Extract raster values by polgon (UI) and calculate mode
r.vals <- extract(lcover.UI, UI.proj)
r.vals <- lapply(r.vals, function(x) x[!is.na(x)])
r.mode <-  lapply(r.vals, FUN=Mode)
r.mode <- as.numeric(r.mode)

# Calculate relative abundance by area of landcover type
#   Assign mode land cover value to each UI polygon
r.uniq <- unique(r.mode)
r.area <- sapply(r.uniq, function(x) sum(values(lcover.UI) == x, na.rm = T))
r.rel <- r.area/sum(r.area)
r.abun <- data.frame(lcover.mode=r.uniq, r.rel=r.rel)

# Calculate scoring function
f.abun <- function(x) {
  exp(-10*x)}
score.abun <- f.abun(UIlist$r.rel)

# Assign score to each UI based on mode landcover type
UIlist <- data.frame(r.mode, ID=UI@data$ID)
UIlist <- merge(UIlist, r.abun, by.x="r.mode", by.y="lcover.mode")
UIlist <- UIlist[order(UIlist$ID),]
score.abun <- f.abun(UIlist$r.rel)
UI@data$score.RelAbundance <- score.abun

# Setup plot layout
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot Landcover types and UI
plot(Fire.proj, main= "Relative Abundance of Primary Cover Type")
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(unique(UIlist$r.rel)))
col3 <- cbind(unique(UIlist$r.rel), col2)
col4 <- merge(UIlist, col3, by.x= "r.rel", by.y = "V1")
col4 <- col4[order(col4$ID),]
col5 <- as.character(col4$col2)
plot(UI.proj, add=T, col=col5, border=col5)

legend("topleft", title= "Proportion of cover type", 
       legend = c(paste0(round(100*max(r.rel)), "%"), "", "", paste0(round(100*min(r.rel)), "%")), 
       fill = c("green", "yellow", "orange", "red"), cex = 0.9)

# Plot scoring function
x <- seq(0, 1, by= 0.01)
plot(f.abun(x) ~ x, main="Scoring function", xlab="Proportion of cover type", ylab="Score", type="n")
lines(loess(f.abun(x) ~ x))


# Plot score distribution
hist(UI@data$score.RelAbundance, main="Distribution of Scores", xlab="Relative Abundance score", breaks=seq(from=0, to=1, by=0.1))

# Plot landcover types - turned off (#)
#plot(lcover.fire, breaks = unique(lcover.fire), col= rainbow(24), 
#     legend=F, axes=F, box=F, main="Landcover Types")

#Cleanup intermediates
rm(lcover.WA, lcover.OR, lcover.ID, Fire.proj, lcover.fire, lcover.UI, lcover.freq, lcover.sort, Mode,
   r.vals, r.mode, r.uniq, r.area, r.rel, r.invabun, r.abun, UIlist, UI.proj, l1, l2)


####
# Calculate scoring function
plot(lcover.fire)
length(unique(lcover.fire))
