# Load land cover data
lcover.WA <- raster("Datasets/Land Cover/gaplandcov_wa/gaplndcov_WA.img")
lcover.OR <- raster("Datasets/Land Cover/gaplandcov_or/gaplndcov_OR.img")
lcover.ID <- raster("Datasets/Land Cover/gaplandcov_id/gaplndcov_ID.img")

# Transform projections
fire.proj <- spTransform(fire.perim, projection(lcover.WA))
ui.proj <- spTransform(ui, projection(lcover.WA))

# Clip land cover data to fire perimeter
stop("Change 'lcover.WA' to the correct state (WA, OR, ID)")
lcover.fire <- crop(lcover.WA, extent( fire.proj))
lcover.fire <- round(mask(lcover.fire, fire.proj))

# Get all land cover IDs present in UIs
lcover.ui <- mask(lcover.fire, ui.proj)
lcover.freq <- as.data.frame(freq(lcover.ui))
lcover.freq <- lcover.freq[!is.na(lcover.freq$value), ]
lcover.sort <- lcover.freq[order(lcover.freq$count), ]

# Find mode land cover ID for each UI
#   Create mode (central tendency) function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#   Extract raster values by polgon (UI) and calculate mode
r.vals <- extract(lcover.ui, ui.proj)
r.vals <- lapply(r.vals, function(x) x[!is.na(x)])
r.mode <-  lapply(r.vals, FUN=Mode)
r.mode <- as.numeric(r.mode)

# Calculate relative abundance by area of landcover type
#   Assign mode land cover value to each UI polygon
r.uniq <- unique(r.mode)
r.area <- sapply(r.uniq, function(x) sum(values(lcover.ui) == x, na.rm = T))
r.rel <- r.area/sum(r.area)
r.abun <- data.frame(lcover.mode=r.uniq, r.rel=r.rel)

# Calculate scoring function
fAbund <- function(x) {
  exp(-10 * x)}

# Assign score to each UI based on mode landcover type
ui.list <- data.frame(r.mode, ID=ui@data$ID)
ui.list <- merge(ui.list, r.abun, by.x="r.mode", by.y="lcover.mode")
ui.list <- ui.list[order(ui.list$ID), ]
score.abun <- fAbund(ui.list$r.rel)
ui@data$score.RelAbundance <- score.abun

# Setup plot layout
layout(matrix(c(1, 2, 1, 3), 2, 2, byrow = TRUE), widths = c(2,1), heights = c(1,1))

# Plot Landcover types and UI
plot(fire.perim, main= "Relative Abundance of Primary Cover Type")
col1 <- colorRampPalette(c("green", "yellow", "orange", "red"))
col2 <- col1(length(unique(ui.list$r.rel)))
col3 <- cbind(unique(ui.list$r.rel), col2)
col4 <- merge(ui.list, col3, by.x= "r.rel", by.y = "V1")
col4 <- col4[order(col4$ID),]
col5 <- as.character(col4$col2)
plot(ui, add=T, col=col5, border=col5)

legend("topleft", title= "Proportion of cover type", 
       legend = c(paste0(round(100*max(r.rel)), "%"), "", "", paste0(round(100*min(r.rel)), "%")), 
       fill = c("green", "yellow", "orange", "red"), cex = 0.9)

# Plot scoring function
x <- seq(0, 1, by= 0.01)
plot(fAbund(x) ~ x, main="Scoring function", xlab="Proportion of cover type", ylab="Score", type="n", ylim = c(0, 1))
lines(loess(fAbund(x) ~ x))


# Plot score distribution
hist(ui@data$score.RelAbundance, main="Distribution of Scores", xlab="Relative Abundance score", breaks=seq(from=0, to=1, by=0.1))

# Plot landcover types - turned off (#)
#plot(lcover.fire, breaks = unique(lcover.fire), col= rainbow(24), 
#     legend=F, axes=F, box=F, main="Landcover Types")

#Cleanup intermediates
rm(lcover.WA, lcover.OR, lcover.ID, fire.proj, lcover.fire, lcover.ui, lcover.freq, lcover.sort, Mode,
   r.vals, r.mode, r.uniq, r.area, r.rel, r.abun, ui.list, ui.proj, col1, col2, col3, col4, 
   col5, score.abun, x)