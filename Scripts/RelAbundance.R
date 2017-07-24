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
## create mode (central tendency) function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

##extract raster values by polgon (UI) and calculate mode
r.vals <- extract(lcover.UI, UI.proj)
r.vals <- lapply(r.vals, function(x) x[!is.na(x)])
r.mode <-  lapply(r.vals, FUN=Mode)
r.mode <- as.numeric(r.mode)

#find relative abundance by area of landcover type
##assign mode land cover value to each UI polygon
r.uniq <- unique(r.mode)
r.area <- sapply(r.uniq, function(x) sum(values(lcover.UI) == x, na.rm = T))
r.rel <- r.area/sum(r.area)
r.invabun <- 1-r.rel
r.abun <- data.frame(lcover.mode=r.uniq, RelAbundance=r.rel, score.lcover=r.invabun)

#assign score inversely proportional to relative abundance of cover type
UIlist <- data.frame(r.mode, ID=UI@data$ID)
UIlist <- merge(UIlist, r.abun, by.x="r.mode", by.y="lcover.mode")
UIlist <- UIlist[order(UIlist$ID),]
UI@data$score.RelAbundance <- UIlist$score.lcover

# Plot Landcover types and UI
plot(Fire.proj, main= "Relative abundance of primary cover type")
plot(UI.proj, col=r.mode, border=r.mode, add=T)
l1 <- paste("L. Cover ID", unique(r.mode))
l2 <- unique(r.mode)
legend("topleft", title= "Primary Land Cover ID", legend = l1, fill = l2, cex = 0.7)

#Cleanup intermediates
rm(lcover.WA, lcover.OR, lcover.ID, Fire.proj, lcover.fire, lcover.UI, lcover.freq, lcover.sort, Mode,
   r.vals, r.mode, r.uniq, r.area, r.rel, r.invabun, r.abun, UIlist, UI.proj, l1, l2)
