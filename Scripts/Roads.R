# Load dataset
roads.ID <- readOGR("D:/Refugia/Working/roads_ID.shp")
roads.WA <- readOGR("D:/Refugia/Working/roads_WA.shp")
roads.OR <- readOGR("D:/Refugia/Working/roads_OR.shp")

lcover.WA <- raster("Datasets/Land Cover/gaplandcov_wa/gaplndcov_WA.img")
lcover.OR <- raster("Datasets/Land Cover/gaplandcov_or/gaplndcov_OR.img")
lcover.ID <- raster("Datasets/Land Cover/gaplandcov_id/gaplndcov_ID.img")

# Transform projection
roads.WA <- spTransform(roads.WA, projection(lcover.WA))
roads.OR <- spTransform(roads.WA, projection(lcover.OR))
roads.ID <- spTransform(roads.WA, projection(lcover.ID))

# Rasterize roads
roads.r.WA <- rasterize(roads.WA, lcover.WA, field=2)
writeRaster(roads.r.WA, "D:/Refugia/Working/roadRasterWA.img", "HFA")


roads.r.ID <- rasterize(roads.ID, lcover.ID, field=2)
writeRaster(roads.r.ID, "D:/Refugia/Working/roadRasterID.img", "HFA")

roads.r.OR <- rasterize(roads.OR, lcover.OR, field=2)
writeRaster(roads.r.OR, "D:/Refugia/Working/roadRasterOR.img", "HFA")

# Save road rasters
writeRaster(roads.r.WA, "D:/Refugia/Working/roadRasterWA.img", "HFA")
writeRaster(roads.r.OR, "D:/Refugia/Working/roadRasterOR.img", "HFA")
writeRaster(roads.r.ID, "D:/Refugia/Working/roadRasterID.img", "HFA")

road_dist <- gridDistance()

!is.na(lcover)
l <- extend(lcover.WA, pnw, value=0)
writeRaster(l, "D:/Refugia/Working/lcovergird.img", "HFA")

#################
lcover <- merge(lcover.WA, lcover.OR, lcover.ID)
writeRaster(lcover, "Datasets/Land Cover/gaplndcov_PNW.img", "HFA", overwrite = TRUE)
rm(pnw,prj,ui,fire.perim)
