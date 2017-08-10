mtbs.fires <- readOGR("D:/antho/Desktop/UIdaho/Datasets/Unburned Islands/mtbs_perims_1984_2014_clp_utm_farea.shp")

Sp.df1 <- gUnionCascaded(pols, id = merge) # combine polygons with the same 'merge' value
# create SPDF using polygons and randomly assigning 1 or 2 to each in the @data df
Sp.df <- SpatialPolygonsDataFrame(Sp.df1,
                                  data.frame(z = factor(sample(1:2, length(Sp.df1), replace = TRUE)),
                                             row.names= unique(merge)))

plot(mtbs.fires)

e <- extent(mtbs.fires)
GT <- GridTopology(c(e@xmin + 15, e@ymin + 15), c(30, 30), c(ceiling((e@xmax - e@xmin) / 30), ceiling((e@ymax - e@ymin) / 30)))
SG <- SpatialGrid(GT)
projection(SG) <- projection(mtbs.fires)

o <- over(SG, mtbs.fires, returnList=TRUE)
ct <- sapply(o, length)
summary(ct)
SGDF <- SpatialGridDataFrame(SG, data=data.frame(ct=ct))
spplot(SGDF, "ct", col.regions=bpy.colors(20))

###############
library(sp)
library(rgeos)
library(raster)
library(rworldmap)

box <- readWKT("POLYGON((-180 90, 180 90, 180 -90, -180 -90, -180 90))")
proj4string(box) <- CRS("+proj=cea +datum=WGS84")
set.seed(1)
pts <- spsample(box, n=10, type="random")
pols <- gBuffer(pts, byid=TRUE, width=50) # create circle polys around each point
merge = sample(1:40, 10, replace = T) # create vector of 100 rand #s between 0-40 to merge pols on

Sp.df1 <- gUnionCascaded(pols, id = merge) # combine polygons with the same 'merge' value
# create SPDF using polygons and randomly assigning 1 or 2 to each in the @data df
Sp.df <- SpatialPolygonsDataFrame(Sp.df1,
                                  data.frame(z = factor(sample(1:2, length(Sp.df1), replace = TRUE)),
                                             row.names= unique(merge)))
Sp.df <- crop(Sp.df, box)
colors <- c(rgb(r=0, g=0, blue=220, alpha=50, max=255), rgb(r=220, g=0, b=0, alpha=50, max=255))
plot(Sp.df, col=colors)


GT <- GridTopology(c(-179.5, -89.5), c(1, 1), c(360, 180))
SG <- SpatialGrid(GT)
proj4string(SG) <- CRS("+proj=cea +datum=WGS84")


o <- over(SG, Sp.df1, returnList=TRUE)
ct <- sapply(o, length)
summary(ct)
SGDF <- SpatialGridDataFrame(SG, data=data.frame(ct=ct))
spplot(SGDF, "ct", col.regions=bpy.colors(20))


