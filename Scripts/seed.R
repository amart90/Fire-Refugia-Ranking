perim.buff.1 <- gBuffer(fire.perim, width=-95)
perim.buff.2 <- gBuffer(fire.perim, width=150)
perim.buff <- erase(perim.buff.2, perim.buff.1)

ui.buff <- buffer(ui, width=95, dissolve=F)
buff.int <- intersect(ui.buff, ui.buff)
buff.int <- gIntersection(ui.buff, ui.buff, byid=TRUE, drop_lower_td = T)


i <- intersect(perim.buff, ui.buff)
j <- erase(ui.buff, i)
plot(j, col='green', border='green', add=T)

plot(fire.perim)
plot(perim.buff, add=T, col='blue', border='blue')
plot(ui.buff, add=T, col='blue', border='blue')
plot(buff.int, add=T, col="red", border='red')
intersect()
u <- cover(ui.buff, ui.buff)
plot(i, add=T, col='red', border='red')
o <- over()
SpatialGrid()
gIntersects()
#####################
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
plot(Sp.df, col='blue')
i <- gDifference(Sp.df, Sp.df, byid=T)
plot(i[2,], add=T, col='green')
plot(i, col='red')
n <- sapply(i, gIntersection)
