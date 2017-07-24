# Isolation
# Setup 
x <- seq(1:500)

# Define function
L <- 1
A <- 6
k <- 0.01
FIso <- L/(1+A*exp(-k*x))

# Plot
plot(x, FIso)

r <- raster("D:/antho/Downloads/LC08_L1TP_045027_20170723_20170723_01_RT_TIR.jpg")
e <- extend(r, PNW, value=0)
r <- readOGR("D:/antho/Desktop/UIdaho/Datasets/Table Mountain/Beaird_e.shp")

plot(e, add=T)
plot(PNW)
q <- rasterize(r, p, fun=sum)
extend()
