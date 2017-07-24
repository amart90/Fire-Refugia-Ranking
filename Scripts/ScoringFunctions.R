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



