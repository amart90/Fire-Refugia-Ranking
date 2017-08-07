ranking <- read.csv("Output/RefugiaRanking.csv", header = TRUE)
ranking2 <- cbind(ranking$ID, ranking[ , grepl("score", names(ranking))])

sapply(1:length(ranking$ID), )

x <- 1
n <- names(ranking2)
lines <- c(paste0(ranking2[x,1],":"),
           paste0("  ", n[2], ": ", ranking2[x, 2]),
           paste0("  ", n[3], ": ", ranking2[x, 3]),
           paste0("  ", n[4], ": ", ranking2[x, 4]),
           paste0("  ", n[5], ": ", ranking2[x, 5]),
           paste0("  ", n[6], ": ", ranking2[x, 6]))



lines
test1 <- file("test1.txt")
writeLines(lines, test1)
close(test1)

# Create text
?cat
