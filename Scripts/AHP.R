library(devtools)
library(data.tree)
devtools::install_github("gluc/ahp", build_vignettes = TRUE)
library(ahp)
ahp.file <- Load("test2.txt")
Calculate(ahp.file)
AnalyzeTable(ahp.file)
vignette("file-format", package = "ahp")


ahpFile <- system.file("extdata", "car.ahp", package="ahp")
cat(readChar(ahpFile, file.info(ahpFile)$size))
