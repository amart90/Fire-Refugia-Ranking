ranking <- read.csv("Output/RefugiaRanking.csv", header = TRUE)
ranking2 <- cbind(ranking$ID, ranking[ , grepl("score", names(ranking))])
n <- names(ranking2)

lines0 <- c("Version: 2.0",
            "",
            "#########################",
            "# Alternatives Section",
            "#",
            "",
            "Alternatives: &alternatives"
)

lines1 <- sapply(1:length(ranking$ID), function(x)
  c(paste0("  ",ranking2[x,1],":"),
    paste0("    ", n[2], ": ", ranking2[x, 2]),
    paste0("    ", n[3], ": ", ranking2[x, 3]),
    paste0("    ", n[4], ": ", ranking2[x, 4]),
    paste0("    ", n[5], ": ", ranking2[x, 5]),
    paste0("    ", n[6], ": ", ranking2[x, 6]))
)

lines2 <- c("",
            "#",
            "# End of Alternatives Section",
            "#####################################",
            "",
            "#####################################",
            "# Goal Section",
            "#",
            "",
            "",
            "Goal:",
            "  name: Most important refugia",
            "  description : >",
            "  author: AJM",
            "  preferences:",
            "    pairwise:",
            "      - [score.building, score.habitat, 1/9]",
            "      - [score.building, score.invasive, 1/3]",
            "      - [score.building, score.isol, 1/5]",
            "      - [score.building, score.RelAbundance, 1/9]",
            "      - [score.habitat, score.invasive, 5]",
            "      - [score.habitat, score.isol, 3]",
            "      - [score.habitat, score.RelAbundance, 5]",
            "      - [score.invasive, score.isol, 1/3]",
            "      - [score.invasive, score.RelAbundance, 3]",
            "      - [score.isol, score.RelAbundance, 1]",
            "    children:",
            "      score.building:",
            "        preferences:",
            "          pairwiseFunction:",
            "            function(a1, a2) min(9, max(1/9, a1$score.building / a2$score.building",
            "        children: *alternatives",
            "      score.habitat:",
            "        preferences:",
            "          pairwiseFunction:",
            "            function(a1, a2) min(9, max(1/9, a1$score.habitat / a2$score.habitat",
            "        children: *alternatives",
            "      score.invasive:",
            "        preferences:",
            "          pairwiseFunction:",
            "            function(a1, a2) min(9, max(1/9, a1$score.invasive / a2$score.invasive",
            "        children: *alternatives",
            "      score.isol:",
            "        preferences:",
            "          pairwiseFunction:",
            "            function(a1, a2) min(9, max(1/9, a1$score.isol / a2$score.isol",
            "        children: *alternatives",
            "      score.RelAbundance:",
            "        preferences:",
            "          pairwiseFunction:",
            "            function(a1, a2) min(9, max(1/9, a1$score.RelAbundance / a2$score.RelAbundance",
            "        children: *alternatives",
            "",
            "#",
            "# End of Goal Section",
            "#####################################"
)
            

test1 <- file("test1.txt")
writeLines(c(lines0,lines1, lines2), test1)
close(test1)



########################## testing ###############################

c(
"  preferences:",
"    pairwise:",
"      - [score.building, score.habitat, 1/9]",
"      - [score.building, score.invasive, 1/3]",
"      - [score.building, score.isol, 1/5]",
"      - [score.building, score.RelAbundance, 1/9]",
"      - [score.habitat, score.invasive, 5]",
"      - [score.habitat, score.isol, 3]",
"      - [score.habitat, score.RelAbundance, 5]",
"      - [score.invasive, score.isol, 1/3]",
"      - [score.invasive, score.RelAbundance, 3]",
"      - [score.isol, score.RelAbundance, 1]",
"    children:",
"      score.building:",
"        preferences:",
"          pairwiseFunction:",
"            PrefBuilding <- function(a1, a2){",
"              if(a1$score.building < a2$score.building) return(1/PrefBuilding(a2,a1))",
"              ratio <- a1$score.building / a2$",
"",
"            }",

"} min(9, max(1/9, a1$score.building / a2$score.building",
"        children: *alternatives",
"      score.habitat:",
"        preferences:",
"          pairwiseFunction:",
"            function(a1, a2) min(9, max(1/9, a1$score.habitat / a2$score.habitat",
"        children: *alternatives",
"      score.invasive:",
"        preferences:",
"          pairwiseFunction:",
"            function(a1, a2) min(9, max(1/9, a1$score.invasive / a2$score.invasive",
"        children: *alternatives",
"      score.isol:",
"        preferences:",
"          pairwiseFunction:",
"            function(a1, a2) min(9, max(1/9, a1$score.isol / a2$score.isol",
"        children: *alternatives",
"      score.RelAbundance:",
"        preferences:",
"          pairwiseFunction:",
"            function(a1, a2) min(9, max(1/9, a1$score.RelAbundance / a2$score.RelAbundance",
"        children: *alternatives"
)

ratio(0.9936204387768)