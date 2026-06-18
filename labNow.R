library("shellpipes")
library(deSolve)
library(ggplot2); theme_set(theme_minimal(base_size=14))

sourceFiles()

objects()

makeGroups(n=10, m=2, kappa=1)

groupSimPlot(cbar=0.5, kappa=1)
groupSimPlot(cbar=0.5, kappa=3)
groupSimPlot(cbar=0.5, kappa=8)

