library("shellpipes")
library(deSolve)
library(ggplot2)

sourceFiles()

objects()

makeGroups(n=10, m=2, kappa=1)

groupSimPlot(cbar=0.2, kappa=5.0, desc="homoLess", nGroups=10)
groupSimPlot(cbar=0.2, kappa=6.0, desc="hetMore", nGroups=10)
