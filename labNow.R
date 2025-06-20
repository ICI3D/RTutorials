library("shellpipes")
library(deSolve)
library(ggplot2); theme_set(theme_minimal(base_size=14))

sourceFiles()

objects()

makeGroups(n=10, m=2, kappa=1)

groupSimPlot(cbar=0.2, kappa=5.0) |> teeGG(desc="homoLess")
groupSimPlot(cbar=0.2, kappa=6.0) |> teeGG(desc="hetMore")

