## Before
library("shellpipes")

hetero <- groupSim(cbar=2, kappa=0.5) 
print(hetero, n=Inf)

######################################################################
## Caching the answers that were used to make the slides, but don't do this way next year

## First group is repeating the exercise; next time separate answers from lag calcs
groupSimPlot(cbar=2.0, kappa=0.0) |> teeGG(desc="homoBase")
groupSimPlot(cbar=2.0, kappa=1) |> teeGG(desc="hetBase")
groupSimPlot(cbar=1, kappa=1, nGroups=20, Tfinal=15) |> teeGG(desc="firstExp")
groupSimPlot(cbar=1.0, kappa=0.0) |> teeGG(desc="noSpread")
groupSimPlot(cbar=1.0, kappa=1.5) |> teeGG(desc="hetSpread")

groupSimPlot(cbar=0.2, kappa=5.0, nGroups=10) |> teeGG(desc="homoLess")
groupSimPlot(cbar=0.2, kappa=6.0, nGroups=10) |> teeGG(desc="hetMore")

groupSimPlot(cbar=4.0, kappa=0.0) |> teeGG(desc="hetLess")
groupSimPlot(cbar=4.0, kappa=1.0) |> teeGG(desc="homoMore")

rates <- (ggplot()
	+ aes(x=time)
	+ geom_line(aes(y=cI))
	+ geom_line(aes(y=cS), color="blue")
)
teeGG(rates %+% groupSim(cbar=2.0, kappa=0.2)) |> teeGG(desc="homoRates")
teeGG(rates %+% groupSim(cbar=2.0, kappa=0.8)) |> teeGG(desc="hetRates")
