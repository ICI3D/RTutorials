## Before
library("shellpipes")

hetero <- groupSim(cbar=2, kappa=0.5) 
print(hetero, n=Inf)

## First group is repeating the exercise; next time separate answers from lag calcs
groupSimPlot(cbar=2.0, kappa=0.0, desc="homoBase")
groupSimPlot(cbar=2.0, kappa=1, desc="hetBase")
groupSimPlot(cbar=1, kappa=1, nGroups=20, Tfinal=15, desc="firstExp")
groupSimPlot(cbar=1.0, kappa=0.0, desc="noSpread")
groupSimPlot(cbar=1.0, kappa=1.5, desc="hetSpread")

groupSimPlot(cbar=0.2, kappa=5.0, desc="homoLess", nGroups=10)
groupSimPlot(cbar=0.2, kappa=6.0, desc="hetMore", nGroups=10)

groupSimPlot(cbar=4.0, kappa=0.0, desc="hetLess")
groupSimPlot(cbar=4.0, kappa=1.0, desc="homoMore")

rates <- (ggplot()
	+ aes(x=time)
	+ geom_line(aes(y=cI))
	+ geom_line(aes(y=cS), color="blue")
)
teeGG(rates %+% groupSim(cbar=2.0, kappa=0.2), desc="homoRates")
teeGG(rates %+% groupSim(cbar=2.0, kappa=0.8), desc="hetRates")
