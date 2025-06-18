## Before
library("shellpipes")

hetero <- groupSim(cbar=2, kappa=0.5) 
print(hetero, n=Inf)

groupSimPlot(cbar=1.0, kappa=0.0, desc="noSpread")
groupSimPlot(cbar=1.0, kappa=0.0, desc="hetSpread")

groupSimPlot(cbar=1.0, kappa=0.0, desc="homoLess")
groupSimPlot(cbar=1.0, kappa=0.0, desc="hetMore")

groupSimPlot(cbar=1.0, kappa=0.0, desc="hetLess")
groupSimPlot(cbar=1.0, kappa=0.0, desc="homoMore")

rates <- (ggplot()
	+ aes(x=time)
	+ geom_line(aes(y=cI))
	+ geom_line(aes(y=cS), color="blue")
)
teeGG(rates %+% groupSim(cbar=2.0, kappa=0.2), desc="homoRates")
teeGG(rates %+% groupSim(cbar=2.0, kappa=0.8), desc="hetRates")
