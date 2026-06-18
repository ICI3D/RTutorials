## Before
## Improve your notes here!!
## This is a batch answers file! Things won't work unless you pay attention to paragraphs!
library("shellpipes")

hetero <- groupSim(cbar=2, kappa=0.5) 
print(hetero, n=Inf)

theme_set(theme_bw(base_size=18)) ## For slides
groupSimPlot(cbar=2.0, kappa=0.0) |> teeGG(desc="homoBase")
groupSimPlot(cbar=2.0, kappa=1) |> teeGG(desc="hetBase")
## First experiment is just to test the idea of experimenting
## Restore for next year
groupSimPlot(cbar=1, kappa=1, nGroups=20, Tfinal=15) |> teeGG(desc="firstExp")
## An example where we can increase epidemic size by increasing kappa
## An example where we can _decrease_ epidemic size by increasing kappa
## Empty space so we can experiment above (~60 lines)





























































groupSimPlot(cbar=1.0, kappa=0.0) |> teeGG(desc="noSpread")
groupSimPlot(cbar=1.0, kappa=1.5) |> teeGG(desc="hetSpread")

groupSimPlot(cbar=0.2, kappa=5.0, nGroups=10) |> teeGG(desc="homoLess")
groupSimPlot(cbar=0.2, kappa=6.0, nGroups=10) |> teeGG(desc="hetMore")

groupSimPlot(cbar=4.0, kappa=0.0) |> teeGG(desc="homoMore")
groupSimPlot(cbar=4.0, kappa=1.0) |> teeGG(desc="hetLess")

rates <- (ggplot()
	+ aes(x=time)
	+ geom_line(aes(y=cI))
	+ geom_line(aes(y=cS), color="blue")
)
rrange <- c(0, 4)
groupRatePlot(cbar=2.0, kappa=0.2, yrange=rrange) |> teeGG(desc="homoRates")
groupRatePlot(cbar=2.0, kappa=0.8, yrange=rrange) |> teeGG(desc="hetRates")
