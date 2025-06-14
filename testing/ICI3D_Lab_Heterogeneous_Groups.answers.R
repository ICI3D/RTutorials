## Before

hetero <- groupSim(cbar=2, kappa=0.5) 
print(hetero, n=Inf)

print(base %+% groupSim(cbar=0.9, kappa=0))
print(base %+% groupSim(cbar=0.9, kappa=0.8))

print(base %+% groupSim(cbar=0.9, kappa=0.2, Tfinal=80))

print(base %+% groupSim(cbar=4.0, kappa=0.2))
print(base %+% groupSim(cbar=4.0, kappa=0.8))

rates <- (ggplot()
	+ aes(x=time)
	+ geom_line(aes(y=cI))
	+ geom_line(aes(y=cS), color="blue")
)
print(rates %+% groupSim(cbar=4.0, kappa=0.2))
print(rates %+% groupSim(cbar=4.0, kappa=0.8))

