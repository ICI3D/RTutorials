
## flows can be inserted into existing models, to create new ones.
## here is an example inserting waning immunity, where recovered
## individuals flow back to the susceptible compartment. this 
## converts the SIR model into a SIRS model.
sirsSpec = (sirSpec
	|> mp_tmb_insert(
				default = list(omega = 0.06)  # waning rate
			, expressions = list(mp_per_capita_flow("R", "S", "omega", "waning"))
	)
)

## see https://canmod.github.io/macpan2/reference/mp_tmb_insert
## for more information on inserting flows and other expressions
## into model specifications.



## we simulate a trajectory from this SIRS model
sirsContinuousTraj = (sirsSpec
	|> mp_rk4()
	|> mp_simulator(time_steps = 100, outputs = "I")
	|> mp_trajectory()
)

## and combine the trajectories from each model 
## into a single data set, and plot it to
## compare the two models.
compareContinuousTraj = bind_rows(
		SIR = sirContinuousTraj
	, SIRS = sirsContinuousTraj
	, .id = "model"
)
(compareContinuousTraj
	|> ggplot()
	+ aes(time, value, colour = model)
	+ geom_line()
)

## what is the effect of waning?
## can you create a plot that clarifies the long-term dynamics of the SIRS model?

