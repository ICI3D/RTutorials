
library(odin2)

rabies_gen <- odin2::odin({
	
	initial(N) <- N0
	initial(S) <- S0
	initial(E) <- E0
	initial(I) <- I0
	initial(V) <- V0
	
	initial(new_cases, zero_every = 7) <- 0
	initial(new_vaccination, zero_every = 7) <- 0
	initial(all_deaths, zero_every = 7) <- 0
	initial(births, zero_every = 7) <- 0

	## process flows
	birth <- parameter(10000/365.25)
	beta <- parameter(0.5)
	lam <- beta * I / N
	
	tau <- parameter(0.1/365.25) # vaccination
	gam <- parameter(1/60) # progression
	mu_b <- parameter(1/(4*365.25)) # back death parameter
	mu_d <- parameter(1/3) # disease death
	eps <- parameter(1/(2*365.25)) # waning
	
	N0 <- parameter(40000)
	Efrac <- parameter(0.001)
	E0 <- round(N0*Efrac)
	I0 <- 0
	V0 <- 0
	S0 <- N0 - E0 - I0 - V0
	
	p_Sout <- 1 - exp(-(lam + tau + mu_b) * dt)
	p_Eout <- 1 - exp(-(gam + mu_b)*dt)
	p_Ideath <- 1 - exp(-(mu_b + mu_d)*dt)
	p_Vout <- 1 - exp(-(mu_b + eps)*dt)
	
	n_Sout <- Binomial(S, p_Sout)
	n_Eout <- Binomial(E, p_Eout)
	n_Vout <- Binomial(V, p_Vout)
	
	n_deathsS <- Binomial(n_Sout, mu_b/(lam + tau + mu_b))
	n_deathsE <- Binomial(n_Eout, mu_b/(gam + mu_b))
	n_deathsI <- Binomial(I, p_Ideath)
	n_deathsV <- Binomial(n_Vout, mu_b/(mu_b + eps))
	
	n_infect <- Binomial(n_Sout - n_deathsS, lam/(lam + tau))
	n_vaccine <- n_Sout - n_deathsS - n_infect

	n_wanes <- n_Vout - n_deathsV
	n_progress <- n_Eout - n_deathsE
	
	n_birth <- Poisson(birth*dt)
		
	update(N) <- N - n_deathsE - n_deathsS - n_deathsI - n_deathsV + n_birth
	update(S) <- S - n_infect - n_deathsS - n_vaccine + n_birth + n_wanes
	update(E) <- E - n_deathsE - n_progress + n_infect
	update(I) <- I - n_deathsI + n_progress
	update(V) <- V - n_deathsV - n_wanes + n_vaccine
	
	update(new_cases) <- new_cases + n_infect
	update(new_vaccination) <- new_vaccination + n_vaccine
	update(all_deaths) <- all_deaths + n_deathsI
	update(births) <- births + n_birth
	
})

rabies_sys <- dust2::dust_system_create(
	rabies_gen(),
	pars = ...,
	n_particles = 1000,
	n_groups = 100,
	seed = NULL,
	deterministic = FALSE,
	n_threads = 8
)

time <- 0:10
dust2::dust_system_set_state_initial(sys)
dust2::dust_system_set_time(sys, 0)
y <- dust2::dust_system_simulate(sys, time)
