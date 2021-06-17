par(mfrow=c(1, 2), cex=1.6)

m <- 8

time <- seq(0, 20, length.out=101)

for (cars in 1:5){
	plot(time, 1-pgamma(time, shape=cars, scale=m/cars)
		, main = cars
		, type = "l"
		, xlab = "Years since infection"
		, ylab = "Proportion surviving"
	)

	plot(time, dgamma(time, shape=cars, scale=m/cars)
		, main = cars
		, type = "l"
		, xlab = "Years since infection"
		, ylab = "Deaths"
	)
}
