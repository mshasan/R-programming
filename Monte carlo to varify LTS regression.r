# Here, function "Monte" uses to perform a simulation to get regression estimates' efficiency, where N1 and N2 are the 15 and 100 sample size, respectively,
# and simu=number of the simulation

Monte <- function(N1, N2, simu){				# initializing the function
	Sigma <- matrix(c(10,3,3,2),2,2)			# creating a covriance matrix of the two variables to generate samples
	B <- matrix(0,simu,8)					# a null matrix to store the values as matrix form
	for(i in 1:simu){								# initializing a for loop
		x <- data.frame(mvrnorm(N1, rep(0, 2), Sigma))		# generating a sample of size 15
		y <- data.frame(mvrnorm(N2, rep(0, 2), Sigma))		# generating a sample of size 100

		b.lt.N1 <- lqs(x[,1]~x[,2], data=x, method ="lts")$coef	# calculating beta coefficients by "lqs" regression of method "lts" for 15 sample 
		b.lm.N1 <- lm(x[,1]~x[,2], data=x)$coef				# calculating beta coefficients by "lm" regression for 15 sample

		b.lt.N2 <- lqs(y[,1]~y[,2], data=y, method="lts")$coef	# calculating beta coefficients by "lqs" regression of method "lts" for 100 sample
		b.lm.N2 <- lm(y[,1]~y[,2], data=y)$coef				# calculating beta coefficients by "lm" regression for 100 sample

		B[i,] <- c(b.lt.N1, b.lm.N1, b.lt.N2,  b.lm.N2)			# storing our results into the predifined null matrix as row vector
		}
	eff <- 1/apply(B,2,var)								# calculating the efficienty of each beta coefficient
	hnames <- c("Sample size 15", "Sample size 100")			# to display the result into array form creating a label of a dimension
	cnames <- c("LTSreg", "LSreg")						# column dimension of that array
	rnames <- c("Intercept", "Slope")						# row dimension of that aray

	RL <- array(eff,dim=c(2,2,2), dimnames=list(rnames, cnames, hnames))	# creating an array to show ultimate results into array form
	list(Result.of.efficiency=RL)								# showing results in listing order
	}

Monte(N1=15, N2=100, simu=10000)			# using our created Monte-carlo function for simulation

# if we observe the efficiency of the beta coefficients we see linear regression(lm) are more
# efficient than LTS regression, based on proper data, no matter what the sample size is.
