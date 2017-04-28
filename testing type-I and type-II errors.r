 function for testing type I & II error from normal dist. where n is sample size, sim=no. of simulation and alpha=significance level

par(mfrow=c(2,1))					# defining plot per page, here is 2
normal.T1T2 <- function(n,sim,alpha){			# initializing the function
	dlmu <- c(seq(0,5,.1))				# vector of differences of means from two sample
	p.hat <- numeric(length(dlmu))			# constructing a vector to store simulated error probabilities
	for(mu in dlmu){				# starting a for loop to use diff. means
		p <- numeric(sim)			# constructing a vector to store simulated p-values
		for(i in 1:sim){			# another for loop for simulation
			x <- rnorm(n, 0, 1)		# x and y are random samples from normal
			y <- rnorm(n, mu, 1)
			ttest <- t.test(x, y , alternative = "two.sided")	# using R-builtin t-test to get t-statistic
			p[i] <- ttest$p.value					# p-value of the corresponding t-test
			}	
		p.hat[mu] <- mean(p < alpha)					# calculating error probabilities for means
		}

	plot(dlmu, p.hat, xlab="delmu",ylab="rejection probability ")		# plotting meandiff. vs error probabilities
	}

normal.T1T2(n=20, sim=10000, alpha=.005)					# using our function



# function for testing type I & II error from gamma dist. where n is sample size, sim=no. of simulation and alpha=significance level
# the working procedures are smame as the previous problem and the R-syntaxes also work in similar fashion. So, we can skip to explain 
# those codes what we have alredy expained before


gamma.T1T2 <- function(n,sim,alpha){
	dlshape <- c(seq(2,8,.1))							# vector of difference of shapes from two samples 
	p.hat <- numeric(length(dlshape))
	for(shape in dlshape){								# starting a for loop to use diff. shapes
		p <- numeric(sim)
		for(i in 1:sim){
			x <- rgamma(n, 2, 2)						# x and y are random samples from gamma
			y <- rgamma(n, shape, 2)
			ttest <- t.test(x, y , alternative = "two.sided")
			p[i] <- ttest$p.value
			}	
		p.hat[shape] <- mean(p < alpha)					# calculating error probabilities for shapes
		}

	plot(dlshape, p.hat, xlab="delshape",ylab="rejection probability ")		# plotting shapediff. vs error probabilities
	}

gamma.T1T2(n=20, sim=10000, alpha=.005)							# using our function

