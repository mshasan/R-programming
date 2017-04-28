# We need to initialize "MASS" library for "fitdistr" function. 
# In his problem our function is "dist.compr" to compare the disributions by plotting Q-Q plot

library(MASS)									# initializing the library MASS

dist.compr <- function(y){							# Initialize the function by considering a vector y
	par(mfrow=c(2,2))								# setup number of picture per page (i.e. 2 rows & 2 columns)
	dist <- c("Normal", "exponential", "cauchy", "gamma")		# "dist" - character vector consists of 4 distributions' name
	n <- length(dist)								# calculating number of distributions
	fit <- list()					# setup a format to get the outputs into listed form
	
	for(i in 1:n){					# initializing a for loop
		fit <- fitdistr(y, dist[i])		# fitting distribution and getting parameters of the corresponding disribution. 
								# "dist[i]" extract the i-th value from "dist" vector

		if(i==1){dat <- rnorm(1000, fit[[1]][1], fit[[1]][2])}	# for i=1 "dat" gives 1000 random values from normal distribution."fit[[1]][1]" and 
												# " fit[[1]][2]" extract mean and sd from the parameters gotten by using "fitdistr"
		else if(i==2){dat <- rexp(1000, fit[[1]])}				# getting 1000 random exponential values,"fit[[1]]" extracting rate parameter
		else if(i==3){dat <- rcauchy(1000, fit[[1]][1], fit[[2]][2])}	# getting 1000 cauchy r.v.,fit[[1]][1] = location,fit[[2]][2] = scale
		else	{dat <- rgamma(1000, fit[[1]][1], fit[[1]][2])}			# 1000 gamm r.v., gamma r.v., fit[[1]][1] = shape, fit[[1]][2] = rate

	qqplot(sort(dat), sort(y), xlab= dist[i], ylab = "Real data", main = "Q-Q plot")   # plotting "qqplot". xlab & ylab is the labels of x and y axis 
		}												     # "sort()" to get sorted values, and "main" is name of the plots	
	}

X <- read.table("HW23.txt", header = F)[,1]		# reading the given data set from the specific directory. "read.able()[,1]" is used to get as vector
dist.compr(X)							# applying the created function

## From the plots we see that Gamma distribution fits the data well
## So, we can say the most likely distribution of the given data is Gamma dist.