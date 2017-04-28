# Here, "LMSfit" is the function to construct a model based on Least meadian Squares, where data=data set, n=sample size and
# rep=replication used to get the best beta coefficients.

LMSfit <- function(data, n, rep){			# initialization of the function
	if(missing(rep)){rep <- 1000}			# set default replication 1000 if rep is not given or missing
	data <- as.matrix(data)				# make ordinary data into data frame
	y <- data[,1]					# select 1st column or response variable
	x <- cbind(1,data[,-1])				# making a matrix of predictors with 1st col. of 1 vector 

	data0 <- data[sample(1:nrow(data), n, replace=FALSE),]	# taking samples without replacement size of n
	Y <- data0[,1]								# select 1st column or response variable from this sample
	X <- cbind(1,data0[,-1])						# making a matrix of predictors with 1st col. of 1 vector 
	
	B0 <- (solve(t(X)%*%X))%*%t(X)%*%Y			# calcualting beta vecor

	eps <- 1							# initializing a value used in while-loop function
	count <- 0							# initializing a var. to count iteration
	est.new <- median((abs(y - x%*%B0))^2)		# calculating meadian of absolute residuals squares

	while(eps > 0.1 & count < rep){			# starting while-loop, it continues if eps>0 or count<rep, otherwise stop
		est.old <- est.new				# convertig est.new into est.old

		data1 <- data[sample(1:nrow(data), n, replace=FALSE),]	# again taking samples without replacement size of n
		Y <- data1[,1]								# select 1st column or response variable from this sample
		X <- cbind(1,data1[,-1])						# making a matrix of predictors with 1st col. of 1 vector
		B1 <- (solve(t(X)%*%X))%*%t(X)%*%Y					# calcualting beta vecors
		est.new <- median((abs(y - x%*%B1))^2)				# calculating meadian of absolute residuals squares
		eps <- abs(est.old - est.new)						# absolute difference between old and new estimate
		count <- count + 1							# if eps > 0.1 then it counts next step by 1
		}
	B1 <- c(B1[1],B1[2])						# creating a beta vector
	names(B1) <- c("Intercept", "Slope")			# attributing names of the beta vector elements
	plot(data[,1], data[,2] ,xlab="X", ylab="Y", lty=4)	# now ploting response vs predictor
	abline(B1)								# getting a line based on intercept and slope
	list(repetitions=count, beta=B1)				# results in listing format
	}

library(MASS)							# will use a dataset from this library named phones
phones <- data.frame(phones)					# converting ordinary data into data frame
data <- data.frame(phones[,2],phones[,1])			# makig 2nd and 1st col. as response and predictor respectively
LMSfit(data = data, n=20 , rep=5000)			# using my generated function

lqs(calls ~ year, data = phones, method = "lms")		# using R builting function to compare our function
abline(lqs(calls~year,data=phones,method="lms"),col=4)	# corresponding line of the plot

# if we see the beta estimates, our estimates are not close with R-builtin function, however, our plot line is far better 
# than the actual plot got by R- builtin model statement.

