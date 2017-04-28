# Here my function "LITSreg" will perform  to get least trimed squared regression estimates, where, M is matrix( may be data set with response and predictor/s)
# or vector of response, and N is set of predictor variable/s. It allows both matrix and vectors.

LITSreg <- function(M, N){		# initializing the function
	if(missing(N)){			# if N is missing then first col. of M treat as response and remaining var. are predictor/s, otherwise N is set of pred.
		N <- M[,-1]			# extracting pred vriable/s
		M <- M[,1]			# extracting response var.
		}
	data <- as.data.frame(cbind(M,N))	# converting into data set of all variables (response and predictors)
	
	n <- length(data[,1])		# calculating no. of obs.
	p <- dim(data)[2]-1		# getting no. of predictors in the data set
	q <- round((n+p+1)/2)		# q is a parameter needed for trim least square
	
	data0 <- data[sample(1:nrow(data), q, replace=FALSE),]	# selecting a random sample of size q (arbitrary, not related with previous q, because
	B0 <- lm(data0[,1]~., data=data0)					# performing reg. model of multi/simple var

	eps <- 1							# initializing a variable
	count <- 0							# initializin another var. to count iteration
	b.new <- sum(sort((B0$fitted.values)^2)[1:q])	# calculating sum of squares of smallest q residuals

	while(eps > 0.001 & count < 5000){			# initializing a while loop, if eps > .001 & count < 5000, the loop will continue
		b.old <- b.new					# converting b.new into b.old				
		
		data1 <- data[sample(1:nrow(data), q, replace=FALSE),]	# again selecting a random sample of size q
		B1 <- lm(data1[,1]~., data=data1)					# performing reg. model of multi/simple var				
		b.new <- sum(sort((B1$fitted.values)^2)[1:q])			# calculating sum of squares of smallest q residuals			
		eps <- abs(b.old - b.new)						# calculating absolute difference of b.new and b.old
		count <- count + 1							# if absolute eps > .001 & count < 5000, it will count another step 
		}
	beta <- summary(B1)
	hyp <- ifelse(beta$coeff[,4] <=.05, "Slope = 0", "Slope != 0" )	# hypothesis and decision
	list(repetitions=count, beta=beta$coeff, Decision=hyp)		# resullts in listing order
	}

library(MASS)
attach(UScrime)					# getting a data set to check our results

x1 <- UScrime[,13]				# 1st pred.
x2 <- UScrime[,3]					# 2nd pred.
y <- UScrime[,16]					# response var.
N <- data.frame(x1,x2)
					
N <- UScrime[,3:15]				# dataset of all pred
	
dat <- data.frame(UScrime)			# data set of all variables

LITSreg(M=dat)				# using our function where M is matrix of all variables
LITSreg(M=y, N=N)				# using our function where M is response and N is predictor matrix

