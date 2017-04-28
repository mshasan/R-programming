# function for simple linear reg. where X and Y are r.v, if X is matrix then Y is missing, and two columns of X will 
# be used as response and predictor 

linreg <- function(X, Y){	# initializing the fun.
	if(missing(Y)){		# if Y is missing then matrix X's 1st and 2nd column are response and predictor var.
		Y <- X[,1]		# converting 1st col. of X into response var. 
		X <- X[,2]		# 2nd col. as predictor
		} 		
	n <- length(Y)		# no. of obs. in Y
	x <- cbind(1,X)		# constructing a matrix with 1st col. only 1 and 2nd col. pred. values
	p <- dim(x)[2]		# no. of parameter for simple reg.

	beta <- (solve(t(x)%*%x))%*%t(x)%*%Y		# using formula to get beta estimates
	y.hat <- x%*%beta				# estimated response var.
	SSX <- sum((X-mean(X))^2)			# sum of square for pred.
	MSE <- sum((Y-y.hat)^2)/(n-p)			# Mean SSE of the model
	slo.test <- beta[2]/sqrt(MSE*(1/SSX))		# wald t-test for slope whether it is equal to zero
	pval <- 2*pt(-abs(slo.test),n-p)		# corresponding t-test p-value
	
	hyp <- ifelse(pval <=.05, "Slope is different than zero", "Slope is equal to zero" )	# hypothesis and decision

	rnames <- c("intercept", "slope")		# to get the required form of result defining row of a matrix								
	cnames <- c("Estimate", "P.value")		# defining col. of that matrix
	result <- c(beta[1], beta[2],NA,pval)		# ultimate vector of results

	mymatrix <- matrix(result, nrow=2, ncol=2,dimnames=list(rnames, cnames))	# required output into matrix form
	list(Regression.Result=mymatrix,Decision=hyp)					# output into listing order.
	}

x <- rnorm(20,1,2)			# to check our result generating predictor r.s from normal
y <- rnorm(20,2,3)			# generating response r.s
dat <- cbind(y,x)			# constructing a matrix of those two variables
linreg(X = x, Y = y)			# using our function for two vectors	
linreg(X = dat)				# same function using for matrix

summary(lm(y~x))	# to check whether our result is oaky or not. we are using R-builtin function for
			# regreession and we see both results are alsmost same. So, our fnction is working properly.