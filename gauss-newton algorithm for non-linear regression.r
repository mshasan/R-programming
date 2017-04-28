# In this problem we have two functions, one(regfunc) for getting derivatives and another(mynls) for non-linear regression
# in regfunc K & r are two free parameters and N0 is fixed parameter, and also there is t, that is time or predictor.

regfunc <- function(K,r,N0,t){				# function initialization
	modfunc <- K/(1+(K-N0)/N0*exp(-r*t))		# non-linear formula for regression

	derivK <- 1/(1+(K-N0)/N0*exp(-r*t))-K*exp(-r*t)/(1+(K-N0)/N0*exp(-r*t))^2/N0			# derivatie w.r.t "K"
	derivr <- K*(K-N0)*t*exp(-r*t)/(1+(K-N0)/N0*exp(-r*t))^2/N0						# derivatie w.r.t "r"
	derivs <- cbind(derivK,derivr)								# binding two deriv. into colum-wise
	return(derivs)											# returning ultimate output
	}

# This is the non-linear function, where, "formula" is the non-linear formula, data=data set, start=initial value of the paremeters,
# fixed=fixed parameter, eps=tolerance level, and maxit=no. of allowing iiteration

mynls <- function(formula,data,start,fixed,eps,maxit){	# initialization of the function
	formula <- as.formula(formula)				# making formula into formula format if needed
	N0 <- fixed								# N0 is my fixed parameter
	t <- data[,1]							# seperating predictor variable from data
	N <- data[,2]							# seperating response variable from data
	b <- start								# initial values of the parameters
	K <- b[1]								# extaracting initial value of 1st parmeter 
	r <- b[2]								# initial value of the 2nd parameter
	res <- N - eval(formula[[3L]])				# caluculating residuals from the fitted formula
	SS.new <- sum(res**2)						# calculating residual sum of squares
	count <- 0								# initializing count variable to count no. of iteration 
	diff <- 1								# initializing another variable to get difference between old and new residuals
	while(diff > eps  & count < maxit){				# starting while-loop to get the required estimate, and will continue if satisfy the inside's conditions	
		SS.old <- SS.new						# converting new.SS into old.SS(sum of squares)
		ff <- regfunc(K,r,N0,t)					# using predefined function to get derivatives
		b <- c(b + solve(t(ff)%*%ff)%*%t(ff)%*%res)	# merging new parameters with old parameters
		K <- b[1]; r <- b[2]					# for while-loop pupose, extracting parameters again
		res <- N - eval(formula[[3L]])			# calcualting residuals
		SS.new <- sum(res**2)					# new sum of squares
		diff <- abs(SS.old - SS.new)				# calculating differnce between two SS
		count=count+1						# adding 1 with count var. if loop has to continue further
		names(b) <- names(start)				# giving predefined names to the parameters
		fit <- eval(formula[[3L]])				# getting fitted values to plot data
		}
	list(parameters=b,maxit=count,RSS=SS.new,fitted=fit)	# result in listing labeled
	}

data <- read.table("daphinapop.txt")		# a data set used in lecture-16
names(data)<-c("t","N")					# giving name to the columns of the data
attach(data)						# attaching variable with corresponding columns
								# now use our crated function

fit <- mynls(formula=N~K/(1+(K-N0)/N0*exp(-r*t)),
		data,start=c(K=500,r=0.1),fixed=29,eps=0.00001,maxit=1000);fit	

y <- fit$fitted			# seperating fitted values from our output to plot and see the trend of the regression line
plot(t,y,type="l")		# plotting fitted data into line 
points(N)				# plotting actual data points
					# it seems the data fitted well
