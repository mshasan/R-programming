# The "myloess" is to perform locally linear regression, where, dat=data, 
# lambda=badthwidth and numpts=number of points of predictor.

myloess <- function(dat,lambda,numpts){		# initialization of the function
	y <- dat[,2]					# selecting response variable's data
	x <- dat[,1]					# predictor variable's data
	y.fit <- numeric(length(y))			# creating a vector to store the output of "y.fit"
	count <- 0					# creating a new variable "count" to store the parameter's values of every step
	for(x0 in c(x)){				# initialization of for loop to get estimate for each points		
		dx <- x-x0				# calculating deviation of each x from initial point
		K <- c(dnorm(dx/lambda))	# getting normal density vector after scalling the devaion term
		X <- cbind(1, dx) 		# creating a covariate matrix

		B <- solve(t(X)%*%diag(K)%*%X)%*%t(X)%*%diag(K)%*%y 	# coefficient estimate of beta
		count <- count+1							# going to the next step
		y.fit[count] <- B[1] 					# ultime fitted value
		}
	output <- cbind(dat[,1],dat[,2],y.fit)			# combining actual data with fitted values
	colnames(output) <- c("x","y","eval-y")			# giving column names of the new matrix
	plot(dat[,1],y.fit,type="l")					# plotting fitted values against x values
	points(dat)							# plotting actual points of the data
	list(result=output)					# getting ultimate result in listing format
	}

dat <- read.table("FuncResp.txt")			# a sample data used in lecture - 19				
names(dat)<-c("PreyDens","PreyEaten")		# giving name of the columns of the data

x <- dat[,1]						# extracting response variable
myloess(dat=dat,lambda=bw.SJ(x),numpts=x)		# using our actual created function

lines(loess.smooth(dat[,1],dat[,2]),col=3)		# for comparision using R-builting function

