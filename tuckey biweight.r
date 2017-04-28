
# our function's name is "tukey" same as tukey's biweight, y is vector, k is a constant with default value, "tol" is the tolerance level

library(MASS)										# initializing the MASS library
tukey <- function (y, k = 4.685 , tol = 1e-06){ 					# initializing the function
   	 y <- y[!is.na(y)]  								# get rid of NAs from data
   	 n <- length(y)									# number of observations
   	 mu <- median(y) 									# take median as initial guess 				
   	 s <- mad(y) 									# calculate median absolute deviation as our scale estimate
   	 if (s == 0) 
       	 stop("cannot estimate scale: MAD is zero for this sample")
	
   	 u <- (y - mu) / (k * s)		# standardizition of the data values by using mad*k instead of mean					
	 v <- rep(0, n)				# creating a null vector to store the data values
       i <- abs(u) <= 1				# indexing the values which are less than or equal to 1
   	 v[i] <- ((1 - u^2)^2)[i]		# getting modified data values by using tukey constraints, and storing those values
	 mu1 <- sum(y*v)/sum(v)  		# getting mean of modified data
       list(mean = mu, scale = s)		# showing desired output in listing order. location = mean = mu1 and scale = s
	 }
					
y <- c(-1,-2,-3,-5,1,2,3,5,6,7,10,66,5,3,2,1)		# to check our function constructing 16 arbitrary data values with one extreme point
mean(y);sd(y)						# usual mean and standard deviation
tukey(y, k=4.685, tol = 1e-06)				# using tuckey function to get mean and standrd deviation of the data set
huber(y, k=1.5, tol=1e-06)					# using huber function to compare our function

# here we see that tukey's biweight and huber give results those are close to each other and consistent, but usual mean and standard deviation
# are somehow affected by extreme value 66.
