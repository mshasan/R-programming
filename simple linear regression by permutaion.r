# function to ger simple linear reg. estimate or permutation estimate together
# if normal pval is FALSE, then it works on pemutation or vice-versa. where X and Y are r.v, if X is matrix then Y is missing, 
# two columns of X will be used as response and predictor respectively
# the first part of this problem is similar as the previous problem . So, we are starting explaination of our R-codes from 
# 2nd part, that is, if normal is false

linregperm <- function(X, Y, normal=FALSE){	# intializing the function and indicating "pval"=not normal
	if(missing(Y)){
		Y <- X[,1] 
		X <- X[,2]
		} 
	if (normal==TRUE){				# a conditional statement
		n <- length(Y)
		x <- cbind(1,X)
		p <- dim(x)[2]
		beta <- (solve(t(x)%*%x))%*%t(x)%*%Y
		y.hat <- x%*%beta
		SSX <- sum((X-mean(X))^2)
		MSE <- sum((Y-y.hat)^2)/(n-p)
		slo.test <- beta[2]/sqrt(MSE*(1/SSX))
		pval <- 2*pt(-abs(slo.test),n-p)
		return(pval)
		}
	if (normal==FALSE){					# another condition, for normal = FALSE, Permutation will work
		X <- (X-mean(X))/sd(X) 				# getting standardized X  and Y values values
		Y <- (Y-mean(Y))/sd(Y)				
	
		corobs <- (cor(X,Y))^2				# squared correlation of those variables		  	
		numperm <- 100000   				# number of simulation
		corperm <- numeric(numperm)			# defining numeric vector to store simulation results
	
			for(j in 1:numperm){						# starting a for loop
			corperm[j] <- (sum(sample(Y)*X))^2/(sum(Y^2)*sum(X^2))  	# using a formula for permutation test where X
			}								# is fixed but Y is permutated or suffling
		pval <- length(corperm[corperm >= corobs])/numperm  			# calculating p-values of the test		
		return(pval)								# getting ultimate result
		}
	}

x <- runif(10)					# to check our result generating predictor r.s from unifrom
y <- rgamma(10,shape=x,rate=1)			# generating response r.s from gamma
mat <- cbind(y, x)				# constructing a matrix of those two variables
linregperm(X = x, Y = y)#, normal = FALSE)	# using our function for two vectors
linregperm(X = mat)#, normal = FALSE)		# same function using for matrix	


library(lmPerm)					# to check whether our result is oaky or not. we are using R-bultin 
data <- data.frame(x,y) 			# function for permutation test. 
summary(lmp(y~x, data,  perm="Exact"))	

						# we see both results are alsmost same. So, our fnction is working well
						
#############################################################################################################################
#############################################################################################################################
