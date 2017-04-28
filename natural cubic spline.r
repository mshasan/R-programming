# Here is our Natural Cubic Splines function named "N.spline", where, "dat1" is the data frame and "K" is the no. of knots that we use.

N.spline <- function(dat1,K){								# Initializing the function
	k <- quantile(dat1[,1],prob=c(seq(.125,.875,length.out=K)))		# calculating percentile corresponding to each knots
	N1 <- rep(1,length(dat1[,1]))							# 1st column of the basis matrix consist of only 1
	N2 <- X <- dat1[,1]								# 2nd column of the basis matrix consists of predictor's values
	
	N3 <- matrix(0,length(X),(K-2))						# matrix consists of 0 to store the remaining columns of the basis matrix
	for(i in 1:(K-2)){													# intializing a for loop to get part of the basis matrix
		d.k <- (pmax(0,(X-k[i])^3) - pmax(0,(X-tail(k,1))^3))/(tail(k,1)-k[i])				# calculating d.k(X) for (i+2)-th knot, based on only positive values
		d.K <- (pmax(0,(X-k[i+1])^3) - pmax(0,(X-(tail(k,1)-k[i+1]))^3))/(tail(k,1)-k[i+1])		# calculating d.(K-1)(X) for the (i+2)-th knot, based on only positive values
		N3[,i] <- d.k - d.K												# getting difference between d.k(X) and d.(K-1)(X)
		}															 
	basis <- cbind(N1,N2,N3)									# ultimate basis matrix
	nsmod <- lm(PreyEaten~basis,data=dat1)							# using lm() function to get Spline estimates
	plot(dat1[,1],fitted(nsmod),type="l",xlab="Predictor",ylab="Response")		# plotting fitted values provided by lm(), based on basis matrix
	points(dat1)											# plotting original points
	list(Basis.Matrix=basis, Model.Object=summary(nsmod))					# required results labeled in listing order
	}

#par(mfrow=c(2,1))							# difining how many plot would be per page
dat1 <- read.table("FuncResp.txt")				# A data set used in lecture-17
names(dat1)<-c("PreyDens","PreyEaten")			# providing names for each column of the data

N.spline(dat1=dat1,K=7)						# applying our Natural cubic Spline function


library(splines)							# to compare our result with R-builtin function initializing a new library
ns(dat1[,1],df=6)							# apply R-bultin Natural spline function
nsmod1<-lm(PreyEaten~ns(PreyDens,df=5),data=dat1)	# getting linear model's estimates
summary(nsmod1)
plot(dat1[,1],fitted(nsmod1),type="l",xlab="Predictor",ylab="Response")			# plotting data provided by R-builting natural fun.
points(dat1)												# plotting original points

# our fuction and R-builtin function both give almost same results. So, we can say our function is working well.
