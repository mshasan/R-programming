# the function we created to perform cross validation in kernel amother is "ker.smth", within this function we will
# use another function "NadWat" to do kernel smothing.

NadWat <- function(x0,dat,lambda){	
	x <- dat[,1]
	y <- dat[,2]
	n <- length(y)
	xdist <- mean(diff(x)) 
	lambsc <- lambda*xdist 
	klambda <- function(x0,x,lambda){
		t <- abs(x-x0)/lambda  
		return (ifelse(abs(t) <= 1, 0.75*(1-t*t), 0))
		}
	fhat <- (klambda(x0,x,lambsc)%*%y)/sum(klambda(x0,x,lambsc))
	return(fhat)
	}

# this is our function, where, dat=data,NadWat=Dr.Pauls Schilekelman's kernel smoother function
# and lam=sequence of lamabda parameters

ker.smth <- function(dat, NadWat, lam){ 		# initialization of the function
	x <- dat[,1]					# extraction predictor variable
	y <- dat[,2]					# extracting response variable
	Ysq <- NULL						# a null vector to store for loop result
	for(lambda1 in lam){				# for loop over lambda parameter to get the estimate for each lambda
		fhatvals <- numeric(length(y))	# another vector to store fitted values
		for(i in 1:length(y)){			# another foor-loop to perform cross validation

			fhatvals[i] <- NadWat(x[i],dat[-i,],lambda1)		# getting fitted values by using cross-validation
			}
		Ysq <- c(Ysq,sum((y - fhatvals)^2))		# calculation deviation sum of squares and storing as vector element
		lambda <- lam[which(Ysq==min(Ysq))]		# getting lambda of the corresponding minimum sum of squares
		}
	list(lambda=lambda)					# result into listing format
	}



dat1<-read.table("FuncResp.txt")				# a sample data set used in lecture - 19
names(dat1)<-c("PreyDens","PreyEaten")			# giving name of the variables

a <- ker.smth(dat=dat1,NadWat,lam=c(seq(2,20,.1)));a		# using our created function 

x <- dat1[,1]								# to see the overall fit we are going to plot the fitted values
fhat <- numeric(length(x))
for(i in 1:length(x))fhat[i] <- NadWat(x[i],dat1[-i,],a$lambda)	# using our estimted lambda to get fitted values
plot(dat1[,1],fhat,type="l")							# plotting the fitted values
points(dat1)									# plotting the actual points

