## Problem - 1_______________________
##(a)_______________

# in this problem 1st creating a data set by using x,y, and e by the given instruction

x <- seq(-10,30,1)		
e <- rnorm(41,0,70)
y <- (x-10)^2 + e 
plot(x,y,xlab="x",ylab="y",main="Simulated data")	# plottiing the data

##(b)________________

library(splines)				# initializing a library to use natural spline
data1 <- data.frame(y,x);data1		# creating a data frame
nsmod1<-lm(y~ns(x,df=5),data=data1)	# using natural spline
summary(nsmod1)
plot(x,fitted(nsmod1),type="l",xlab="Predictor",ylab="Fitted")	# plotting data provided by R-builting natural fun.
points(x,y)			# plotting points

##(c)_________________
# this probelm using kernel-sotting function and plotig fitted value and actual points

frks <- ksmooth(x,y,"normal",bandwidth=10)
plot(frks,type="l",xlab="Predictor",ylab="Fitted")
points(x,y)

##(d)_________________
# using locally linear regression function

lfr<-loess(y~x,data=data1,degree=2)
summary(lfr)
plot(lfr,type="l",xlab="Predictor",ylab="Fitted")	# plotting fitted values
points(x,y)			# plotting actual points

################################################################################
###############################################################################

## Probelm - 2___________________________
# in this problem doing a simulation to compare better process

natural <- numeric(500) 	# creating 3 vectors to store estimated values
kernel <- numeric(500)
local <- numeric(500)

for(i in 1:500){			# a for loop to do simulation
	x <- seq(-10,30,1)	#  creating data set like problem 1
	e <- rnorm(41,0,70)
	y <- (x-10)^2 + e 
	nsmod1 <- lm(y~ns(x,df=5))				# fitting natural spline regression
	frks <- ksmooth(x,y,"normal",bandwidth=10)	# performing kernel smotting
	lfr <- loess(y~x,degree=2)				# fitting local linear regression
	natural[i] <- mean((y-fitted(nsmod1))^2)		# calculating 3 types of average squareed diff. 
	kernel[i] <- mean((y-frks$y)^2)
	local[i] <- mean((y-fitted(lfr))^2)
	}
res <- cbind(natural,kernel,local)		# creating a matrix by using mean difference of simulation
apply(res,2,mean)					# calculating mean, plotting histogram to compare their performance
par(mfrow=c(2,2))
hist(natural)
hist(kernel)
hist(local)
## here we see natural spline performed well than other two


####################################################################################
###################################################################################


## Problem - 3_____________________________________

liver <- read.csv("liver.csv", header = T)	# reading the liver Expression data
bw <- read.csv("bodyweight.csv", header = F)	# reading body weight data

##(a)___________________________
liver2 <- as.matrix(liver[,9:143])			# creating a matrix from liver data

pval1 <- NULL				# a vector to store result
for(i in 1:nrow(liver2)){		# a for loop to perform correlation test over all genes
	y <- liver2[i,]			# i-th stage gene
	x <- as.numeric(bw)
	pval1 <- c(pval1, cor.test(x,y,"two.sided")$p.value)	# calculating p-value from corr.test
	}

sum(pval1 <= 1-(1-0.05)^(1/nrow(liver2)))		# no. of significant genes

#sig1 <- liver[2:nrow(liver),1][pval1 <= 1-(1-0.05)^(1/nrow(liver2))]

##(b)_______________
## in this problem we are using Dunn-Sidak family wise error rate method

FDR=0.05					
g<-nrow(liver2) 				#number of genes
rval<-(1:g)/g*FDR
spval<-sort(pval1)
(numsig<-sum(cumsum(spval<rval)==(1:g))) #This gives the number of significant genes at FDR=0.05
# calculating no. of significant genes

## (c)________________________
library(splines)						# intializing a library to use splines
pvali<-sort(pval1) 					# sorted p-values
genei<-order(pval1)					# ordered pvalues 

g <- nrow(liver2)						# total no. of genes
lambdaseq <- seq(from=0.0,to=0.99,by=0.01) 	# sequence of lambda values
pihatseq <- NULL  					# this will be the sequence of pi0 estimates
								# this loop finds the pi0 estimates for each lambda in lambdaseq:
for(lambda in lambdaseq){
	pihatseq <- c(pihatseq,sum(pval1 > lambda)/g/(1-lambda))
	}

pimod <- lm(pihatseq~ns(lambdaseq,df=7)) 		# this is the fitted spline
								# plot the spline and the pi(lambda) values:
par(cex=1)
plot(lambdaseq,fitted(pimod),type="l",ylab="pihat(lambda)",xlab="lambda", main="spline for pi0(lambda)")
points(cbind(lambdaseq,pihatseq))

pihat <- fitted(pimod)[84]  				# our final estimate for pi0 
								# next, find q-values:
qhat<-vector(length=g)  				# will contain q-values
qhat[g]<-pihat*pvali[g]  				# q-value for highest p-value

								# remaining q-values:
for(i in (g-1):1){
	qhat[i]<-min(pihat*g*pvali[i]/i,qhat[i+1])
	}

sum(qhat < 0.05)				# no. of genes signficant at FDR=0.05. 

	



	




























