# Problem 1(a,b,c,d)________________________________________

# "t_test" is our function name to get t-test statistics. Here x is 1st sample, y is 2nd sample and mu is 1st sample mean, and D and sigma are Difference 
# between two population means and standard deviations respectively, "r" indicates correlation between two samples. "mu" and "D" can take can take different
# values where as "sigma" and "r" can take either TRUE or FALSE. TRUE means population variance(sigma) is equal and correlarion(r) is present, and vice-versa,
# and "md" is the mean of difference of two samples 

t_test <- function(x,y,mu=0,D=0,sigma=TRUE,r=FALSE,md=0){		# Initializing the function

	if(missing(y)){y <- x}				# If we have 1 sample it will consider 1st sample as 2nd sample, then our both samples will be same 
	dat <- data.frame(x,y)				# construct a data frame using 1st sample or both samples

		n1 <- length(dat[,1])		# no. of obs. in 1st sample
		n2 <- length(dat[,2])		# no. of obs. in 2nd sample
		M1 <- mean(dat[,1])		# Mean of 1st sample
		M2 <- mean(dat[,2])		# mean of 2nd sample
		S1 <- sd(dat[,1])			# Standard deviation of 1st sample
		S2 <- sd(dat[,2])			# Standard deviation of 2nd sample		

	if(all(dat[,1]%in%dat[,2])==TRUE){		# This will check whether both samples are equal or not. If equal then it will start one sample test.
		ifelse(mu!=0, mu<-mu, 0)		# If we set any value for "mu" then it takes that mu, otherwise mu=0
		df <- n1 - 1				# degrees of freedom for 1st or one sample test
		t <- (M1 - mu)/(S1/sqrt(n1))		# t-test formula for one sample
		print("One sample t-test")		# print the characteristics of the t-test

		print("Null Hyp: Population Mean is equal to zero") 	# Our Null hypothesis.
		}
	else	{						# Initializing the two samples t-test  
		if (sigma==TRUE & r==FALSE){		# sigma==TRUE & r==FALSE mean both samples are indep. have equal popn. var. And initiailize equal var. t-test
			ifelse(D!=0, D<-D, 0)		# If we set any value for "D" then it takes that D, otherwise D=0
			df <- (n1+n2-2)			# Calculation degrees of freedom for corresponding t-test

			Sp <- ((n1-1)*S1 + (n2-1)*S2)/(n1+n2-2)					# Pooled estimate of standard deviations from both samples
			t <- (M1 - M2 - D)/sqrt(Sp^2*(1/n1 + 1/n2))				# t-test formula for two independent samples with equal var.
			print("Two independent samples t-test with equal variance")		# print the characteristics of the t-test
			print("Null Hyp: Two population means are equal") 			# Our Null hypothesis.
			}
		else if (sigma==FALSE & r==FALSE){							# sigma==FALSE & r==FALSE mean samples are indep. and have unequal 
														# popn. var. And initiailize unequal var. test

			df <- (S1^2/n1 + S2^2/n2)^2/((S1^2/n1)^2/(n1-1) + (S2^2/n2)^2/(n2-1))	# Calculation degrees of freedom for corresponding t-test
			t <- (M1 - M2)/sqrt(S1^2/n1 + S2^2/n2)						# t-test formula for two indep. samples with unequal var.
			print("Two independent samples t-test with unequal variance")		# print the charactristics of the t-test
			print("Null Hyp: Two population means are equal") 				# Our Null hypothesis.
			}
		else	{									# initializing the correlated t-test
			ifelse(md!=0, md<-md, 0)					# If we set any value for "md" then it takes that md, otherwise md=0
			x.d <- (dat[,1]-dat[,2])					# difference between two sample's observations
			s.d <- sd(x.d)							# standard deviation of two differences
			n <- length(x.d)							# no. of observations
			df <- n-1								# degrees of freedom for corresponding t-test
			t <- (mean(x.d)-md)/(s.d/sqrt(n))				# t-test formula for two correlated samples test
			print("Two correlated samples t-test")			# print the charactristics of the t-test	
			print("Null Hyp: Population mean is equal to zero") 	# Our Null hypothesis.
			}
		}
		pval.2t <- round(2*pt(-abs(t),df),4)			# finding pvalue for two sided test
		pval.lt <- round(1-pt(-abs(t),df),4)			# finding pvalue for lower tail test
		pval.gt <- round(pt(-abs(t),df),4)				# pvalue for upper tail test
	T.D <- c(t, df)						# vector consists of t-statistic and corresponding degrees of freedom
		
	p <- c(pval.2t, pval.lt, pval.gt)								# making a vector consists of all pvalues(one tail and two tails)
	Des <- ifelse(p < .05, "Reject null Hyp.", "Failed to reject null Hyp.")	# making decisions based on pvalues
	rnames <- c("Two sided", "Less", "Greater")						# our ultimate results present as matrix consists of 3 rows and 2 columns.
	cnames <- c("Pvalue", "Decisions")								# "rnames" and "cnames" represent rows and columns names respectively
	result <- c(p,Des)												# vector consists of pvalues and decisions
	RL <- as.data.frame(matrix(result, nrow=3, ncol=2,dimnames=list(rnames, cnames)))		# our ultimate result-matrix as data.frame format
	list(Test.result=T.D[names(T.D)<-c("T.test", "Df")], Result=RL)					# outputs in listed form
	}

# To check our function the following steps are taken. First, we check our function by using a dummy data set consists of two samples indicated by x(sample1)
# and y(sample2). Then using our function "t_test()" we get some results, which are same as the results have gotten using R-bult-in function "t.test()". 
# So, our function seems okay.
	
x <- rnorm(20,2,4) 					# sample 1
y <- rnorm(20,2,4)					# sample 2

t_test(x,y)							# t-test using our function

t.test(x,y, alternative = "two.sided",mu=0,paired = F,var.equal = T)	# tests using R-built-in functions, such as 2-tailed and 1-tailed.
t.test(x,y,  alternative = "less",mu=0,paired = F,var.equal = T)
t.test(x,y,  alternative = "greater",mu=0,paired = F,var.equal = T)
