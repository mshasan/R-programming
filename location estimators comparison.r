# our functions's name is 'compare'to compare the different estimators.
# here rep=number of replications,n=number of different 'e' values, est=number of estimators, N=number normal observations, and mu=mean of normal distribution 

compare <- function(rep, n, est, N, mu){		# initializing the function
	mn <- matrix(0, n, est)				# creating a null matrix to store the mean estimates for each 'e'
	vr <- matrix(0, n, est)				# creating a null matrix to store the variance estimates for each 'e'
	e <- 0						# initializing the value of 'e' as 0
	for(j in 1:n){				# starting for loop to get estimates for each 'e'
		e <- e + .01			# increases the value of 'e' by .01 in every step

		diff <- matrix(0, rep, est)	# creating another null matrix to store the difference between 'true' location and produced values for every replication  
		for(i in 1:rep){			# starting another for loop to get different estimates for every replication

			x <- c(rnorm(round(N*(1-e))),rnorm(round(N*e),mean=mu))	# random data generated from mixed normal distributions
			xbar <- mean(x)								# usual mean of the data
			md <- median(x)								# usual median
			tmu <- mean(x, trim = 0.3)						# trimmed mean
			hbr <- huber(x, k = 1.5, tol = 1e-06)				# calculating huber estimate
			tuky <- tukey(x, k = 4.685, tol = 1e-06)				# calculating tukey biweight estimate
			diff[i,] <- c(0-xbar,0-md,0-tmu,0-hbr$mu,0-tuky$mean)		# combining all location estimtes as vector, and storing in the 'diff' matrix as row
			}

		mn[j,] <- apply(diff, 2, mean)		# calculating mean of each variable such as xbar, md, tmu, hbr and tuky, and storing as vector
		vr[j,] <- apply(diff, 2, var)			# calculating variance of each variable and storing as vector
		}

	r1 <- range(c(min(mn),max(mn)))			# calculating range of the mean among all the values of the matrix to get appropriate graphical range 
	r2 <- range(c(min(vr),max(vr)))			# calculating range of the variance among all the values of the matrix
	w <- seq(.01,.1,.01)					# generating a sequence which is same as our "e", previously expalined

	par(mfrow=c(2,1))						# configuring how many plots would be per page(2 rows and 1 column)
	par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)		# configuring spaces to put legend and graph appropriately
									# creating multiple plot's with corresponding legends' codes for mean & variance vectors 

	matplot(w,mn,type="l",ylim=r1,main="e vs means",xlab = "e", ylab = "Mean of Estimators",col=1:5, lwd = 2)		# mean plot against 'e'
	legend("topright",inset=c(-0.45,0),c("mean","median","trim","huber","tukey"),lty=1,col=1:5)				# corresponding legend of means
	matplot(w,vr,type="l",ylim=r2,main="e vs variance",xlab = "e", ylab = "Variance of Etimators",col=1:5, lwd = 2) 	# variance plot against 'e'
	legend("topright",inset=c(-0.45,0),c("mean","median","trim","huber","tukey"),lty=1,col=1:5, lwd=2)			# corresponding legends of variance
	}

compare(rep=1000, n=10, est=5, N=100, mu=15)		# using our function to see the ultimate results

# if we observe the 'e' vs 'mean' graphs we see all the means of estimators are close to 0, except usual mean called average.
# our actual data came from mixed normal, most of them are from standard normal and few of them from other normal distribution.
# with the change of percentage of outliers other methods give almost same result (i.e mean = 0), however, actual mean can't not, 
# and actual mean tends to change with the outliers because it is affeted by exteme values.

# on the other hand if we obbserve 'e' vs 'variance' we see tukey and median are giving more variance whereas huber and actual mean 
# are giving less variance. However, if we consider the range of densities all are very close to each other.




