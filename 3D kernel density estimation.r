# funntion for estimating kernel density for bivariate data by using normal function
# where x and y are independent bivariate, "bw" is bandwidth, "n" is equally spaced points ans "adjust" for error adjustment

mydensity3D <- function (X, bw, n , adjust=1){		# initializing the function
	x <- X[,1]							# x and y are the 1st and 2nd col. of the matrix
	y <- X[,2]
	nx <- length(x)						# calculating no. of obs in x
      n <- rep(n, length.out = 2L)				# for bivariate reason calculating 2 set of n

	if(missing(bw)) bw <- c(bw.nrd0(x), bw.nrd0(y))  	# for missing bw, calculatig a bandwidth
	bw <- c(bw.nrd0(x), bw.nrd0(x))				# calculating bw if not missing
      
	xmin <- min(x) - mad(x)*0.3				# adjusting our range of interval by using median absolute dev. to get
	xmax <- max(x) + mad(x)*0.3				# good result, and overcome frome outliers
	ymin <- min(y) - mad(y)*0.3
	ymax <- max(y) + mad(y)*0.3

	
    	gx <- seq(from=xmin,to=xmax,length.out=n[1L])	# generating a seq. by using the above range of x
    	gy <- seq(from=ymin,to=ymax,length.out=n[1L])	# generating a seq. by using the above range of y

	
    	ax <- outer(gx, x, "-")/bw[1L]			# by using outer function getting diff. of all 
   	ay <- outer(gy, y, "-")/bw[2L]			# combinations between gx and x, and gy and y

    	z <- tcrossprod(matrix(dnorm(ax), , nx), matrix(dnorm(ay), , nx))/(nx * bw[1L] * bw[2L])		

			# return a matrix of densities after calculating all combination of densities by using cross-product. 
			# This is formally equivalent to (but usually slightly faster than) the t(x) %*% y (crossprod) 
			# or x %*% t(y) (tcrossprod).

	r <- matrix(0,625,2)			# null matrix to store output
		for(j in 1:25){			# using a for loop to get the ultimate output
		r[] <- cbind(gx[j], gy)		# storing every combination of my result
		}
	mat <- cbind(r,as.vector(z))

	par(mfrow=c(1,1))			# plot per page, here is 1
						
	persp(x = gx, y = gy, z = z, xlab = "x", ylab = "y",			# drawing a surface plot with labels
	zlab = expression(K(x, y)), theta = -35, axes = TRUE, box = TRUE, main="Bivaraite kernel density")
	
	return(list(x = gx, y = gy, z = z, result=mat))			# geetting results in listing order
	}



x <- seq(from = -1.1, to = 1.1, by = 0.05)				# generating a sample sequence
y <- seq(from = -1.1, to = 1.1, by = 0.05)				# another sample sequence
X <- cbind(x, y)									# creating a matrix
mydensity3D(X, bw="nrd0", n=25 , adjust=1)				# using my kernel function for bivaraite data

library(MASS)							
kde2d(x,y)
persp(x = kde2d(x,y)$x, y = kde2d(x,y)$y, z = kde2d(x,y)$z, xlab = "x", ylab = "y",			
zlab = expression(K(x, y)), theta = -35, axes = TRUE, box = TRUE, main="Bivaraite kernel density by R-builtin")

# to check our function we can compare R-builin function names "kde2d()". we observe that our results are close enough 
# to the builin function, we can say our result is better because we used "mad()" to get better range.

