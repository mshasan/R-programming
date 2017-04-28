# our funtion's name is "kernel" which will estimate the kernel density using normal distribution. 
# here 'y' is a vector, 'bw' is the bandwidth and 'n' is the number of equally spaced points.

kernel <- function(y, bw, n){				# initializing the function
	m <- length(y)					# calculation number of observations in the vector
	if(missing(n)) n = 500				# if 'n' is missing then it will take n=500 as a default
	h <- bw.SJ(y)					# calculating bandwith by using builtin 'SJ' functional form
	if(missing(bw)) bw = h				# if 'bw' is missing then it will take 'h' as default bandwith

	ygrid <- seq(from = min(y) - 1, to = max(y) + 1, length.out = n)		# generating a sequence of length n'
	gauss <- function(y) 1/sqrt(2*pi) * exp(-(y^2)/2)				# creating a function called 'gauss', which is actually standard normal pdf
	bump <- sapply(y, function(a) gauss((ygrid - a)/bw)/(m * bw))		# using 'gauss' function inside of the 'sapply' function to get bumping estimate
	hist(y, freq = F)										# construcitng a histogram

	lines(ygrid, rowSums(bump), ylab = expression(hat(f)(y)),type = "l", xlab = "y", lwd = 2)		# a curve of kernel densities gotten from our 'kernel' function

	cbind(y, dnorm(y))					# creating a matrix consists of vector values and corresponding densities
	}

x<-rnorm(30,0,10)							# to check our function generating 30 random data from normal distribution with mean = 0 and sd = 10
kernel(x)#, k=0.4, n=500)					# using our "kernel" function

lines(density(x), lty = 2, col = "red", lwd=2)		# a curve is superimposed by using builtin kernel function to compare our kernel densities

# our density function and bultin kernel function are giving almost same results
