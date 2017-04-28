# a funcction for testing probability distribution of order statistics, where, x is any point, k is k-th order,
# mu1=mean, sd1=standard deviation,nrep=no.of replication.

ordesim <- function(x,k,n,mul,sdl,nrep,lower.tail=T){
	tr <- NULL
	nrep <- 10000
	for(i in 1:nrep){				# a for loop to do simulation
 		X <- sort(rnorm(n,mul,sdl))	# ordering the X values
		tr <- c(tr, X[k] <= x)		# comparing the X values
		}
	L.prob <- sum(tr)/nrep		# getting lowe tail probability
	U.prob <- (nrep-sum(tr))/nrep
	return(L.prob)
	}
ordesim(x=0,k=87,n=100,mul=0,sdl=1,nrep=10000,lower.tail=T)	# using our created function