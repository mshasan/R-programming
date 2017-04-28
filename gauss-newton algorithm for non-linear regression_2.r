library(mosaic)

varNames <- all.vars(formula)
varNames <- varNames[is.na(match(varNames, prm))]
eval(as.name(varNames))
prm <- b

mynls <- function(formula,data,start,fixed,eps,maxit){
	formula <- as.formula(formula)
	N0 <- data[1,2]
	t <- data[,1]
	N <- data[,2]
	prm <- names(start)
	derv <- list()
	for(j in 1:length(prm))	
		derv[[j]] <- D(as.expression(formula[[3L]]), prm[j])

	b <- start
	K <- start[1]; r <- start[2]
	res <- N - eval(formula[[3L]])
	SS.new <- sum(res**2)
	count <- 0
	diff <- 1
	while(diff > eps  & count < maxit){
		SS.old <- SS.new
		
		ff <- matrix(0,length(t),length(start))
		for(i in 1:length(start))ff[,i] <- eval(derv[[i]])
		
		b <- c(b+solve(t(ff)%*%ff)%*%t(ff)%*%res)
		K <- b[1]; r <- b[2]	

		res <- N - eval(formula[[3L]])
		SS.new <- sum(res**2)
		diff <- abs(SS.old - SS.new)
		count=count+1
		names(b)<-c("K","r")
		fit <- K/(1+(K-N0)/N0*exp(-r*t))
		}
	list(parameters=b,maxit=count,RSS=SS.new,fitted=fit)
	}

data<-read.table("daphinapop.txt")
names(data)<-c("t","N")
attach(data)

fit <- mynls(formula=N~K/(1+(K-N0)/N0*exp(-r*t)),data,start=c(K=500,r=0.1),fixed=N0,eps=0.00001,maxit=1000)
y <- fit$fitted
plot(t,y,type="l")
points(N)


log <- nls(population ~ K/(1+(K-N0)/N0*exp(-r*time)),start=c(K=500,r=0.5),data=popdat, trace=T)
plot(time,fitted(log),type="l")
points(population)
