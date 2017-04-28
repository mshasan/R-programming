## (f)significan genes at q-value threshold of 0.05__________________________________________________________

library(splines)						# intializing a library to use splines
pvali<-sort(pval1) 					# sorted p-values
genei<-order(pval1)					# ordered pvalues 

g <- ncol(data61)-2					# total no. of genes
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

pihat <- fitted(pimod)[69]  				# our final estimate for pi0 
								# next, find q-values:
qhat<-vector(length=g)  				# will contain q-values
qhat[g]<-pihat*pvali[g]  				# q-value for highest p-value

								# remaining q-values:
for(i in (g-1):1){
	qhat[i]<-min(pihat*g*pvali[i]/i,qhat[i+1])
	}

sig2 <- data61[0,3:ncol(data61)][qhat < 0.05]	# genes signficant at FDR=0.05. 
