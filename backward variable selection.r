library(MASS)
attach(UScrime)			# Uploading the data set

round(cor(UScrime),1)		# calculating correlation martix of all variables. Here, we can observe that "Ineq" and "ED"
					# are highly correlated. So, we can use any of them. However, we will do that later if necessary

# now performing Backward selection procedure to get the most fesible variables, at alpha = 0.10.
# we created a function to do backward elimination named "Backward", where, dat=data set and alpha=significance level.

Backward <- function(dat, alpha){				# initializiation the function
	new.Model <- lm(y~., data=dat)			# permorming linear regression analysis
	M <- summary(new.Model)$coeff 			# extracting coefficients of variables
	count <- 1							# intializing a varaiable to count iteration
	while(max(M[,4])> alpha){				# starting a while loop, it will run wheneve p-value > alpha
		old.Model <- new.Model				# converting new estimate into old
		M <- summary(old.Model)$coeff 				# extracting coefficients of variables
		V <- case.names(M)[which(M[,4]==max(M[,4]))]		# selecting variable whose p-value is > alpha and maximum than all variables
	
		new.Model <-eval(substitute(update(old.Model,.~. -v),list(v=as.name(V))))		# deleting insignificant variable from the updated model
		count <- count+1
		}
	list(iteration=count, beta=summary(old.Model)$coeff)		# result in listing order
	}
Backward(dat=UScrime, alpha=0.10)						# using our bacward function

new <- data.frame(M, Ed, Po1, U2, Ineq, Prob, y)  # this data set consists of all 
								  #feasible varaibles, whose p-values are all less than alpha

par(mfrow=c(3,2))				# plotting y vs all predictors to see their relationship. Here, it seems their 
						# relationships are linear. So, we don't need to transform our variables
for(i in 1:6){
	plot(y,new[,i])
	abline(rlm(y~new[,i],maxit = 50))
	}

# since from correlation matrix we have seen "Ineq" and "Ed" are highly correlated, we add their interation in model

Mod1 <- lm(y~.+Ed*Ineq, data=new)
summary(Mod1)

# after including the interaction, it seems none of "Ineq", "Ed" and their interaction
# are significant. Now, excluding "Ed"  from the model and see what happen

Mod2 <- lm(y~.+Ed*Ineq-Ed, data=new)
summary(Mod2)

# It seems interation and "Ineq" both are significant. Now observing if we exclude
# "Ineq" from the model and include "Ed"

Mod3 <- lm(y~.+Ed*Ineq-Ineq, data=new)
summary(Mod3)

# In this case "Ed" seems significant, but not as significant as "Ineq" in the
# previous model. So, it is better to include "Ineq".

sq <- seq(1:47)			# ading a sequence variable to label all observations to check outliers
new1 <- cbind(sq,new)
Model <- lm(y~.-Ed-sq, data=new1)
summary(Model)

res <- Model$resi
pred <- Model$fitted.values

par(mfrow=c(1,1))	
library(calibrate)						# intializing a library to get observation index
plot(res, pred, xlab="Residuals", ylab="Predicted")	# ploting residual vs predicted values
textxy(res,pred,labs=sq)					# a code to get observation index

# in the plot we see observation no. 29 may be outlier. But we don't want to exclude
# because we don't know the details background of the sample collection.
# at last our suggesting model is consists of "M", "Po1", "U2", "Ineq" and "Prob".
# and all are main variables without any interaction term


## Problem 4(2)_______________________________________________

# beween last two predictors only "Prob", that is, probability of imprisonment.
# has significant effect of punishment.
