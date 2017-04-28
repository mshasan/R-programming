# Problem 2(1,2)____________________________________________________

# ANOVA our function to get he anova results, and "data2" is the dataset to work on it inside the function.

ANOVA <- function(data2){				# initializing the function
	data2 <- as.data.frame(data2)			# convert the observations into data set if necessary
	Y <- data2[,1]					# select 1st column(response variable) of the data
	Y.bar <- mean(data2[,1])			# Taking average of the 1st column
	SST <- round(sum((Y- Y.bar)^2))		# Getting total sum of square rounded by whole numbers

	Tr <- length(levels(data2[,2]))		# Getting total number of levels or treatments
	SSTR <- NULL					# Initializing the Treat. sum of square by NULL value
	for(i in 1:Tr){					# Starts the for loop cycle, and cycles are equal to no. of treatmeants 

		T <- mean(data2[data2[ ,2]==levels(data2[,2])[i],][,1]) 	# "T" Takes mean of the response values corresponding to the unique labels
												# "levels(data2[,2])[i],]" extracts i-th label and  
												# "data2[data2[ ,2]==levels(data2[,2])[i],][,1]" takes corresponding response values
		r <- nrow(data2[data2[ ,2]==levels(data2[,2])[i],])	# "r" counts the no. of obs. corresponding to the i-the label
		SSTR <- round(sum(SSTR, r*(T - Y.bar)^2))			# "SSTR" adding treaments sum of square with the previous SSTR, and rounded by whole number		
		}
	SSE <- SST - SSTR				# calculating error sum of square 
	SS <- c(SSTR,SSE,SST)			# Converting 3 sum of squares into 1 vector

	n <- length(Y)				# Counting total no. of observations in the date set
	MST <- round(SST/(n-1),2)		# Mean total sum of square rounded by 2 decimals points
	MSTR <- round(SSTR/(Tr-1),2)		# Mean Treat sum of square rounded by 2 decimals points
	MSE <- round(SSE/(n-Tr),2)		# Mean Error sum of square rounded by 2 decimals points
	F <- round(MSTR/MSE,3)			# F-statistic of treatment rounded by 3 decimals points
	p <- round(1-pf(F, 2, 47),5)		# Pvalue corresponding to the F-statistic rounded by 5 decimals points

	rnames <- c("Treatment", "Residuals", "Total")						# We diplay our result as matrix consiss of 3 rows and 5 columns
	cnames <- c("Df", "SS", "MSS", "F.value", "P.value")					# "rnames" and "cnames" represent rows and columns names respectively
	result <- c(2,47,49,SSTR,SSE,SST,MSTR,MSE,MST,F,NA ,NA ,p,NA,NA)			# vector consists of anova results 
	mymatrix <- matrix(result, nrow=3, ncol=5,dimnames=list(rnames, cnames)) 	# matrix form of the anova results
	Des <- ifelse(p < .05, "Reject null Hyp.", "Failed to reject null Hyp.") 	# Getting decision of hypohesis based on pvalue
	H0 <- "Treatments are equal"									# Our null hypothesis
	list(ANOVA.Result=mymatrix,Null.Hyp = H0, Decision=Des)				# All results in a listed form
	}

# To check our function we can use following steps. 1st we will use our function on the given data "HW22.txt" and then use R-built-in function 
# to get the anova results. We get same results in both cases. So, we can say our function is okay.

HW22 <- read.table("HW22.txt", header = T) 			# reading data set from the specific directory	
ANOVA(HW22)									# using our function "ANOVA" on data "HW22.txt"

fit <- aov(d1 ~ d2, data=HW22) 					# R-built-in function for anova
summary(fit)