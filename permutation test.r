pval2 <- NULL								# another null vector to store permutaion's p-values
for(j in 3:ncol(data61)){						# for loop to perform permutation over all genes						
	x <- data61[,j][data61[,2]==1]				# extracting j-th gene's expression values with bipolar
	y <- data61[,j][data61[,2]==0]				# extractiong j-th gene's expression values with normal
	X <- as.numeric(levels(x)[x])					# converting x and y values into numeric vector
	Y <- as.numeric(levels(y)[y])

	meandiffobs=(mean(X)-mean(Y))				# calculating difference between two observed values' means
	l1 <- length(X)						# no. of values of X and Y
	l2 <- length(Y)

	ds <- c(X, Y)						# combining X and Y
	meandiff <- numeric(10) 				# a numeric vector to store mean difference during permutations 
	numperm <- 10						# no. of iteration per permutation test
		for(i in 1:numperm){	
		s1 <- sample(l1+l2,l1) 				# generating a sample index from combination of X and Y of size X
		perm1 <- ds[s1] 					# 1st sample
		perm2 <- ds[-s1]					# 2nd sample
		meandiff[i]=(mean(perm1)-mean(perm2))	# mean difference between two samples
		}
	meandiff <- sort(meandiff,decreasing=T)					# sorting the mean differences
	upperpval <- length(meandiff[meandiff > meandiffobs])/numperm  	# calculating no. of upper p-value
	lowerpval <- length(meandiff[meandiff < meandiffobs])/numperm	# lower p-value
	pval2 <- c(pval2, min(upperpval,lowerpval))				# vector of p-values from permutation test
	}
pval2