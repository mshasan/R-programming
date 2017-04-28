# Problem 1(1)______________________________________
# The function "read.table()" reads the data named "HW1data.txt" from the 
# prespecified directory and "header = F" ignores the headline or header of 
# the dataset

dat <- read.table("HW1data.txt", header = F); dat

# Problem 1(2)______________________________________
# Here "[dat[,1]"indicates the 1st column of the whole data set, 
# and "dat[dat[,1]>10,]" and "dat[dat[,1]<10,]" give the observations whose 
# 1st column is grater than 10 and less than 10, respectively.
 
x<-dat[dat[,1]>10,]; x
y<-dat[dat[,1]<10,]; y

# Problem 1(3)______________________________________
# mean() and var() are two R functions use to calculate the mean and variance 
# respectively. Here x[,2] and y[,2] identify the 2nd column of the two groups 
# x and y respectively.

mean.x <- mean(x[,2]); mean.x
var(x[,2])

mean.y <- mean(y[,2]); mean.y
var(y[,2])

# Problem 1(4)______________________________________
# Since we need output of the two means, we construct a vector by the two means
# to make a single set, and to assign names to two groups we used "names()". 
# The function "write.table()" is used to get the output to the sepecific directory. 
# Here, "mean.xy" is desired result, "Hasan_HW1out.txt" is our file name and 
# row.names=T is used to get the row names.

mean.xy <- c(mean.x, mean.y); mean.xy
names(mean.xy) <- c("group x mean", "group y mean")
write.table(mean.xy,file="Hasan_HW1out.txt", row.names=T)



#################################################################
#################################################################

# Problem 2(a)__________________________________________
# Here "rnorm()" is used to get 20 random values from normal dist. with mean 10 and
# standard deviation 5, and "sort(x1[x1 > 8])" gives the sorted values from smaller to
# larger which are greater than 8
 
x1 <- rnorm(20,mean=10,sd=5); x1
sort(x1[x1 > 8])

# Problem 2(b)__________________________________________
# "x1[x1 < 8]" gives the values less than 8 and "x1[x1 > 12]" gives the values greater
# than 12. The function c() makes a vector by using those values and sort() 
# is used to sort them from smaller to larger.

sort(c(x1[x1 < 8], x1[x1 > 12]))

# Problem 2(c)__________________________________________
# "x1[x1 >= 8 & x1 <= 12]" gives the values which are greater than or equal to 8 and
# less than or equal to 12, and "sort()" sort the values from smaller to larger 

sort(x1[x1 >= 8 & x1 <= 12])


#################################################################
#################################################################

# Problem 3(a)___________________________________________
# rnorm() gives the 20 random obs. from normal dist. with mean 10 and standard 
# deviation 5. sample() gives 20 values from sequence 1 to 20 but in random order.
# runif() gives 20 standard uniformly distributed obs.
# cbind() is used to combine x1, x2 and x3 as the matrix columns.

x1 <- rnorm(20,mean=10,sd=5)
x2 <- sample(20)
x3 <- runif(20)
combine <- cbind(x1, x2, x3); combine

# Problem 3(b)___________________________________________
# Here combine[,2] indicates the 2nd column of the combine matrix, the function
# order() with "combine[order(combine[,2], decreasing = T),]" gives the desending
# ordered values according to the 2nd column, where "decreasing=T" order the 
# values from larger to smaller.  

reorder <- combine[order(combine[,2], decreasing = T),]
reorder

# Problem 3(c)___________________________________________
# "reorder[reorder[,1] > 9 & reorder[,3] < 0.6, ]" gives the boservations whose 
# 1st column's values are greater than 9 and 3rd column's values are less than 0.6. 

reorder[reorder[,1] > 9 & reorder[,3] < 0.6, ]


