##(g) dendogram of significant genes___________________________________________________________________________

qval <- qhat[qhat < 0.05]				# corresponding qvalue for significant genes

data62 <- read.table("data61.txt",header=T,colClasses = "character")	# reading data as columns are vector of class 
brain <- data62[1,3:ncol(data61)][qhat < 0.05]					# vector of brain function of sgnificant genes

clust <- hclust(dist(qval))				# getting cluster estimation
plot(clust,label=brain)					# ploting cluster with labeles of brain function

windows()							# Opening a new window 
ht <- cutree(clust, h=.000001)			# making group because we have a lot of cluster elements, we try to zoom in, 
clust.ht <- hclust(dist(qval[ht==1]))		# calculating cluster estimation
plot(clust.ht,label=brain[ht==1])			# ploting with labels

windows()								# again new window for another zoom in result
kt <- cutree(clust.ht,k=10)					# creating group
clust.kt <- hclust(dist(qval[ht==1][kt==1]))		# cluster estimation
plot(clust.kt,label=brain[ht==1][kt==1])			# ploting the result

# we see our cluster analysis attempt to sort brain function into discrete groups. Ideally, brain function within
# the clusters are more similar in some sense than the brain fnction between groups. Since, we have same types of brain 
# function for different genes, we see same brian function into different groups, however, they are within group are 
# more similar than the beween groups 