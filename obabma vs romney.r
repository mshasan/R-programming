dat1 <- read.table("PresidentialPollDataNov2.txt", sep="\t")			# reading the data from the source file
names(dat1) <- c("Name","Date","SZ.RLV","MOE","Obama","Romney","Result" )	# attributing names for each column of the data
attach(dat1)											# attaching the data

x <- as.vector(SZ.RLV)				# converting "SZ.RLV" column as vector
y <- strsplit(x, '[ ]')				# seperating numerical and character values
y2 <- sapply(y, '[[', 1)			# keeping only numerical values for further calculations
S.size <- as.numeric(y2)			# making all values numerical and "NA"(which were character)

dat2 <- data.frame(S.size,dat1[,2:6])	# creating a new data set
dat3 <- dat2[rowSums(is.na(dat2)) == 0,]	# excluding missing observations

t1 <- as.vector(dat3[,2])			# converting dates as vector
t2 <- strsplit(t1, '[-]')			# seperating two dates, previously were together
t2.1 <- sapply(t2, '[[', 1)			# getting begining date of survey
t2.2 <- sapply(t2, '[[', 2)			# getting ending date of survey

t.1st <- strptime(t2.1 , "%m/%d")		# changing begning date into another format
t.2nd <- strptime(t2.2 , "%m/%d")		# changing ending date into another format

date1 <- as.POSIXlt(t.1st, origin="2012-01-01")$yday+1	# conveting begning date into no. of days from 01/01/2012
date2 <- as.POSIXlt(t.2nd, origin="2012-01-01")$yday+1	# conveting ending date into no. of days from 01/01/2012
day <- date2-date1+1							# calculating no. of days between two dates

Obama.vt <- dat3[,5]*dat3[,1]/100					# calculating no. of votes for Obama for each survey
Romney.vt <- dat3[,6]*dat3[,1]/100					# votes for Romney

dat4 <- data.frame(dat3[,1],day,Obama.vt,Romney.vt)		# creating another data set
Sz.Pc <- dat4/day								# converting data from different no. of days into a data set for each day
dat5 <- Sz.Pc[rep(1:nrow(Sz.Pc), day), ]				# creating corresponding replications of the previous days, because votes are evenly distributed

day.seq <- NULL								# starting a vactor with null value
for(i in 1:length(date1))
	day.seq <- c(day.seq,seq(date1[i],date2[i],by=1))	# a for loop to generate sequences from begnng date to end date

dat6 <- data.frame(day.seq,dat5)					# another new data set
dat6 <- dat6[order(dat6[,1]),][1:1197,]								# truncating observation, because, we need to use observation utill November 01
dat7 <- aggregate(cbind(dat6[,2],dat6[,4],dat6[,5]) ~ dat6[,1], data = dat6, sum)	# aggregating data, that is, computes summary statistics for each, and returns the result in a convenient form. 

Obama.perc <- dat7[,3]/dat7[,2]*100						# calcualting percentage votes for Obama
Romney.perc <- dat7[,4]/dat7[,2]*100					# percentage for Romney
dat8 <- data.frame(dat7[,1],Obama.perc,Romney.perc)			# forming another data set

ker.Ob <- ksmooth(dat8[,1],dat8[,2],"normal",bandwidth=15)		# doing kernel smoothing over Obama's percentage
ker.Rm <- ksmooth(dat8[,1],dat8[,3],"normal",bandwidth=15)		# kernel smoothing over Romney's percentage

matplot(dat8[,1],cbind(ker.Ob$y,ker.Rm$y) ,type="l",xlab = "Day in year", ylab = "Percent supporting")	# ploting the kernel smooting fitted vlues
matpoints(dat8[,1],cbind(dat8[,2],dat8[,3]),pch = 21)										# ploting actual data points
legend(75,48.80,cex=.70,c("Obama","Romney"),lty=1,col=1:2)									# giving legend of the information		

###################################################################################################################################################################################
###################################################################################################################################################################################
