##############################################################################################
# bring in data from an outside tab delimited text file, can be converted from a EXCEL file
#     (shown in this example)
#
##############################################################################################
rm(list=ls(all=TRUE))   # removes anything that might be restored from previously running R

trees<- read.table("data/trees.txt",header=TRUE)

attach(trees)
names(trees)
dim(trees)

# change species name to a class variable, rather than a number #
speciesname<-factor(trees$species)
trees2<- data.frame(trees,speciesname)

detach(trees)

rm(trees,speciesname)

ls()

attach(trees2)

names(trees2)

# get a simple summary of all variables in dataframe trees2 #
summary(trees2)

##############################################################################################
# get simple scatterplots for volume with all variables in trees2           
# notice that the plot changes to a box plot for the class variable, 
# speciesname    (shown in this example)
##############################################################################################
                                                           
plot(volume~.,data=trees2)

rank<- order(dbh)                        # get the order of trees by dbh
sortedtrees<- trees2[rank,]              # use the order of trees to order the data

plot(volume~dbh,type="p") # points

plot(volume~dbh,type="l",data=sortedtrees) # lines -- data must be sorted by x

plot(volume~dbh,type="b",data=sortedtrees) # both -- data must be sorted by x

plot(volume~dbh,type="h",data=sortedtrees) # histogram


##########################################################################
# normality plots and histograms 
#########################################################################

qqnorm(dbh)     # normality plot
qqline(dbh,col=2)

qqnorm(height)     # normality plot
qqline(height,col=2)

hist(dbh, breaks =20 , density=10,col="green", border="black") # draws a histogram

hist(height, breaks =10 , density=10,col="black", border="black") # draws a histogram

#############################################################################################
#  plots from regression -- standard set.  NOTE: you will need to click on the 
# graph window to bring up each plot (i.e., "Waiting to confirm page change..."
#############################################################################################

model1<-lm(volume~height,data=trees2)
plot(model1)

##########################################################################
#  Box plots:  volume and logarithm of volume
#########################################################################

boxplot(volume~speciesname,data=trees2,col='pink')

boxplot(volume~speciesname,data=trees2,log="y",col='pink')

##########################################################################
# Multiple plots
#########################################################################
require(lattice)

xyplot(volume~height+dbh,data=trees2,scales="free",groups=speciesname,auto.key=TRUE,layout=c(1,2),corner=c(0,0))


##########################################################################
#  Another way to get multiple plots:  2 box plots, one graphics image
#########################################################################

par(mfrow=c(1,2),mai=c(1.4, 1.4, 0.2, 0.2),cex=1.5)
boxplot(volume~speciesname,data=trees2,col='pink')
boxplot(volume~speciesname,data=trees2,log="y",col='pink')
par (mfrow=c(1,1), mai=c(1.0,1.0,1.0,1.0),cex=1.0)

#############################################################################################
#  Clean up all of your files, or shut down R before doing another exercise
#############################################################################################

rm(list=ls(all=TRUE))   # removes anything that might be restored from previously running R

ls()












