# Part I. FIRST WAY TO BRING DATA INTO R.  Type the numbers directly into R, each variable separately, and 
# then append these together.  Each variable will be a column in a matrix called treedat.  
# Then, put this into a dataframe for use later (another way to store the data) called treedat2.
####################################################################################
# enter in the tree numbers first
treeno<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
treeno
# list the first entry of trees
treeno[1:1]
#
# enter in the dbh data
dbh<-c(10.1,11.2,19.7,20.5,17.8,17.0,11.0,4.1,6.0,8.0,2.3,20.1,18.0,22.1,16.3,20.5,17.0,18.0,17.0,19.7)
dbh
#
# enter in the height data
height<-c(14.2,15.1,25.3,21.2,21.5,18.0,12.1,5.2,6.3,9.1,10.1,19.2,16.0,26.3,17.3,19.8,20.1,22.3,19.5,18.6)
height
#
treedat<-data.frame(treeno,dbh,height)
treedat
# to select the dbh column of the dataframe, only, use
col2<- treedat[,"dbh"]
col2

########################################################################################################
# Clean up the workspace before going to the next part
#######################################################################################################
ls()      # This lists all of the objects in the workspace
rm(col2,treeno,dbh,height)    # Remove these since they are now part of treedat and treedat2 objects.
ls()
#######################################################################################################
# Part II. Basic statistics. for dbh values, get a. the mean; b. the variance; 
#  c the standard error of the mean; d. the mode; e. the median;   f. The CV as a percent;   
#  g.  A 95% CI for the true mean;  h.  proportional of obs > 10.9 cm dbh; 
#  i.  Prob. of > 10.0 cm dbh assuming a normal distribution;
#######################################################################################################
n<-length(treedat[,"dbh"])
n                   # no. of observations

dbhbar<- sum(treedat[,"dbh"])/n
dbhbar              # a.  mean dbh

ssdbh<- sum((treedat[,"dbh"]-dbhbar)^2)  #  corrected sums of squares, dbh
vardbh<-ssdbh/(n-1)
vardbh             #  b.  variance of dbh

sterrordbh<- (vardbh/n)^0.5   
sterrordbh       # c.  standard error of the mean dbh

sorteddbh<-sort(treedat[,"dbh"])
mediandbh<-sorteddbh[n/2]  # find the dbh at n/2, the middle
mediandbh            #d. median dbh

quantile(treedat[,"dbh"])  #  to get all the quantiles

cvdbh<-100*((vardbh^0.5)/dbhbar)
cvdbh                #e. cv as a percent

df<-(n-1)  # degrees of freedom is no of samples - 1
tvalue<-qt(0.975,df) # get the tvalue for 1-alpha/2 to get a (1-alpha)*100 confidence interval
lowerCI<- (dbhbar-(tvalue*sterrordbh))  # g.  A 95% CI for the true mean

upperCI<- (dbhbar+(tvalue*sterrordbh))
df; tvalue; lowerCI; upperCI

dbh<- treedat[,"dbh" ]
nobs<- length(dbh [dbh > 10.9])  
proplarge<- (nobs/n)*100 
sorteddbh
proplarge    #  h.  proportional of obs > 10.9 cm dbh 

sddbh<- (vardbh^0.5)   
zvalue<- (10.0 - dbhbar)/sddbh 
zvalue   
problarge<- 1-pnorm(zvalue)  
problarge #  i.  Prob. of > 10.0 cm dbh assuming a normal distribution
               
##############################################################################################
# statistics using built in functions, useful for data in general
##############################################################################################
#  means and other stats of all variables using built in stats functions
mean(treedat[,"dbh"])
mean(treedat[,"height"])
sd(treedat[,"dbh"])
sd(treedat[,"height"])

# histograms of variables
hist(treedat[,"dbh"])  # note that this graph will be replaced by the next one, unless it is saved
# The histogram will appear in a separate window. Can use File, and Save As, to save the histogram
# as a .jpg or other picture file

hist(treedat[,"height"])   # The dbh histogram disappears, and the height histogram appears
hist(treedat[,"height"],plot=FALSE)  # calculates frequencies but no histogram is drawn

# Sort the trees by dbh first and then by height.  First step is to calculate the indexes for this order.
indexsort<-order(treedat$dbh,treedat$height)
# use these indices to then sort the trees
sorttreedat<-treedat[indexsort,]
sorttreedat                 # data sorted by dbh and height afterward


# summary statistics for each of the files-- all the same data just stored differently
require(stats)   # This is another R pack with other functions in it.
summary(treedat)
summary(sorttreedat)

#######################################################################################################
# Clean up the workspace before going to the next part
#######################################################################################################
ls()      # This lists all of the objects in the workspace
rm(cvdbh, dbh,dbhbar,df,indexsort,lowerCI,mediandbh,n,nobs,problarge,
proplarge,sddbh,sorteddbh,sorttreedat,ssdbh,sterrordbh,tvalue,upperCI,vardbh,
zvalue)    # Remove these since they are now part of treedat and treedat2 objects.
ls()
###############################################################################################
Part III.  linear regression of height vs. dbh
###############################################################################################

# Obtain the regression using the dataframe, treedat2. lm calculates the linear regression.
# Summary prints out some of the results obtained.
summary(lm(treedat$height~treedat$dbh))

# plot the original data and the fitted line.
plot(treedat$height~treedat$dbh)
abline(lm(treedat$height~treedat$dbh))

# since treedat was created as a dataframe, the names can be attached for simpler commands
attach(treedat)  # this allows you to use the dataframe, treedat2, with shorter names for variables
names(treedat)

cor(treedat)   # correlations first
plot(height~dbh)   # simple plot of height (y) versus dbh (x)

lm.height=lm(height~dbh)  # fit the linear model and store it as an object
lm.height
summary(lm(height~dbh))  # can use the fitted linear model that was stored as an object instead
summary(lm.height)
anova(lm.height)

plot(height~dbh)
abline(lm.height)  # plot the height versus dbh and overlay the regression line

yhat<-fitted(lm.height) # store the predicted values in an object, yhat. 
resid<-resid(lm.height) # store the observed - predicted values, called the residuals, in resid
cbind(height,yhat,resid)  # list measured and predicted height and differences (i.e. residuals)

plot(yhat~height)
abline(a=0,b=1)   # plot a reference line where yhat equals height 

plot(resid~yhat)  # residual plot

plot(resid~dbh)    #different residual plot

qqnorm(resid)     # normality plot
qqline(resid,col=2)

hist(resid, freq=FALSE,breaks = 8, density=10,col="green", border="black") # draws a histogram

#############################################################################################
#  Clean up your files, etc.
############################################################################################
detach(treedat)  # This just detaches the dataframe, treedat, but it can be reattached
ls()      # This lists all of the objects in the workspace
          # keep the treedat, lm.height, resid,and yhat objects

##############################################################################################
# Part IV. Fixing problems using transformations.  Do the transformations using the dbh and 
# height objects, and then put this into a new dataframe.
##############################################################################################

attach(treedat)  # reattach treedat dataframe
dbhsq<-dbh^2    # get dbh squared
logdbh<-log(dbh) # get log of dbh
treedat2<-data.frame(treedat,dbhsq,logdbh) # add these to treedat and save in treedat3
treedat2
detach(treedat)

rm (dbhsq,logdbh)

attach(treedat2) # This attaches treedat2 to the session, meaning that you can now use the variable names
names(treedat2)

# regression using dbh squared.  This gives a parabola shape.  
lm.height2<-lm(height~dbhsq)
lm.height2
summary(lm.height2)
anova(lm.height2)
yhat2<-fitted(lm.height2)
resid2<-resid(lm.height2)
cbind(height,yhat2,resid2)  # list measured and predicted height and differences (i.e. residuals)

plot(yhat2~height)
abline(a=0,b=1)   # plot a reference line where yhat equals height 

plot(resid2~yhat2)  # residual plot

plot(resid2~dbh)    #different residual plot

qqnorm(resid2)     # normality plot
qqline(resid2,col=2)

hist(resid2, freq=FALSE,breaks = 8, density=10,col="green", border="black") # draws a histogram

#############################################################################################
#  Clean up your files, etc.
############################################################################################
detach(treedat2)  # This just detaches the dataframe, treedat2, but it can be reattached
ls()      # This lists all of the objects in the workspace
rm(lm.height,lm.height2,resid,resid2,treedat,treedat2,yhat,yhat2)    # remove all objects
ls()

##############################################################################################
# Part V. SECOND and THIRD WAY to bring in data, and CI's for predicted heights
#  1.  bring in data from an outside tab delimited text file, can be converted from a EXCEL file
#     (shown in this example)
#  2.  If EXCEL saved as comma delimited, then change this to:
#      treedat4<-read.csv("E:\\R_workshop\\materials for nigeria_2009\\course_materials_nigera\\data\\ht_dbh.csv",header=TRUE)
##############################################################################################
treedat3<- read.table("E:\\R_materials_workshops\\workshop_2013\\data\\ht_dbh.txt",header=TRUE)

attach(treedat3)
names(treedat3)
treedat3

dbhsq<-dbh^2  # dbh squared
logdbh<-log(dbh) #log dbh
treedat4<-data.frame(treedat3,dbhsq,logdbh)
treedat4
detach(treedat3)
rm(treedat3,dbhsq,logdbh)

attach(treedat4)

lm.height<-lm(height~dbhsq)
lm.height
summary(lm(height~dbhsq))
anova(lm(height~dbhsq))

yhat<-fitted(lm.height)
resid<-resid(lm.height)
cbind(height,yhat,resid)  # list measured and predicted height and differences (i.e. residuals)

plot(yhat~height)
abline(a=0,b=1)   # plot a reference line where yhat equals height 

plot(resid~yhat)  # residual plot

plot(resid~dbh)    #different residual plot

qqnorm(resid)     # normality plot
qqline(resid,col=2)

hist(resid, breaks =8 , density=10,col="green", border="black") # draws a histogram

# get 95% confidence intervals for the average height, given each of the dbh's in the data, using the equation
predCI<- predict(lm.height, treedat4,interval="confidence")
predCI

# plot the orginal data, the predicted values and 95% confidence interval bands
rank<- order(dbh)
sortedpredCI=predCI[rank,]
dbhsort<-dbh[rank]
plot(dbh,height)
matlines(dbhsort,sortedpredCI,lty=c(1,2,2),color="black") #overlay the predicted values and CI using linetypes given

# get confidence intervals for the average heights for dbh=20 cm
dbhnew<-20.0
dbhsqnew<-dbhnew^2
new <- data.frame(dbh=dbhnew,dbhsq=dbhsqnew) 

# default is for a 95 percent confidence interval for mean y given the x.  Can change the percent by including the level
pred.w.clim <- predict(lm(height~ dbhsq), new, interval="confidence",level=0.90) 
pred.w.clim

pred.w.plim <- predict(lm(height ~dbhsq), new, interval="prediction",level=0.90)
pred.w.plim

#############################################################################################
#  Clean up all of your files, or shut down R before doing another exercise
#############################################################################################
rm(list=ls(all=TRUE))
















