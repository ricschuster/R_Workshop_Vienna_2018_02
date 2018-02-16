

#################################################################################
# EX_OLS.R 
# bringing in data for exercise 1 from an outside comma delimited (.csv) file 
# converted from a EXCEL file.  If EXCEL saved as tab delimited .txt file, use 
# treedat<- read.table("C:\\R\\FRST_430_2007_labs\\trees.txt",header=T) instead. 
# instead.
###############################################################################
treedat<-read.csv("data/volume30.csv", header=T) 

names(treedat)  # print out the column (i.e., variable) names
dim(treedat)    # number of rows and columns
head(treedat)  # print out the first few lines of data
treedat[1,]    # print out the first line of data
treedat[,1]    # print out the first column of data
treedat$dbh    # print out the dbh values
treedat[1:3,1:2]  # print out lines 1 to 3 and columns 1 and 2

###############################################################################
#  PART I
##############################################################################

# Basic Graphs

# set this up for 2 graphs across and 2 graphs down with some borders
par(mfrow=c(2,2),mai=c(0.5,0.5,0.5,0.5),cex=1.0)
plot(treedat$dbh,treedat$volume, type="p", cex.main=1.2, 
  main="Volume versus dbh",xlab="dbh(cm)",ylab="volume(m3)")
plot(treedat$height,treedat$volume, type="p", cex.main=1.2, 
  main="Volume versus height",xlab="height(m)", ylab="volume(m3)")
plot(treedat$age,treedat$volume, type="p", cex.main=1.2, 
  main="Volume versus age",xlab="age(years)",ylab="volume(m^3)")
par(mfrow=c(1,1),mai=c(1.0,1.0,1.0,1.0),cex=1.0)

###############################################
#   add in some transformations of variables
# log is natural logarithm, log10 is log base 10

treedat$lnvolume<-log(treedat$volume)
treedat$lndbh<-log(treedat$dbh)
treedat$lnht<-log(treedat$height)
treedat$lnage<-log(treedat$age)
treedat$recdbh<-1/treedat$dbh
treedat$dbhsq<-treedat$dbh^2
treedat$combinedvar<-treedat$dbh^2*treedat$height

names(treedat)  # print out the column (i.e., variable) names
dim(treedat)    # number of rows and columns
head(treedat)   # first few lines of data

#######################################################
# write to an external file.  In this case, a csv file

write.csv(treedat, "../data/volumes30_plus.csv")

# to write to a .txt file, use:
# write.table(treedat, "../data/volumes30_plus.txt",
#   col.names=TRUE)


##### MODEL 1 #############################################################
          
lm.volume.M1<-lm(volume~combinedvar,data=treedat) #fit the model
lm.volume.M1  # get the estimated coefficients for the model
anova(lm.volume.M1)   # get the Anova table for the model, including the F test
summary(lm.volume.M1) # get t-tests and some fit statistics for the model
treedat$yhat.M1<-fitted(lm.volume.M1)  # the estimated y values
treedat$resid.M1<-resid(lm.volume.M1)  # the errors

names(treedat)
dim(treedat)
head(treedat)

# get diagnostic plots
par(mfrow=c(2,2),mai=c(0.5,0.5,0.5,0.5),cex=1.0)
plot(treedat$yhat.M1,treedat$resid.M1, main="Model 1, Residual Plot",
  ylab="yhat", xlab="residual")
plot(treedat$volume,treedat$yhat.M1, main="Model 1, Fitted line plot",
  ylab="yhat", xlab="volume")
qqnorm(treedat$resid.M1, main="Model 1, Normality plot")
hist(treedat$resid.M1, breaks =8 , density=10,col="green", border="black",
main="Model 1, Error Distribution") 
par(mfrow=c(1,1),mai=c(1.0,1.0,1.0,1.0),cex=1.0)

# get 95% confidence intervals for the mean volume (i.e., the yhats)
# for every observation in the data
treedat$CI.M1<- predict(lm.volume.M1, treedat,interval="confidence", 
  level=0.95)

names(treedat)
dim(treedat)
head(treedat)

# get 95% prediction intervals for i.e., the likely spread of y values 
# given x not mean of y values given x for every observation in the data
treedat$predCI.M1<- predict(lm.volume.M1, treedat,interval="prediction", level=0.95)

names(treedat)
dim(treedat)
head(treedat)


# get confidence intervals for the mean volume for dbh=50 cm height=25 m and age=80 years
combinedvarnew<-50^2*25  # dbh^2*height

newvalues <- data.frame(combinedvar=combinedvarnew) 

pred.clim.M1 <- predict(lm.volume.M1, newvalues, interval="confidence",level=0.95) 
pred.clim.M1

pred.plim.M1 <- predict(lm.volume.M1, newvalues, interval="prediction",level=0.95)
pred.plim.M1

##### MODEL 2 #############################################################
          
lm.volume.M2<-lm(lnvolume~lndbh+lnht+lnage,data=treedat) #fit the model
lm.volume.M2  # get the estimated coefficients for the model
anova(lm.volume.M2)   # get the Anova table for the model, including the F test
summary(lm.volume.M2) # get t-tests and some fit statistics for the model
treedat$yhat.M2<-fitted(lm.volume.M2)  # the estimated y values
treedat$resid.M2<-resid(lm.volume.M2)  # the errors

names(treedat)
dim(treedat)
head(treedat)

# get diagnostic plots
par(mfrow=c(2,2),mai=c(0.5,0.5,0.5,0.5),cex=1.0)
plot(treedat$yhat.M2,treedat$resid.M2, main="Model 2, Residual Plot",
  ylab="yhat", xlab="residual")
plot(treedat$lnvolume,treedat$yhat.M2, main="Model 2, Fitted line plot",
  ylab="yhat", xlab="lnvolume")
qqnorm(treedat$resid.M2, main="Model 2, Normality plot")
hist(treedat$resid.M2, breaks =8 , density=10,col="green", border="black",
main="Model 2, Error Distribution") 
par(mfrow=c(1,1),mai=c(1.0,1.0,1.0,1.0),cex=1.0)

# get 95% confidence intervals for the mean volume (i.e., the yhats)
# for every observation in the data
treedat$CI.M2<- predict(lm.volume.M2, treedat,interval="confidence", level=0.95)

names(treedat)
dim(treedat)
head(treedat)

# get 95% prediction intervals for i.e., the likely spread of y values given x not mean
# of y values given x for every observation in the data
treedat$predCI.M2<- predict(lm.volume.M2, treedat,interval="prediction", level=0.95)

names(treedat)
dim(treedat)
head(treedat)


# get confidence intervals for the mean volume for dbh=50 cm height=25 m and age=80 years
lndbhnew<-log(50)
lnhtnew<-log(25)
lnagenew<-log(80)

newvalues <- data.frame(lndbh=lndbhnew,lnht=lnhtnew,lnage=lnagenew) 

A<-"Caution: Logarithm of volumes here"
pred.clim.M2 <- predict(lm.volume.M2, newvalues, interval="confidence",level=0.95) 
A
pred.clim.M2

pred.plim.M2 <- predict(lm.volume.M2, newvalues, interval="prediction",level=0.95)
pred.plim.M2

######################################################################## 

##  copy and paste code of Models 1 and 2 for Models 3 and 4

########################################################################

#####################  Part II #########################################

##### MODEL 5 #############################################################

#  First, make sure that crownclass is a factor
treedat$crownclass<-as.factor(treedat$crownclass) 
        
lm.volume.M5<-lm(lnvolume~crownclass+lndbh+lnht+lnage+crownclass*lndbh
  +crownclass*lnht+crownclass*lnage, data=treedat) #fit the model
lm.volume.M5  # get the estimated coefficients for the model
anova(lm.volume.M5)   # get the Anova table for the model, including the F test
summary(lm.volume.M5) # get t-tests and some fit statistics for the model
treedat$yhat.M5<-fitted(lm.volume.M5)  # the estimated y values
treedat$resid.M5<-resid(lm.volume.M5)  # the errors

names(treedat)
dim(treedat)
head(treedat)

# get diagnostic plots
par(mfrow=c(2,2),mai=c(0.5,0.5,0.5,0.5),cex=1.0)
plot(treedat$yhat.M5,treedat$resid.M5, main="Model 2, Residual Plot",
  ylab="yhat", xlab="residual")
plot(treedat$lnvolume,treedat$yhat.M5, main="Model 2, Fitted line plot",
  ylab="yhat", xlab="lnvolume")
qqnorm(treedat$resid.M5, main="Model 2, Normality plot")
hist(treedat$resid.M5, breaks =8 , density=10,col="green", border="black",
main="Model 1, Error Distribution") 
par(mfrow=c(1,1),mai=c(1.0,1.0,1.0,1.0),cex=1.0)

# get 95% confidence intervals for the mean volume (i.e., the yhats)
# for every observation in the data
treedat$CI.M5<- predict(lm.volume.M5, treedat,interval="confidence", level=0.95)

names(treedat)
dim(treedat)
head(treedat)

# get 95% prediction intervals for i.e., the likely spread of y values given x not mean
# of y values given x for every observation in the data
treedat$predCI.M5<- predict(lm.volume.M5, treedat,interval="prediction", level=0.95)

names(treedat)
dim(treedat)
head(treedat)


# get confidence intervals for the mean volume for dbh=50 cm height=25 m 
# age=80 years, and crownclass=1
lndbhnew<-log(50)
lnhtnew<-log(25)
lnagenew<-log(80)
cc<-1
crownclassnew<-as.factor(cc)

newvalues <- data.frame(lndbh=lndbhnew,lnht=lnhtnew,lnage=lnagenew,
  crownclass=crownclassnew) 

A<-"Caution: Logarithm of volumes here"
pred.clim.M5 <- predict(lm.volume.M5, newvalues, interval="confidence",
  level=0.95) 
A
pred.clim.M5

pred.plim.M5 <- predict(lm.volume.M5, newvalues, interval="prediction",
  level=0.95)
pred.plim.M5

### partial F test of crownclass using M2 versus M5
anova(lm.volume.M2, lm.volume.M5)

################################################################
############ copy and past the Model 5 code to 
### fit the model you selected -- Model 1 or other model



