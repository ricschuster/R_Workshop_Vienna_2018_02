#############################################################################################
# Generalized linear models - loistic regression to fit point count data  
# 06_GLM_birds.r UBC R workshop                      
#
#############################################################################################
library(MASS)
library(ROCR)

# read in data and create data frame 'brddata' and examine names and dimensions

brddata <- read.csv(file="data/PtCnt_FRST532B.csv")
dim(brddata)
names(brddata)

# Z indicates standardized variables
# URB_100Z - Urban/industrial area within a 100m buffer
# RUR_100Z - Rural area within a 100m buffer 
# FOR0_100Z - Forest cover in a 100 m buffer structural stages open and shrubby
# FOR1_100Z - Forest cover 100 m; structural stages closed and young forest
# FOR2_100Z - Forest cover 100 m; structural stages mature and old forest
# SAV_100Z - Savannah area within a 100m buffer
# SHR_100Z - Shrub area within a 100m buffer 
# CR_CL_2Z - Crown closure (26-60%) area within a 100m buffer (Landsat)
# CR_CL_3Z - Crown closure (>60%) area within a 100m buffer (Landsat)
# Is_sizeZ - Island size of plot location
# rd_up_100Z - Road length unpaved within a 100 m buffer
# rd_p_100Z - Road length paved within a 100 m buffer


###OLS fit.  This model does not make sense, but it shows that a 
# generalized linear model is the same as the usual linear model
# when a normal distribution (gaussian) is specified.
ols1 <- glm(CR_CL_2Z ~ Easting+Northing, family=gaussian, data=brddata)
summary(ols1)

###Check residual and other plots
###Set up four plots per page
par(mfrow=c(2,2))
plot(ols1)
 
## GLM: use glm() to fit mortality model for DF
brcr_m1 <- glm(BRCR ~ CR_CL_2Z, family=binomial, data=brddata)

##Examine the plot diagnostics. Plots will have the same interpretation as do those from OLS
par(las=1, mfrow=c(2,2), cex=0.75, mar=c(4,4,4,1))
plot(brcr_m1) 

summary(brcr_m1)

### Classification table
### Set threshold in this case the mean value is chose
### not run because ROCR used instead
### threshold<-mean(fitted(brcr_m1))
### table(brcr_m1$y,fitted(brcr_m1)>threshold)

### show ROCR plot and calculate AUC value
par(mfrow=c(1,1))
brcrpr <- prediction(fitted(brcr_m1), brcr_m1$y)
brcrpe <- performance(brcrpr, measure = "tpr", x.measure = "fpr")
brcrauc <- performance(brcrpr, measure = "auc")
plot(brcrpe, colorize=T)
brcrauc@y.values[[1]]

### Concordance table ###
### requires custom function: "Concordance.function.r" ###
source("Concordance.function.r")
Concordance(brcr_m1)

###Additional Predictors####
###Nested comparison of models [M(1) versus M(2)]
brcr_m2 <- glm(BRCR ~ CR_CL_2Z + rd_up_100Z + Is_sizeZ, family=binomial, data=brddata)
summary(brcr_m2)

##Generalized likelihood ratio test (uses Deviance) 
##Note the order is important
anova(brcr_m2, test= "Chisq")

brcr_m3 <- glm(BRCR ~ CR_CL_2Z + rd_up_100Z, family=binomial, data=brddata)
anova(brcr_m3, test= "Chisq")

##Wald test and coefficients 
summary(brcr_m3)

#Extract AICs and compare Models 1, 2 and 3
AIC(brcr_m1)
AIC(brcr_m2)
AIC(brcr_m3)

## Proportion of deviance explained by each model
## (null-residual deviance)/null.deviance 
r1 <- (brcr_m1$null.deviance-deviance(brcr_m1))/brcr_m1$null.deviance
r2 <- (brcr_m2$null.deviance-deviance(brcr_m2))/brcr_m2$null.deviance
r3 <- (brcr_m3$null.deviance-deviance(brcr_m3))/brcr_m3$null.deviance 
(rsq <- cbind(r1, r2,r3))

##Estimate dispersion parameter estimates for Model 3
###

deviance(brcr_m3)
summary(brcr_m3)$dispersion    ## 1 (by definition)
dfr <- df.residual(brcr_m3)
deviance(brcr_m3)/dfr  
d3 <- sum(residuals(brcr_m3,"pearson")^2)  
disp3 <- d3/dfr    

gg3 <- update(brcr_m3,family=quasipoisson)
summary(gg3)$dispersion   

#pchisq(d3,df=dfr,lower.tail=FALSE)

all.equal(coef(brcr_m3),coef(gg3)) ## TRUE
se1 <- coef(summary(brcr_m3))[,"Std. Error"]
se2 <- coef(summary(gg3))[,"Std. Error"]
se2/se1

sqrt(disp3)

#str(brcr_m3)

par(mfrow=c(2,2))
plot(brcr_m1)

#1-pchisq(599.18, 2199)

##Deviance or likelihood ratio test
##Note the order is important
brcr_m3 <- glm(BRCR ~ CR_CL_2Z + rd_up_100Z, family=binomial, data=brddata)

anova(brcr_m2, test= "Chisq")
anova(brcr_m3, test= "Chisq")

#Generalized likelihoof ratio test
anova(brcr_m1,brcr_m2, brcr_m3, test= "Chisq")

summary(brcr_m3)$coef

##Model fit using quasibinomial
brcr_m4 <- glm(BRCR ~ CR_CL_2Z + rd_up_100Z, family=quasibinomial, data=brddata)

summary(brcr_m4)

anova(brcr_m4, test= "Chisq")

