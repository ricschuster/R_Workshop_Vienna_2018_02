#####################################################
# Topic 4: normal linear regression 
# checking assumptions
#####################################################

# some examples from R-bloggers
# https://www.r-bloggers.com/checking-glm-model-assumptions-in-r/


#####################################################
# testing model assumptions on simulated data
x <- runif(100, 0, 10)
y <- 1 + 2 * x + rnorm(100, 0, 1)
m <- lm(y ~ x)   # model

par(mfrow = c(2, 2))
plot(m)

# "bad" model
y <- 1 + 2 * x + 1 * x^2 - 0.5 * x^3
m_b <- lm(y ~ x)

plot(m_b)

par(mfrow = c(1, 1))


###################################################
#### tree data
setwd("...")

trees<- read.table("trees.txt",header=TRUE)
tree_model1<-lm(volume~height,data=trees)
plot(tree_model1)


####################################################################
#assumptions, collinearity and autocorrelation based on real data 
data(cars)       # using the cars dataset from base R 

mod_car <- lm(mpg ~ ., data=mtcars) 
plot(mod_car)

cor(mtcars[1:5])        #correlation between columns 1-5
plot(mtcars[1:5], panel=panel.smooth)
pairs(mtcars[1:5])

###
library(car) # load the car package which has the vif()
vif(mod_car)

### autocorrelation
acf(cars)

acf(residuals(mod_car))



############################################################################
# qq plots -  normal distribution
# A nice way to see if the patterns are different from those expected under the model conditions 
# is to derive new response values from the fitted coefficient and the residual variance. 
# With the following code you can then derive 8 new plots and randomly allocate the real plot 
# to a position. If you are able to find the real plot and if its pattern are different from 
# the others then the model does not meet its assumptions:

mod <- lm(dist ~ speed, data=cars) 

#not norm_vert
a<-rgamma(n=100, scale=1, shape=0.8)
b<-rnorm(n = 100, mean = 600, sd = 30) 
ab<-cbind.data.frame(a,b)

cbind.data.frame()
mod <- lm(a ~ b, data=ab)

N<-length(resid(mod))
sigma<-summary(mod)$sigma
par(mfrow=c(3,3))
rnum<-sample(1:9, 1)
for(i in 1:(rnum-1)){
  x<-rnorm(N, 0, sigma)
  qqnorm(x, main=i)
  qqline(x)
}
qqnorm(resid(mod), main=rnum)
qqline(resid(mod))
for(i in (rnum+1) :9) {
  x<-rnorm(N, 0, sigma)
  qqnorm(x, main=i)
  qqline(x)
}

rnum    #gives you the number of the plot with the model data

##############################################################