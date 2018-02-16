# Collection of functions to check the distribution of data 
# by Matthias-Claudio Loretto # 24.02.2017 # 

# Which distribution fits my data best?

#create data
a<-rnorm(n = 1000, mean = 600, sd = 30)  #normal
b<-rgamma(n=1000, scale=1, shape=0.8)    #gamma
c<-rnbinom(1000, mu = 4, size = 1)       #negativ binomial

#Histograms
hist(a, n=50)
hist(b, n=50)
hist(c, n=50)

###################
library(MASS)
#comparing histograms of real data and simulated data

tt<-fitdistr(b,"gamma")
par(mfrow=c(2,1))
hist(b,n=20)
hist(rgamma(10000,tt$estimate[1],tt$estimate[2]),n=20)
par(mfrow=c(1,1))

##################
library(car)
#here the points should be between the dashed lines
#continuous
qqp(b, "norm")
qqp(b, "lnorm")
gamma <- fitdistr(b, "gamma")
qqp(b, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

#discrete
nbinom <- fitdistr(c, "Negative Binomial")
qqp(c, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(c, "Poisson")
qqp(c, "pois", poisson$estimate)

##################
library(fitdistrplus)
#more diagnostic plots
#first get some ideas about possible distributions #but maybe not too effective/informative?
descdist(b, discrete = FALSE)     
descdist(c, discrete = TRUE)

#testing distributions
#continuos
fit.gamma<-fitdist(b, "gamma")
plot(fit.gamma)
fit.norm<-fitdist(b, "norm")
plot(fit.norm)

#discrete
fit.nbinom<-fitdist(c, "nbinom")
plot(fit.nbinom)
fit.pois<-fitdist(c, "pois")
plot(fit.pois)

##############
# non visual based on AIC of fitted distribution # the lower the better
fit.gamma$aic
fit.norm$aic

fit.nbinom$aic
fit.pois$aic

#or direct comparison
gofstat(list(fit.norm, fit.gamma))
gofstat(list(fit.nbinom, fit.pois))

#test for normality
shapiro.test(a)       #test for normality

#############
#beta distribution here treated separately since values have to be between 0 and 1
d<-rbeta(1000,1,5)
hist(d, n=50)

fit.beta<-fitdist(d, "beta")  
plot(fit.beta)

# previous function sometimes have errors with start parameters are
## Define function to estimate parameters of a beta distribution.
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

## Calculate mean and variance of the data.
mu.PO <- mean(d)
var.PO <- var(d)
## Apply function to your data.
bt.PO<-estBetaParams(mu.PO, var.PO)

par(mfrow=c(2,1))
hist(d,n=50)
hist(rbeta(10000,bt.PO$alpha, bt.PO$beta),n=50)
par(mfrow=c(1,1))

###########

