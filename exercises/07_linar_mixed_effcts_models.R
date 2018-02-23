#################################
# Linear mixed-effects models
#################################
library(lme4)

# get the correct working directory

dat<-read.delim("rikz.txt")
head(dat)
str(dat)

mod <- lmer(Richness ~ NAP + (1|Beach), dat=dat)
summary(mod)

library(lmerTest)  # get p-values 
summary(mod <- lmer(Richness~NAP + (1|Beach), dat=dat))

library(MuMIn)     # get R?
r.squaredGLMM(mod)
#marginal R? = variance explained by fixed factors
#conditional R? = variance explained by fixed and random factors


##############################################################
# diagnostics
par(mfrow=c(2,2))
plot(fitted(mod), resid(mod)) #residuals versus fitted
abline(h=0, lty=2)

qqnorm(resid(mod), main="qqplot, residuals")  #qq plot of residuals
qqline(resid(mod))

scatter.smooth(fitted(mod), sqrt(abs(resid(mod))))  # residual variance versus fitted

qqnorm(unlist(ranef(mod)), main="qqplot random effects")
qqline(unlist(ranef(mod)))  # qq plot of random effects

# conclusion: model fit is no entirely satisfying!!
# Because the residual variance seems to slightly increase with increasing values
# and the distribution of the residuals shows a heavier upper tail compared to the normal distribution
# -> maybe Poisson error distribution is a better choice


