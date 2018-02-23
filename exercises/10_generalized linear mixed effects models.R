#################################
# Generalized linear mixed-effects models
#################################
library(lme4)

# get the correct working directory

dat<-read.delim("rikz.txt")
str(dat)

mod_pois <- glmer(Richness ~ NAP + (1|Beach), dat=dat, family=poisson)
summary(mod_pois)

##############################################################
# diagnostics
par(mfrow=c(2,2))
plot(fitted(mod_pois), resid(mod_pois)) #residuals versus fitted
abline(h=0, lty=2)

qqnorm(resid(mod_pois), main="qqplot, residuals")  #qq plot of residuals
qqline(resid(mod_pois))

scatter.smooth(fitted(mod_pois), sqrt(abs(resid(mod_pois))))  # residual variance versus fitted

qqnorm(unlist(ranef(mod_pois)), main="qqplot random effects")
qqline(unlist(ranef(mod_pois)))  # qq plot of random effects

# the assumptions are better met than with the linear mixed model!
