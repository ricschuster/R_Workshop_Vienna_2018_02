################################
# Model averaging
################################

library(MuMIn)

# Example from Burnham and Anderson (2002), page 100:
fm1 <- lm(y ~ X1 + X2 + X3 + X4, data = Cement)

options(na.action=na.fail)   # to avoid an error message
all_models <- dredge(fm1, rank="AICc") #all model combinations
all_models

plot(Weights(all_models))  #plots weights of all models

submod<-subset(all_models, delta<7) # model combinations less than AIC delta 7 (often also 2 is used!) as the 'best' models
submod   # here the null model should not be included!

avg_mod <- model.avg(submod)  # model averaging
summary(avg_mod)              # mostly use full averaging - conditional only if on model is miuch better than the others

plot(importance(avg_mod))

confint(avg_mod)  # caution this calculation is not absolutely correct, rather an approximation
# correct estimation only works with bayesian methods

