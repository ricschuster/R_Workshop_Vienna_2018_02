library(MASS)
library(lmtest)
library(pscl)
library(sandwich)

data <- read.csv(file="data/bird_cnt.csv")

Creeper <- data$BRCR
Rural <- data$RUR_100Z
Crown_cl <- data$CR_CL_2Z
Road_up <- data$rd_up_100Z

#Creeper Histogram
tmp <- hist (Creeper, breaks=0:(max(Creeper)+1), main="Brown Creeper point count data",
  xlab="Number of individuals identified", ylab="Frequency", ylim=c(0,400), right=FALSE, axes=F)
axis(2)
axis(1, at=tmp$mids, labels=0:max(Creeper))


bird <- data.frame(cbind(Creeper, Rural, Crown_cl, Road_up))

pois<-glm(Creeper ~Rural + Crown_cl + Road_up,family = poisson, data = bird)
summary(pois)
coeftest(pois, vcov = sandwich)

qpois<-glm(Creeper ~Rural + Crown_cl + Road_up,family = quasipoisson, data = bird)
summary(qpois)

nb<-glm.nb(Creeper ~Rural + Crown_cl + Road_up,link = "log", data = bird)
summary(nb)


llhNB = logLik(nb)
llhPoisson  =logLik(pois)
d <- 2 * (llhNB - llhPoisson)
pval <- pchisq(as.numeric(d), df=1, lower.tail=FALSE)/2
pval

lrtest(pois,nb)

##ZERO INFLATION

f1 <- formula(Creeper ~ Rural + Crown_cl + Road_up
                      | Rural + Crown_cl + Road_up)

Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit",
              data = bird)
summary(Zip1)

f4 <- formula(Creeper ~ Rural + Crown_cl + Road_up
                      | Rural + Crown_cl + Road_up)

Nb4<- zeroinfl(f4, dist = "negbin", link = "logit",
              data = bird)
summary(Nb4)

lrtest(Nb4,Zip1)

llhZNB = logLik(Nb4)
llhZPoisson  =logLik(Zip1)
d <- 2 * (llhZNB - llhZPoisson)
pvalz <- pchisq(as.numeric(d), df=1, lower.tail=FALSE)/2
pvalz

#estimates
fm <- list("ML-Pois" = pois, "Quasi-Pois" = qpois, "NB" = nb,
    "ZI-Pois" = Zip1, "ZI-NB" = Nb4)
sapply(fm, function(x) coef(x)[1:8])

#stderrors
cbind("ML-Pois" = sqrt(diag(vcov(pois))),
  "Adj-Pois" = sqrt(diag(sandwich(pois))),
  sapply(fm[-1], function(x) sqrt(diag(vcov(x)))[1:8]))

#-2*LL
rbind(logLik = sapply(fm, function(x) logLik(x)),
  Df = sapply(fm, function(x) attr(logLik(x), "df")))

#AIC
rbind(AIC = sapply(fm, function(x) AIC(x)))


#expected to observed zero counts
round(c("Obs" = sum(Creeper < 1), "ML-Pois" = sum(dpois(0, fitted(pois))),
  "NB" = sum(dnbinom(0, mu = fitted(nb), size = nb$theta)),
  "ZI-Pois" = sum(predict(Zip1, type = "prob")[,1]),
  "ZI-NB" = sum(predict(Nb4, type = "prob")[,1])))

#Vuong tests (see Hilbe p. 176+)
vuong(Nb4,nb)
vuong(Zip1,pois)
