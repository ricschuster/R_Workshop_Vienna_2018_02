

###############################################################
#    R and WinBUGS code for for the following book:           #
#    Kéry (2010) Introduction to WinBUGS for ecologists.      # 
#    Academic Press, Burlington.			      #
###############################################################


# This document contains all the R and WinBUGS code from the book. 
# To execute the code, you can copy-paste parts of if into an 
# open R window. Alternatively, you can open the text file in 
# Tinn-R (though beware of problems with sink(); check the book 
# appendix for a workaround) or any other R editor.

# Code that is contiguous in the book is contiguous in this 
# document. Code that is separated by one or more lines of 
# non-code text is separated by one line in this document.

# All code written and tried out with R 2.8.1. and WinBUGS 1.4.

# Document created on 31 May 2010 by Marc Kéry 
# Last changes: 24 June 2010



# You may have to add a 'working.directory' argument to calls to
# the function bugs().




###############################################################
#        There is no code in chapters 1-4.                    #
###############################################################



###############################################################
#        Chapter 5: Model of the mean in WinBUGS from R       #
###############################################################



### 5.2. Data generation
# Generate two samples of body mass measurements of male peregrines
y10 <- rnorm(n = 10, mean = 600, sd = 30) # Sample of 10 birds
y1000 <- rnorm(n = 1000, mean = 600, sd = 30) # Sample of 1000 birds

# Plot data
xlim = c(450, 750)
par(mfrow = c(2,1))
hist(y10, col = 'grey ', xlim = xlim, main = 'Body mass (g) of 10 male peregrines')
hist(y1000, col = 'grey', xlim = xlim, main = ' Body mass (g) of 1000 male peregrines')



### 5.3. Analysis using R
summary(lm(y1000 ~ 1))



### 5.4. Analysis using WinBUGS
library(R2WinBUGS)		# Load the R2WinBUGS library
setwd("C:/_Marc Kery/_WinBUGS book/Naked code") # May have to adapt that

# Save BUGS description of the model to working directory
sink("model.txt")
cat("
model {

# Priors
 population.mean ~ dunif(0,5000)		# Normal parameterized by precision
 precision <- 1 / population.variance	# Precision = 1/variance
 population.variance <- population.sd * population.sd
 population.sd ~ dunif(0,100)

# Likelihood
 for(i in 1:nobs){
    mass[i] ~ dnorm(population.mean, precision)
 }
}
",fill=TRUE)
sink()

# Package all the stuff to be handed over to WinBUGS
# Bundle data
win.data <- list(mass = y1000, nobs = length(y1000))

# Function to generate starting values
inits <- function()
  list (population.mean = rnorm(1,600), population.sd = runif(1, 1, 30))

# Parameters to be monitored (= to estimate)
params <- c("population.mean", "population.sd", "population.variance")

# MCMC settings
nc <- 3					# Number of chains
ni <- 1000				# Number of draws from posterior (for each chain)
nb <- 1					# Number of draws to discard as burn-in
nt <- 1					# Thinning rate

# Start Gibbs sampler: Run model in WinBUGS and save results in object called out
out <- bugs(data = win.data, inits = inits, parameters.to.save = params, model.file = "model.txt", 
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, debug = TRUE, DIC = TRUE, working.directory = getwd())

ls()

out					# Produces a summary of the object

names(out)

str(out)

hist(out$summary[,8])			# Rhat values in the eighth column of the summary
which(out$summary[,8] > 1.1)		# None in this case

par(mfrow = c(3,1))
matplot(out$sims.array[1:999,1:3,1], type = "l")
matplot(out$sims.array[,,2] , type = "l")
matplot(out$sims.array[,,3] , type = "l")

par(mfrow = c(3,1))
matplot(out$sims.array[1:20,1:3,1], type = "l")
matplot(out$sims.array[1:20,,2] , type = "l")
matplot(out$sims.array[1:20,,3] , type = "l")

par(mfrow = c(3,1))
hist(out$sims.list$population.mean, col = "grey")
hist(out$sims.list$population.sd, col = "blue")
hist(out$sims.list$population.variance, col = "green")

par(mfrow = c(1,1))
plot(out$sims.list$population.mean, out$sims.list$population.sd)

pairs(cbind(out$sims.list$population.mean, out$sims.list$population.sd, out$sims.list$population.variance))

summary(out$sims.list$population.mean)
summary(out$sims.list$population.sd)
sd(out$sims.list$population.mean)
sd(out$sims.list$population.sd)

summary(lm(y1000 ~ 1))






###############################################################
#        Chapter 6: Essentials of linear models               #
###############################################################



### 6.2. Stochastic part of linear models: Statistical distributions


### 6.2.1. Normal distribution
n <- 100000				# Sample size
mu <- mean <- 600			# Body mass of male peregrines
sd <- st.dev <- 30			# SD of body mass of male peregrines

sample <- rnorm(n = n, mean = mu, sd = sd)
print(sample, dig = 4)
hist(sample, col = "grey")


### 6.2.2. Continuous uniform distribution
n <- 100000				# Sample size
a <- lower.limit <- 0
b <- upper.limit <- 10

sample <- runif(n = n, min = a, max = b)
print(sample, dig = 3)
hist(sample, col = "grey")


### 6.2.3. Binomial distribution: The "coin-flip distribution"
n <- 100000				# Sample size
N <- 16					# Number of individuals that flip the coin 
p <- 0.8				# Probability of being counted (seen), dead or a male

sample <- rbinom(n = n, size = N, prob = p)
print(sample, dig = 3)
hist(sample, col = "grey")


### 6.2.4. Poisson distribution
n <- 100000				# Sample size
lambda <- 5				# Average no. individuals per sample, density 

sample <- rpois(n = n, lambda = lambda)
print(sample, dig = 3)

par(mfrow = c(2,1))
hist(sample, col = "grey", main = "Default histogram")
plot(table(sample), main = "A better graph", lwd = 3, ylab = "Frequency")



### 6.3. Deterministic part of linear models: Linear predictor and design matrices
mass <- c(6, 8, 5, 7, 9, 11)
pop <- factor(c(1,1,2,2,3,3))
region <- factor(c(1,1,1,1,2,2))
hab <- factor(c(1,2,3,1,2,3))
svl <- c(40, 45, 39, 50, 52, 57)


### 6.3.1. The model of the mean
lm(mass ~ 1)

model.matrix(mass~1)


### 6.3.2. t-Test
lm(mass ~ region)

model.matrix(mass ~region)

lm(mass~region)

model.matrix(~region-1)

lm(mass~region-1)


### 6.3.3. Simple linear regression
lm(mass ~ svl)

model.matrix(mass ~svl)

lm(mass~svl)
fm1 <- lm(mass~svl)

plot(svl, mass, pch=19)
abline(lm(mass~svl), col="red")

model.matrix(~svl-1)

lm(mass~svl-1)


### 6.3.4. One-way analysis of variance (one-way ANOVA)
lm(mass ~ pop)

model.matrix(~pop)

model.matrix(~pop-1)

lm(mass~pop)				# Effects parameterization (R default)

lm(mass~pop-1)				# Means parameterization


### 6.3.5. Two-way analysis of variance (two-way ANOVA)
lm(mass ~ region + hab)

model.matrix(~region + hab)

lm(mass ~ region * hab)

model.matrix(~region * hab)

lm(mass ~ region * hab-1-region-hab)

model.matrix(~ region * hab-1-region-hab)


### 6.3.6. Analysis of covariance (ANCOVA)
lm(mass ~ pop + svl)			# Additive model
lm(mass ~ pop * svl)			# Interactive model
lm(mass ~ pop + svl + pop:svl) 	# Same, R’s way of specifying the interaction term

model.matrix(lm(mass ~ pop + svl))	# Additive model

model.matrix(lm(mass ~ pop * svl))	# Interactive model

model.matrix(lm(mass ~ pop + svl-1))	# Additive model

model.matrix(lm(mass ~ (pop * svl - 1 - svl))) # Interactive model

lm(mass ~ pop + svl)

abline(lm(mass~svl), col="red")
fm <- lm(mass ~ pop + svl)		# Refit model
plot(svl, mass, col = c(rep("red", 2), rep("blue", 2), rep("green", 2)), pch=19, main = "ANCOVA")
abline(fm$coef[1], fm$coef[4], col = "red")
abline(fm$coef[1]+ fm$coef[2], fm$coef[4], col = "blue")
abline(fm$coef[1]+ fm$coef[3], fm$coef[4], col = "green")

lm(mass ~ pop * svl)

fm <- lm(mass ~ pop * svl)		# Refit model
plot(svl, mass, col = c(rep("red", 2), rep("blue", 2), rep("green", 2)), pch=19, main = "ANCOVA + Interaction")
abline(fm$coef[1], fm$coef[4], col = "red")
abline(fm$coef[1]+ fm$coef[2], fm$coef[4] + fm$coef[5], col = "blue")
abline(fm$coef[1]+ fm$coef[3], fm$coef[4] + fm$coef[6], col = "green")

lm(mass ~ pop + svl-1)

lm(mass ~ pop * svl-1 - svl)






###############################################################
#                   Chapter 7: T-test                         #
###############################################################



### 7.1. T-test with equal variances


### 7.1.2. Data generation
n1 <- 60				# Number of females
n2 <- 40				# Number of males
mu1 <- 105				# Population mean of females
mu2 <- 77.5				# Population mean of males
sigma <- 2.75				# Average population SD of both

n <- n1+n2				# Total sample size
y1 <- rnorm(n1, mu1, sigma)		# Data for females
y2 <- rnorm(n2, mu2, sigma)		# Date for males
y <- c(y1, y2)				# Aggregate both data sets
x <- rep(c(0,1), c(n1, n2))		# Indicator for male
boxplot(y ~ x, col = "grey", xlab = "Male", ylab = "Wingspan (cm)", las = 1)

n <- n1+n2				# Total sample size
alpha <- mu1				# Mean for females serves as the intercept
beta <- mu2-mu1				# Beta is the difference male-female
E.y <- alpha + beta*x			# Expectation
y.obs <- rnorm(n = n, mean = E.y, sd = sigma)	# Add random variation
boxplot(y.obs ~ x, col = "grey", xlab = "Male", ylab = "Wingspan (cm)", las = 1)


### 7.1.3. Analysis using R
fit1 <- lm(y ~ x)			# Analysis of first data set
fit2 <- lm(y.obs ~ x)			# Analysis of second data set
summary(fit1)
summary(fit2)

anova(fit1)
anova(fit2)

model.matrix(fit1)
model.matrix(fit2)


### 7.1.4. Analysis using WinBUGS
# Define BUGS model
sink("ttest.txt")
cat("
model {

# Priors
 mu1 ~ dnorm(0,0.001)			# Precision = 1/variance
 delta ~ dnorm(0,0.001)			# Large variance = Small precision
 tau <- 1/ (sigma * sigma)
 sigma ~ dunif(0, 10)

# Likelihood
 for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau) 
    mu[i] <- mu1 + delta *x[i]
    residual[i] <- y[i] - mu[i]		# Define residuals
 }

# Derived quantities: one of the greatest things about a Bayesian analysis
 mu2 <- mu1 + delta			# Difference in wingspan
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list("x", "y", "n")

# Inits function
inits <- function(){list(mu1=rnorm(1), delta=rnorm(1), sigma = rlnorm(1))}

# Parameters to estimate
params <- c("mu1","mu2", "delta", "sigma", "residual")

# MCMC settings
nc <- 3		# Number of chains
ni <- 1000	# Number of draws from posterior for each chain
nb <- 1		# Number of draws to discard as burn-in
nt <- 1		# Thinning rate

# Start Gibbs sampler
out <- bugs(data = win.data, inits = inits, parameters = params, model = "ttest.txt", 
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, debug = TRUE, working.directory = getwd())

print(out, dig = 3)

plot(1:100, out$mean$residual)
abline(h = 0)

boxplot(out$mean$residual ~ x, col = "grey", xlab = "Male", ylab = "Wingspan residuals (cm)", las = 1)
abline(h = 0)



### 7.2. T-test with unequal variances


### 7.2.2. Data generation
n1 <- 60				# Number of females
n2 <- 40				# Number of males
mu1 <- 105				# Population mean for females
mu2 <- 77.5				# Population mean for males
sigma1 <- 3				# Population SD for females
sigma2 <- 2.5				# Population SD for males

n <- n1+n2				# Total sample size
y1 <- rnorm(n1, mu1, sigma1)		# Data for females
y2 <- rnorm(n2, mu2, sigma2)		# Data for males
y <- c(y1, y2)				# Aggregate both data sets
x <- rep(c(0,1), c(n1, n2))		# Indicator for male
boxplot(y ~ x, col = "grey", xlab = "Male", ylab = "Wingspan (cm)", las = 1)


### 7.2.3. Analysis using R
t.test(y ~ x)


### 7.2.4. Analysis using WinBUGS
# Define BUGS model
sink("h.ttest.txt")
cat("
model {

# Priors
 mu1 ~ dnorm(0,0.001)
 mu2 ~ dnorm(0,0.001)
 tau1 <- 1 / ( sigma1 * sigma1)
 sigma1 ~ dunif(0, 1000) 		# Note: Large var. = Small precision
 tau2 <- 1 / ( sigma2 * sigma2)
 sigma2 ~ dunif(0, 1000)

# Likelihood
 for (i in 1:n1) {
    y1[i] ~ dnorm(mu1, tau1) 
 }

 for (i in 1:n2) {
    y2[i] ~ dnorm(mu2, tau2) 
 }

# Derived quantities
 delta <- mu2 - mu1
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list("y1", "y2", "n1", "n2")

# Inits function
inits <- function(){ list(mu1=rnorm(1), mu2=rnorm(1), sigma1 = rlnorm(1), sigma2 = rlnorm(1))}

# Parameters to estimate
params <- c("mu1","mu2", "delta", "sigma1", "sigma2")

# MCMC settings
nc <- 3					# Number of chains
ni <- 2000				# Number of draws from posterior for each chain
nb <- 500				# Number of draws to discard as burn-in
nt <- 1					# Thinning rate

# Unleash Gibbs sampler
out <- bugs(data = win.data, inits = inits, parameters = params, model = "h.ttest.txt", 
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, debug = TRUE)

print(out, dig = 3)






###############################################################
#            Chapter 8: Normal linear regression              #
###############################################################



### 8.2. Data generation
n <- 16					# Number of years
a = 40					# Intercept
b = -1.5				# Slope
sigma2 = 25				# Residual variance

x <- 1:16 				# Values of covariate year
eps <- rnorm(n, mean = 0, sd = sqrt(sigma2))
y <- a + b*x + eps			# Assemble data set
plot((x+1989), y, xlab = "Year", las = 1, ylab = "Prop. occupied (%)", cex = 1.2)



### 8.3. Analysis using R
print(summary(lm(y ~ I(x+1989))))
abline(lm(y~ I(x+1989)), col = "blue", lwd = 2)



### 8.4. Analysis using WinBUGS


### 8.4.1. Fitting the model
# Write model
sink("linreg.txt")
cat("
model {

# Priors
 alpha ~ dnorm(0,0.001)
 beta ~ dnorm(0,0.001)
 sigma ~ dunif(0, 100)

# Likelihood
 for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau) 
    mu[i] <- alpha + beta*x[i]
 }

# Derived quantities
 tau <- 1/ (sigma * sigma)
 p.decline <- 1-step(beta)		# Probability of decline

# Assess model fit using a sums-of-squares-type discrepancy
 for (i in 1:n) {
    residual[i] <- y[i]-mu[i]		# Residuals for observed data
    predicted[i] <- mu[i]		# Predicted values
    sq[i] <- pow(residual[i], 2)	# Squared residuals for observed data

# Generate replicate data and compute fit stats for them
    y.new[i] ~ dnorm(mu[i], tau) # one new data set at each MCMC iteration
    sq.new[i] <- pow(y.new[i]-predicted[i], 2)	# Squared residuals for new data
 }
 fit <- sum(sq[])			# Sum of squared residuals for actual data set
 fit.new <- sum(sq.new[])		# Sum of squared residuals for new data set
 test <- step(fit.new - fit)		# Test whether new data set more extreme
 bpvalue <- mean(test)			# Bayesian p-value
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list("x","y", "n")

# Inits function
inits <- function(){ list(alpha=rnorm(1), beta=rnorm(1), sigma = rlnorm(1))}

# Parameters to estimate
params <- c("alpha","beta", "p.decline", "sigma", "fit", "fit.new", "bpvalue", "residual", "predicted")

# MCMC settings
nc = 3  ;  ni=1200  ;  nb=200  ;  nt=1

# Start Gibbs sampler
out <- bugs(data = win.data, inits = inits, parameters = params, model = "linreg.txt", 
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, debug = TRUE)

print(out, dig = 3)


### 8.4.2. Goodness-of-Fit assessment in Bayesian analyses
plot(out$mean$predicted, out$mean$residual, main = "Residuals vs. predicted values", 
las = 1, xlab = "Predicted values", ylab = "Residuals")
abline(h = 0)

lim <- c(0, 3200)
plot(out$sims.list$fit, out$sims.list$fit.new, main = "Graphical posterior predictive check", 
las = 1, xlab = "SSQ for actual data set", ylab = "SSQ for ideal (new) data sets", xlim = lim, ylim = lim)
abline(0, 1)

mean(out$sims.list$fit.new > out$sims.list$fit)	# Bayesian p-value


### 8.4.3. Forming predictions
plot((x+1989), y, xlab = "Year", las = 1, ylab = "Prop. occupied (%)", cex = 1.2)
abline(lm(y~ I(x+1989)), col = "blue", lwd = 2)
pred.y <- out$mean$alpha + out$mean$beta * x
points(1990:2005, pred.y, type = "l", col = "red", lwd = 2)
text(1994, 20, labels = "blue – ML; red - MCMC", cex = 1.2)

predictions <- array(dim = c(length(x), length(out$sims.list$alpha)))
for(i in 1:length(x)){
   predictions[i,] <- out$sims.list$alpha + out$sims.list$beta*i
}
LPB <- apply(predictions, 1, quantile, probs = 0.025) # Lower bound
UPB <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound

plot((x+1989), y, xlab = "Year", las = 1, ylab = "Prop. occupied (%)", cex = 1.2)
points(1990:2005, out$mean$alpha + out$mean$beta * x, type = "l", col = "black", lwd = 2)
points(1990:2005, LPB, type = "l", col = "grey", lwd = 2)
points(1990:2005, UPB, type = "l", col = "grey", lwd = 2)


### 8.4.4. Interpretation of confidence vs. credible intervals
hist(out$sims.list$beta, main = "", col = "grey", xlab = "Trend estimate", xlim = c(-4, 0))
abline(v = 0, col = "black", lwd = 2)






###############################################################
#            Chapter 9: Normal one-way ANOVA                  #
###############################################################



### 9.2. Fixed-effects ANOVA


### 9.2.1. Data generation
ngroups <- 5				# Number of populations
nsample <- 10				# Number of snakes in each
pop.means <- c(50, 40, 45, 55, 60) 	# Population mean SVL
sigma <- 3				# Residual sd

n <- ngroups * nsample 			# Total number of data points
eps <- rnorm(n, 0, sigma)		# Residuals 
x <- rep(1:5, rep(nsample, ngroups)) 	# Indicator for population
means <- rep(pop.means, rep(nsample, ngroups))
X <- as.matrix(model.matrix(~ as.factor(x)-1)) # Create design matrix
X					# Inspect that
y <- as.numeric(X %*% as.matrix(pop.means) + eps) # assemble -- NOTE: as.numeric ESSENTIAL for WinBUGS
					# %*% denotes matrix multiplication
boxplot(y~x, col="grey", xlab="Population", ylab="SVL", main="", las = 1)


### 9.2.2. Maximum likelihood analysis using R
print(anova(lm(y~as.factor(x))))
cat("\n")
print(summary(lm(y~as.factor(x)))$coeff, dig = 3)
cat("Sigma:         ", summary(lm(y~as.factor(x)))$sigma, "\n")


### 9.2.3. Bayesian analysis using WinBUGS
# Write model
sink("anova.txt")
cat("
model {

# Priors
 for (i in 1:5){			# Implicitly define alpha as a vector
    alpha[i] ~ dnorm(0, 0.001)
 }
 sigma ~ dunif(0, 100)

# Likelihood
 for (i in 1:50) {
    y[i] ~ dnorm(mean[i], tau) 
    mean[i] <- alpha[x[i]]
 }

# Derived quantities
 tau <- 1 / ( sigma * sigma)
 effe2 <- alpha[2] - alpha[1]
 effe3 <- alpha[3] - alpha[1]
 effe4 <- alpha[4] - alpha[1]
 effe5 <- alpha[5] - alpha[1]

# Custom hypothesis test / Define your own contrasts
 test1 <- (effe2+effe3) - (effe4+effe5) # Equals zero when 2+3 = 4+5
 test2 <- effe5 - 2 * effe4 		# Equals zero when effe5 = 2*effe4
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list("y", "x")

# Inits function
inits <- function(){ list(alpha = rnorm(5, mean = mean(y)), sigma = rlnorm(1) )}

# Parameters to estimate
params <- c("alpha", "sigma", "effe2", "effe3", "effe4", "effe5", "test1", "test2")

# MCMC settings
ni <- 1200
nb <- 200
nt <- 2
nc <- 3

# Start Gibbs sampling
out <- bugs(win.data, inits, params, "anova.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug = TRUE)

# Inspect estimates
print(out, dig = 3)



### 9.3. Random-effects ANOVA


### 9.3.1. Data generation
npop <- 10				# Number of populations: now choose 10 rather than 5
nsample <- 12				# Number of snakes in each
n <- npop * nsample			# Total number of data points

pop.grand.mean <- 50			# Grand mean SVL
pop.sd <- 5				# sd of population effects about mean
pop.means <- rnorm(n = npop, mean = pop.grand.mean, sd = pop.sd)
sigma <- 3				# Residual sd
eps <- rnorm(n, 0, sigma) 		# Draw residuals

x <- rep(1:npop, rep(nsample, npop))
X <- as.matrix(model.matrix(~ as.factor(x)-1))
y <- as.numeric(X %*% as.matrix(pop.means) + eps) # as.numeric is ESSENTIAL
boxplot(y ~ x, col = "grey", xlab = "Population", ylab = "SVL", main = "", las = 1) # Plot of generated data
abline(h = pop.grand.mean)


### 9.3.2. Restricted maximum likelihood (REML) analysis using R
library('lme4')				# Load lme4

pop <- as.factor(x)			# Define x as a factor and call it pop

lme.fit <- lmer(y ~ 1 + 1 | pop, REML = TRUE)
lme.fit					# Inspect results
ranef(lme.fit)				# Print random effects


### 9.3.3. Bayesian analysis using WinBUGS
sink("re.anova.txt")
cat("
model {

# Priors and some derived things
for (i in 1:npop){
    pop.mean[i] ~ dnorm(mu, tau.group) 	# Prior for population means
    effe[i] <- pop.mean[i] - mu 	# Population effects as derived quant’s
 }
 mu ~ dnorm(0,0.001)			# Hyperprior for grand mean svl
 sigma.group ~ dunif(0, 10)		# Hyperprior for sd of population effects
 sigma.res ~ dunif(0, 10)		# Prior for residual sd

# Likelihood
 for (i in 1:n) {
    y[i] ~ dnorm(mean[i], tau.res)
    mean[i] <- pop.mean[x[i]]
 }

# Derived quantities
 tau.group <- 1 / (sigma.group * sigma.group)
 tau.res <- 1 / (sigma.res * sigma.res)
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(y=y, x=x, npop = npop, n = n)

# Inits function
inits <- function(){ list(mu = runif(1, 0, 100), sigma.group = rlnorm(1), sigma.res = rlnorm(1) )}

# Params to estimate
parameters <- c("mu", "pop.mean", "effe", "sigma.group", "sigma.res")

# MCMC settings
ni <- 1200
nb <- 200
nt <- 2
nc <- 3

# Start WinBUGS
out <- bugs(win.data, inits, parameters, "re.anova.txt", n.thin=nt, 
n.chains=nc, n.burnin=nb, n.iter=ni, debug = TRUE)

# Inspect estimates
print(out, dig = 3)






###############################################################
#            Chapter 10: Normal two-way ANOVA                 #
###############################################################



### 10.2. Data generation
# Choose sample size
n.pop <- 5
n.elev <- 3
nsample <- 12
n <- n.pop * nsample

# Create factor levels
pop <- gl(n = n.pop, k = nsample, length = n)
elev <- gl(n = n.elev, k = nsample / n.elev, length = n)

# Choose effects
baseline <- 40	# Intercept
pop.effects <- c(-10, -5, 5, 10) # Population effects
elev.effects <- c(5, 10)	# Elev effects
interaction.effects <- c(-2, 3, 0, 4, 4, 0, 3, -2)	# Interaction effects
all.effects <- c(baseline, pop.effects, elev.effects, interaction.effects)

sigma <- 3
eps <- rnorm(n, 0, sigma)		# Residuals

X <- as.matrix(model.matrix(~ pop*elev) ) # Create design matrix
X					# Have a look at that

wing <- as.numeric(as.matrix(X) %*% as.matrix(all.effects) + eps)
	# NOTE: as.numeric is ESSENTIAL for WinBUGS later
boxplot(wing ~ elev*pop, col = "grey", xlab = "Elevation-by-Population", ylab = "Wing length", 
main = "Simulated data set", las = 1, ylim = c(20, 70))	# Plot of generated data
abline(h = 40)

library("lattice")			# Load the lattice library
xyplot(wing ~ elev | pop, ylab = "Wing length", xlab = "Elevation", 
main = "Population-specific relationship between wing and elevation class")
xyplot(wing ~ pop | elev, ylab = "Wing length", xlab = "Population", 
main = "Elevation-specific relationship between wing and population")



### 10.3. Aside: Using simulation to assess bias and precision of an estimator
lm(wing ~ pop*elev)
all.effects

n.iter <- 1000				# Desired number of iterations
estimates <- array(dim = c(n.iter, length(all.effects))) # Data structure to hold results

for(i in 1:n.iter) {			# Run simulation n.iter times
   print(i)				# Optional
   eps <- rnorm(n, 0, sigma)		# Residuals 
   y <- as.numeric(as.matrix(X) %*% as.matrix(all.effects) + eps) # Assemble data
   fit.model <- lm(y ~ pop*elev)	# Break down data
   estimates[i,] <- fit.model$coefficients # Keep values of coefs.
}

print(apply(estimates, 2, mean), dig = 2)
all.effects



### 10.4. Analysis using R
mainfit <- lm(wing ~ elev + pop)
mainfit

intfit <- lm(wing ~ elev*pop-1-pop-elev)
intfit



### 10.5. Analysis using WinBUGS


### 10.5.1. Main-effects ANOVA using WinBUGS
# Define model
sink("2w.anova.txt")
cat("
model {

# Priors
 alpha ~ dnorm(0, 0.001)		# Intercept
 beta.pop[1] <- 0			# set to zero effect of 1st level
 beta.pop[2] ~ dnorm(0, 0.001)
 beta.pop[3] ~ dnorm(0, 0.001)
 beta.pop[4] ~ dnorm(0, 0.001)
 beta.pop[5] ~ dnorm(0, 0.001)
 beta.elev[1] <- 0			# ditto
 beta.elev[2] ~ dnorm(0, 0.001)
 beta.elev[3] ~ dnorm(0, 0.001)
 sigma ~ dunif(0, 100)

# Likelihood
 for (i in 1:n) {
    wing[i] ~ dnorm(mean[i], tau) 
    mean[i] <- alpha + beta.pop[pop[i]] + beta.elev[elev[i]]
 }

# Derived quantities
 tau <- 1 / ( sigma * sigma)
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(wing=wing, elev = as.numeric(elev), pop = as.numeric(pop), n = length(wing))

# Inits function
inits <- function(){ list(alpha = rnorm(1), sigma = rlnorm(1) )}

# Parameters to estimate
params <- c("alpha", "beta.pop", "beta.elev", "sigma")

# MCMC settings
ni <- 1200
nb <- 200
nt <- 2
nc <- 3

# Start Gibbs sampling
out <- bugs(win.data, inits, params, "2w.anova.txt", n.thin=nt, 
n.chains=nc, n.burnin=nb, n.iter=ni, debug = TRUE)

# Print estimates
print(out, dig = 3)

summary(mainfit)


### 10.5.2. Interaction-effects ANOVA using WinBUGS
# Write model
sink("2w2.anova.txt")
cat("
model {

# Priors
 for (i in 1:n.pop){
    for(j in 1:n.elev) {
       group.mean[i,j] ~ dnorm(0, 0.0001)
    }
 }
 sigma ~ dunif(0, 100)

# Likelihood
 for (i in 1:n) {
    wing[i] ~ dnorm(mean[i], tau) 
    mean[i] <- group.mean[pop[i], elev[i]]
 }

# Derived quantities
 tau <- 1 / ( sigma * sigma)
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(wing=wing, elev = as.numeric(elev), pop = as.numeric(pop), 
n = length(wing), n.elev = length(unique(elev)), n.pop = length(unique(pop)))

# Inits function
inits <- function(){list(sigma = rlnorm(1) )}

# Parameters to estimate
params <- c("group.mean", "sigma")

# MCMC settings
ni <- 1200
nb <- 200
nt <- 2
nc <- 3

# Start Gibbs sampling
out <- bugs(win.data, inits, params, "2w2.anova.txt", n.thin=nt, 
n.chains=nc, n.burnin=nb, n.iter=ni, debug = TRUE)

# Print estimates
print(out, dig = 3)

summary(intfit)


### 10.5.3. Forming predictions
or <- c(1,4,7,10,13,2,5,8,11,14,3,6,9,12,15)
plot(or, out$mean$group.mean, xlab = "Elev-by-Population", las = 1, 
ylab = "Predicted wing-length", cex = 1.5, ylim = c(20, 70))
segments(or, out$mean$group.mean, or, out$mean$group.mean + out$sd$group.mean, 
col = "black", lwd = 1)
segments(or, out$mean$group.mean, or, out$mean$group.mean - out$sd$group.mean, 
col = "black", lwd = 1)
abline(h = 40)






###############################################################
#          Chapter 11: General linear model (ANCOVA)          #
###############################################################



### 11.2. Data generation
n.groups <- 3
n.sample <- 10	
n <- n.groups * n.sample		# Total number of data points
x <- rep(1:n.groups, rep(n.sample, n.groups)) # Indicator for population
pop <- factor(x, labels = c("Pyrenees", "Massif Central", "Jura"))
length <- runif(n, 45, 70)		# Obs. body length (cm) is rarely less than 45

Xmat <- model.matrix(~ pop*length)
print(Xmat, dig = 2) 
beta.vec <- c(-250, 150, 200, 6, -3, -4)

lin.pred <- Xmat[,] %*% beta.vec	# Value of lin.predictor
eps <- rnorm(n = n, mean = 0, sd = 10)	# residuals 
mass <- lin.pred + eps			# response = lin.pred + residual
hist(mass)				# Inspect what we’ve created
matplot(cbind(length[1:10], length[11:20], length[21:30]), cbind(mass[1:10], mass[11:20], 
mass[21:30]), ylim = c(0, max(mass)), ylab = "Body mass (g)", xlab = "Body length (cm)", 
col = c("Red","Green","Blue"), pch = c("P","M","J"), las = 1, cex = 1.2, cex.lab = 1.5)



### 11.3. Analysis using R
summary(lm(mass ~ pop * length))

beta.vec
cat("And the residual SD was 10 \n")



### 11.4. Analysis using WinBUGS (and a cautionary tale 
# about the importance of covariate standardition)
# Define model
sink("lm.txt")
cat("
model {

# Priors
 for (i in 1:n.group){		
    alpha[i] ~ dnorm(0, 0.001)		# Intercepts
    beta[i] ~ dnorm(0, 0.001)		# Slopes
 }
 sigma ~ dunif(0, 100)			# Residual standard deviation
 tau <- 1 / ( sigma * sigma)

# Likelihood
 for (i in 1:n) {
    mass[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[pop[i]] + beta[pop[i]]* length[i]
 }

# Derived quantities
# Define effects relative to baseline level
 a.effe2 <- alpha[2] - alpha[1]		# Intercept Massif Central vs. Pyr.
 a.effe3 <- alpha[3] - alpha[1]		# Intercept Jura vs. Pyr.
 b.effe2 <- beta[2] - beta[1]		# Slope Massif Central vs. Pyr.
 b.effe3 <- beta[3] - beta[1]		# Slope Jura vs. Pyr.

# Custom tests
 test1 <- beta[3] - beta[2]		# Slope Jura vs. Massif Central
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(mass = as.numeric(mass), pop = as.numeric(pop), 
length = length, n.group = max(as.numeric(pop)), n = n)

# Inits function
inits <- function(){ list(alpha = rnorm(n.groups, 0, 2), 
beta = rnorm(n.groups, 1, 1), sigma = rlnorm(1))}

# Parameters to estimate
parameters <- c("alpha", "beta", "sigma", "a.effe2", "a.effe3", 
"b.effe2", "b.effe3", "test1")

# MCMC settings
ni <- 1200
nb <- 200
nt <- 2
nc <- 3

# Start Markov chains
out <- bugs(win.data, inits, parameters, "lm.txt", n.thin=nt, 
n.chains=nc, n.burnin=nb, n.iter=ni, debug = TRUE)

print(out, dig = 3)			# Bayesian analysis
beta.vec				# Truth in the data-generating process
summary(lm(mass ~ pop * length))	# The ML solution again

# Data passed to WinBUGS
win.data <- list(mass = as.numeric(mass), pop = as.numeric(pop), 
length = as.numeric(scale(length)), n.group = max(as.numeric(pop)), n = n)

# Start Markov chains
out <- bugs(win.data, inits, parameters, "lm.txt", n.thin=nt, 
n.chains=nc, n.burnin=nb, n.iter=ni, debug = FALSE)

# Inspect results
print(out, dig = 3)

print(lm(mass ~ pop * as.numeric(scale(length)))$coefficients, dig = 4)






###############################################################
#           Chapter 12: Linear mixed-effects model            #
###############################################################



### 12.2. Data generation
n.groups <- 56				# Number of populations
n.sample <- 10				# Number of vipers in each pop
n <- n.groups * n.sample 		# Total number of data points
pop <- gl(n = n.groups, k = n.sample) 	# Indicator for population

# Body length (cm)
original.length <- runif(n, 45, 70) 
mn <- mean(original.length)
sd <- sd(original.length)
cat("Mean and sd used to normalise.original length:", mn, sd, "\n\n")
length <- (original.length - mn) / sd
hist(length, col = "grey")

Xmat <- model.matrix(~pop*length-1-length)
print(Xmat[1:21,], dig = 2) 		# Print top 21 rows

intercept.mean <- 230			# mu_alpha
intercept.sd <- 20			# sigma_alpha
slope.mean <- 60			# mu_beta
slope.sd <- 30				# sigma_beta

intercept.effects<-rnorm(n = n.groups, mean = intercept.mean, sd = intercept.sd)
slope.effects <- rnorm(n = n.groups, mean = slope.mean, sd = slope.sd)
all.effects <- c(intercept.effects, slope.effects) # Put them all together

lin.pred <- Xmat[,] %*% all.effects	# Value of lin.predictor
eps <- rnorm(n = n, mean = 0, sd = 30)	# residuals 
mass <- lin.pred + eps			# response = lin.pred + residual

hist(mass, col = "grey")		# Inspect what we’ve created

library("lattice")
xyplot(mass ~ length | pop)



### 12.3. Analysis under a random-intercepts model


### 12.3.1. REML analysis using R
library('lme4')
lme.fit1 <- lmer(mass ~ length + (1 | pop), REML = TRUE)
lme.fit1


### 12.3.2. Bayesian analysis using WinBUGS
# Write model
sink("lme.model1.txt")
cat("
model {

# Priors
 for (i in 1:ngroups){		
    alpha[i] ~ dnorm(mu.int, tau.int)	# Random intercepts
 }

 mu.int ~ dnorm(0, 0.001)		# Mean hyperparameter for random intercepts
 tau.int <- 1 / (sigma.int * sigma.int)
 sigma.int ~ dunif(0, 100)		# SD hyperparameter for random intercepts

 beta ~ dnorm(0, 0.001)			# Common slope
 tau <- 1 / ( sigma * sigma)		# Residual precision
 sigma ~ dunif(0, 100)			# Residual standard deviation

# Likelihood
 for (i in 1:n) {
    mass[i] ~ dnorm(mu[i], tau)		# The random variable
    mu[i] <- alpha[pop[i]] + beta* length[i] # Expectation
 }
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(mass = as.numeric(mass), pop = as.numeric(pop), 
length = length, ngroups = max(as.numeric(pop)), n = n)

# Inits function
inits <- function(){list(alpha = rnorm(n.groups, 0, 2), beta = rnorm(1, 1, 1), 
mu.int = rnorm(1, 0, 1), sigma.int = rlnorm(1), sigma = rlnorm(1))}

# Parameters to estimate
parameters <- c("alpha", "beta", "mu.int", "sigma.int", "sigma")

# MCMC settings
ni <- 2000
nb <- 500
nt <- 2
nc <- 3

# Start Gibbs sampling
out <- bugs(win.data, inits, parameters, "lme.model1.txt", n.thin=nt, 
n.chains=nc, n.burnin=nb, n.iter=ni, debug = TRUE)

# Inspect results
print(out, dig = 3)

# Compare with input values
intercept.mean  ;  slope.mean  ;  intercept.sd  ;  slope.sd  ;  sd(eps)



### 12.4. Analysis under a random-coefficients model without correlation between intercept and slope


### 12.4.1. REML analysis using R
library('lme4')
lme.fit2 <- lmer(mass ~ length + (1 | pop) + ( 0+ length | pop))
lme.fit2


### 12.4.2. Bayesian analysis using WinBUGS
# Define model
sink("lme.model2.txt")
cat("
model {

# Priors
 for (i in 1:ngroups){		
    alpha[i] ~ dnorm(mu.int, tau.int)	# Random intercepts
    beta[i] ~ dnorm(mu.slope, tau.slope)# Random slopes
 }

 mu.int ~ dnorm(0, 0.001)		# Mean hyperparameter for random intercepts
 tau.int <- 1 / (sigma.int * sigma.int)
 sigma.int ~ dunif(0, 100)		# SD hyperparameter for random intercepts

 mu.slope ~ dnorm(0, 0.001)		# Mean hyperparameter for random slopes
 tau.slope <- 1 / (sigma.slope * sigma.slope)
 sigma.slope ~ dunif(0, 100)		# SD hyperparameter for slopes

 tau <- 1 / ( sigma * sigma)		# Residual precision
 sigma ~ dunif(0, 100)			# Residual standard deviation

# Likelihood
 for (i in 1:n) {
    mass[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[pop[i]] + beta[pop[i]]* length[i]
 }
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(mass = as.numeric(mass), pop = as.numeric(pop), 
length = length, ngroups = max(as.numeric(pop)), n = n)

# Inits function
inits <- function(){ list(alpha = rnorm(n.groups, 0, 2), beta = rnorm(n.groups, 10, 2), 
mu.int = rnorm(1, 0, 1), sigma.int = rlnorm(1), mu.slope = rnorm(1, 0, 1), 
sigma.slope = rlnorm(1), sigma = rlnorm(1))}

# Parameters to estimate
parameters <- c("alpha", "beta", "mu.int", "sigma.int", "mu.slope", "sigma.slope", "sigma")

# MCMC settings
ni <- 2000
nb <- 500
nt <- 2
nc <- 3

# Start Gibbs sampling
out <- bugs(win.data, inits, parameters, "lme.model2.txt", n.thin=nt, 
n.chains=nc, n.burnin=nb, n.iter=ni, debug = TRUE)

print(out, dig = 2)

# Compare with input values
intercept.mean  ;  slope.mean  ;  intercept.sd  ;  slope.sd  ;  sd(eps)



### 12.5. The random-coefficients model with correlation between intercept and slope


### 12.5.2. Data generation
n.groups <- 56
n.sample <- 10
n <- n.groups * n.sample 
pop <- gl(n = n.groups, k = n.sample)

original.length <- runif(n, 45, 70) 	# Body length (cm)
mn <- mean(original.length)
sd <- sd(original.length)
cat("Mean and sd used to normalise.original length:", mn, sd, "\n\n")
length <- (original.length - mn) / sd
hist(length, col = "grey")

Xmat <- model.matrix(~pop*length-1-length)
print(Xmat[1:21,], dig = 2) 		# Print top 21 rows

library(MASS)				# Load MASS
?mvrnorm				# Check syntax

intercept.mean <- 230			# Values for five hyperparameters
intercept.sd <- 20
slope.mean <- 60
slope.sd <- 30
intercept.slope.covariance <- 10

mu.vector <- c(intercept.mean, slope.mean)
var.cova.matrix <- matrix(c(intercept.sd^2,intercept.slope.covariance, 
intercept.slope.covariance, slope.sd^2),2,2)

effects <- mvrnorm(n = n.groups, mu = mu.vector, Sigma = var.cova.matrix)
effects					# Look at what we’ve created
apply(effects, 2, mean)
var(effects)

intercept.effects <- effects[,1]
slope.effects <- effects[,2]
all.effects <- c(intercept.effects, slope.effects) # Put them all together

lin.pred <- Xmat[,] %*% all.effects	# Value of lin.predictor
eps <- rnorm(n = n, mean = 0, sd = 30)	# residuals 
mass <- lin.pred + eps			# response = lin.pred + residual

hist(mass, col = "grey")		# Inspect what we’ve created

library("lattice")
xyplot(mass ~ length | pop)


### 12.5.3. REML analysis using R
library('lme4')
lme.fit3 <- lmer(mass ~ length + (length | pop))
lme.fit3


### 12.5.4. Bayesian analysis using WinBUGS
# Define model
sink("lme.model3.txt")
cat("
model {

# Priors
 for (i in 1:ngroups){
    alpha[i] <- B[i,1]
    beta[i] <- B[i,2]
    B[i,1:2] ~ dmnorm(B.hat[i,], Tau.B[,])
    B.hat[i,1] <- mu.int
    B.hat[i,2] <- mu.slope
}

 mu.int ~ dnorm(0, 0.001)		# Hyperpriors for random intercepts
 mu.slope ~ dnorm(0, 0.001)		# Hyperpriors for random slopes

 Tau.B[1:2,1:2] <- inverse(Sigma.B[,])
 Sigma.B[1,1] <- pow(sigma.int,2)
 sigma.int ~ dunif(0, 100)		# SD of intercepts
 Sigma.B[2,2] <- pow(sigma.slope,2)
 sigma.slope ~ dunif(0, 100)		# SD of slopes
 Sigma.B[1,2] <- rho*sigma.int*sigma.slope
 Sigma.B[2,1] <- Sigma.B[1,2]
 rho ~ dunif(-1,1)
 covariance <- Sigma.B[1,2]

 tau <- 1 / ( sigma * sigma)		# Residual
 sigma ~ dunif(0, 100)			# Residual standard deviation

# Likelihood
 for (i in 1:n) {
    mass[i] ~ dnorm(mu[i], tau)		# The 'residual' random variable
    mu[i] <- alpha[pop[i]] + beta[pop[i]]* length[i]  # Expectation
 }
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(mass = as.numeric(mass), pop = as.numeric(pop), 
length = length, ngroups = max(as.numeric(pop)), n = n)

# Inits function
inits <- function(){ list(mu.int = rnorm(1, 0, 1), sigma.int = rlnorm(1), 
mu.slope = rnorm(1, 0, 1), sigma.slope = rlnorm(1), rho = runif(1, -1, 1), sigma = rlnorm(1))}

# Parameters to estimate
parameters <- c("alpha", "beta", "mu.int", "sigma.int", "mu.slope", 
"sigma.slope", "rho", "covariance", "sigma")

# MCMC settings
ni <- 2000
nb <- 500
nt <- 2
nc <- 3

# Start Gibbs sampler
out <- bugs(win.data, inits, parameters, "lme.model3.txt", n.thin=nt, 
n.chains=nc, n.burnin=nb, n.iter=ni, debug = TRUE)

print(out, dig = 2)			# Bayesian analysis
lme.fit3				# Frequentist analysis






###############################################################
#          Chapter 13: GLM intro: Poisson ‘t-test’            #
###############################################################



### 13.1. Data generation
n.sites <- 10
x <- gl(n = 2, k = n.sites, labels = c("grassland", "arable"))
n <- 2*n.sites

lambda <- exp(0.69 + 0.92*(as.numeric(x)-1)) # x has levels 1 and 2, not 0 and 1

C <- rpois(n = n, lambda = lambda)	# Add Poisson noise
aggregate(C, by = list(x), FUN = mean)	# The observed means
boxplot(C ~ x, col = "grey", xlab = "Land-use", ylab = "Hare count", las = 1)



### 13.4. Analysis using R
poisson.t.test <- glm(C ~ x, family = poisson) # Fit the model
summary(poisson.t.test)			# t-Test
anova(poisson.t.test, test = "Chisq")	# Likelihood ratio test (LRT)



### 13.5. Analysis using WinBUGS
# Define model
sink("Poisson.t.test.txt")
cat("
model {

# Priors
 alpha ~ dnorm(0,0.001)
 beta ~ dnorm(0,0.001)

# Likelihood
 for (i in 1:n) {
    C[i] ~ dpois(lambda[i]) 
    log(lambda[i]) <- alpha + beta *x[i]

# Fit assessments
    Presi[i] <- (C[i] - lambda[i]) / sqrt(lambda[i]) # Pearson residuals
    C.new[i] ~ dpois(lambda[i])		# Replicate data set
    Presi.new[i] <- (C.new[i] - lambda[i]) / sqrt(lambda[i]) # Pearson resi
    D[i] <- pow(Presi[i], 2)
    D.new[i] <- pow(Presi.new[i], 2)
 }

# Add up discrepancy measures
 fit <- sum(D[])
 fit.new <- sum(D.new[])
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(C = C, x = as.numeric(x)-1, n = length(x))

# Inits function
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1))}

# Parameters to estimate
params <- c("lambda","alpha", "beta", "Presi", "fit", "fit.new")

# MCMC settings
nc <- 3
ni <- 3000
nb <- 1000
nt <- 2

# Start Gibbs sampler
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, 
model.file="Poisson.t.test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, 
n.iter=ni, debug = TRUE)


### 13.5.1. Check of MCMC convergence and model adequacy
print(out, dig = 3)

which(out$summary[,8] > 1.1)		# which value in the 8th column is > 1.1 ?

hist(out$summary[,8], col = "grey", main = "Rhat values")

plot(out$mean$Presi, las = 1)
abline(h = 0)

plot(out$sims.list$fit, out$sims.list$fit.new, main = 
"Posterior predictive check \nfor sum of squared Pearson residuals", 
xlab = "Discrepancy measure for actual data set", 
ylab = "Discrepancy measure for perfect data sets")
abline(0,1, lwd = 2, col = "black")

mean(out$sims.list$fit.new > out$sims.list$fit)


### 13.5.2. Inference under the model
print(out, dig = 3)

summary(poisson.t.test)

hist(out$sims.list$beta, col = "grey", las = 1, xlab = "Coefficient for arable", main = "")

par(mfrow = c(2,1))
hist(exp(out$sims.list$alpha), main = "Grassland study areas", 
col = "grey", xlab = "", xlim = c(0,10), breaks = 20)
hist(exp(out$sims.list$alpha + out$sims.list$beta), main = "Arable study areas", 
col = "grey", xlab = "Expected hare count", xlim = c(0,10), breaks = 20)






###############################################################
#  Chapter 14: Overdisperrsion, zero-inflation and offsets    #
###############################################################



### 14.1. Overdispersion


### 14.1.2. Data generation
n.site <- 10
x <- gl(n = 2, k = n.site, labels = c("grassland", "arable"))
eps <- rnorm(2*n.site, mean = 0, sd = 0.5)# Normal random effect
lambda.OD <- exp(0.69 +(0.92*(as.numeric(x)-1) + eps) )
lambda.Poisson <- exp(0.69 +(0.92*(as.numeric(x)-1)) ) # For comparison

C.OD <- rpois(n = 2*n.site, lambda = lambda.OD)
C.Poisson <- rpois(n = 2*n.site, lambda = lambda.Poisson)

par(mfrow = c(1,2))
boxplot(C.OD ~ x, col = "grey", xlab = "Land-use", main = "With OD", 
ylab = "Hare count", las = 1, ylim = c(0, max(C.OD)))
boxplot(C.Poisson ~ x, col = "grey", xlab = "Land-use", main = "Without OD", 
ylab = "Hare count", las = 1, ylim = c(0, max(C.OD)) )


### 14.1.3. Analysis using R
glm.fit.no.OD <- glm(C.OD ~ x, family = poisson)
glm.fit.with.OD <- glm(C.OD ~ x, family = quasipoisson)
summary(glm.fit.no.OD)
summary(glm.fit.with.OD)
anova(glm.fit.no.OD, test = "Chisq")
anova(glm.fit.with.OD, test = "F")


### 14.1.4. Analysis using WinBUGS
# Define model
sink("Poisson.OD.t.test.txt")
cat("
model {
# Priors
 alpha ~ dnorm(0,0.001)
 beta ~ dnorm(0,0.001)
 sigma ~ dunif(0, 10)	
 tau <- 1 / (sigma * sigma)

# Likelihood
 for (i in 1:n) {
    C.OD[i] ~ dpois(lambda[i]) 
    log(lambda[i]) <- alpha + beta *x[i] + eps[i]
    eps[i] ~ dnorm(0, tau)
 }
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(C.OD = C.OD, x = as.numeric(x)-1, n = length(x))

# Inits function
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1), sigma = rlnorm(1))}

# Parameters to estimate
params <- c("lambda","alpha", "beta", "sigma")

# MCMC settings
nc <- 3		# Number of chains
ni <- 3000		# Number of draws from posterior per chain
nb <- 1000		# Number of draws to discard as burn-in
nt <- 5		# Thinning rate

# Start Gibbs sampling
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, 
model.file="Poisson.OD.t.test.txt", n.thin=nt, n.chains=nc, 
n.burnin=nb, n.iter=ni, debug = TRUE)

print(out, dig = 3)



### 14.2. Zero-inflation


### 14.2.2. Data generation
psi <- 0.8
n.site <- 20
x <- gl(n = 2, k = n.site, labels = c("grassland", "arable"))

w <- rbinom(n = 2*n.site, size = 1, prob = psi)

lambda <- exp(0.69 +(0.92*(as.numeric(x)-1)) )

C <- rpois(n = 2*n.site, lambda = w *lambda)
cbind(x, w, C)


### 14.2.3. Analysis using R
library(pscl)
fm <- zeroinfl(C ~ x | 1, dist = "poisson")

summary(fm)


### 14.2.4. Analysis using WinBUGS
# Define model
sink("ZIP.txt")
cat("
model {
# Priors
 psi ~ dunif(0,1)
 alpha ~ dnorm(0,0.001)
 beta ~ dnorm(0,0.001)

# Likelihood
 for (i in 1:n) {
    w[i] ~ dbern(psi)
    C[i] ~ dpois(eff.lambda[i]) 
    eff.lambda[i] <- w[i]*lambda[i]
    log(lambda[i]) <- alpha + beta *x[i]
 }

# Derived quantity
 R.lpsi <- logit(1-psi)
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(C = C, x = as.numeric(x)-1, n = length(x))

# Inits function
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1), w = rep(1, 2*n.site))}

# Parameters to estimate
params <- c("lambda","alpha", "beta", "w", "psi", "R.lpsi")

# MCMC settings (need fairly long chains)
nc <- 3					# Number of chains
ni <- 50000				# Number of draws from posterior per chain
nb <- 10000				# Number of draws to discard as burn-in
nt <- 4					# Thinning rate

# Start WinBUGS
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, model.file="ZIP.txt", 
n.thin=nt,n.chains=nc,n.burnin=nb, n.iter=ni, debug = TRUE)

print(out, dig = 3)



### 14.3. Offsets


### 14.3.2. Data generation
n.site <- 10
A <- runif(n = 2*n.site, 2,5)		# Areas range in size from 2 to 5 km2
x <- gl(n = 2, k = n.site, labels = c("grassland", "arable"))
linear.predictor <- log(A) + 0.69 +(0.92*(as.numeric(x)-1))
lambda <- exp(linear.predictor)
C <- rpois(n = 2*n.site, lambda = lambda) # Add Poisson noise


### 14.3.3. Analysis using R
glm.fit.no.offset <- glm(C ~ x, family = poisson)
glm.fit.with.offset <- glm(C ~ x, family = poisson, offset = log(A))
summary(glm.fit.no.offset)
summary(glm.fit.with.offset)
anova(glm.fit.with.offset, test = "Chisq") # LRT


### 14.3.4. Analysis using WinBUGS
# Define model
sink("Offset.txt")
cat("
model {
# Priors
 alpha ~ dnorm(0,0.001)
 beta ~ dnorm(0,0.001)

# Likelihood
 for (i in 1:n) {
    C[i] ~ dpois(lambda[i]) 
    log(lambda[i]) <- 1 * logA[i] + alpha + beta *x[i]	# Note offset
 }
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(C = C, x = as.numeric(x)-1, logA = log(A), n = length(x))

# Inits function
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1))}

# Parameters to estimate
params <- c("lambda","alpha", "beta")

# MCMC settings
nc <- 3		# Number of chains
ni <- 1100		# Number of draws from posterior
nb <- 100		# Number of draws to discard as burn-in
nt <- 2		# Thinning rate

# Start Gibbs sampling
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, model.file="Offset.txt", 
n.thin=nt,n.chains=nc,n.burnin=nb, n.iter=ni, debug = TRUE)

print(out, dig = 3)






###############################################################
#                Chapter 15: Poisson ANCOVA                   #
###############################################################



### 15.2. Data generation
n.groups <- 3
n.sample <- 100
n <- n.groups * n.sample

x <- rep(1:n.groups, rep(n.sample, n.groups)) # Population indicator
pop <- factor(x, labels = c("Pyrenees", "Massif Central", "Jura"))

length <- runif(n, 4.5, 7.0)		# Wing length (cm)
length <- length - mean(length)		# Centre by subtracting the mean

Xmat <- model.matrix(~ pop*length)
print(Xmat, dig = 2) 

beta.vec <- c(-2, 1, 2, 5, -2, -7)

lin.pred <- Xmat[,] %*% beta.vec	# Value of lin.predictor
lambda <- exp(lin.pred)			# Poisson mean
C <- rpois(n = n, lambda = lambda)	# Add Poisson noise

# Inspect what we’ve created
par(mfrow = c(2,1))
hist(C, col = "grey", breaks = 30, xlab = "Parasite load", main = "", las = 1)
plot(length, C, pch = rep(c("P","M","J"), each=n.sample), las = 1, 
col = rep(c("Red","Green","Blue"), each=n.sample), 
ylab = "Parasite load", xlab = "Wing length", cex = 1.2)
par(mfrow = c(1,1))



### 15.3. Analysis using R
summary(glm(C ~ pop * length, family = poisson))
beta.vec



### 15.4. Analysis using WinBUGS


### 15.4.1. Fitting the model
# Define model
sink("glm.txt")
cat("
model {

# Priors
 for (i in 1:n.groups){		
    alpha[i] ~ dnorm(0, 0.01)		# Intercepts
    beta[i] ~ dnorm(0, 0.01)		# Slopes
 }
 

# Likelihood
 for (i in 1:n) {
    C[i] ~ dpois(lambda[i])		# The random variable
    lambda[i] <- exp(alpha[pop[i]] + beta[pop[i]]* length[i])
 }					# Note double-indexing: alpha[pop[i]]

# Derived quantities
# Recover effects relative to baseline level (no. 1)
 a.effe2 <- alpha[2] - alpha[1]		# Intercept Massif Central vs. Pyr.
 a.effe3 <- alpha[3] - alpha[1]		# Intercept Jura vs. Pyr.
 b.effe2 <- beta[2] - beta[1]		# Slope Massif Central vs. Pyr.
 b.effe3 <- beta[3] - beta[1]		# Slope Jura vs. Pyr.

# Custom test
 test1 <- beta[3] - beta[2]		# Slope Jura vs. Massif Central
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(C = C, pop = as.numeric(pop), n.groups = n.groups, length = length, n = n)

# Inits function
inits <- function(){list(alpha=rlnorm(n.groups,3,1), beta=rlnorm(n.groups,2,1))}

# Parameters to estimate
params <- c("alpha", "beta", "a.effe2", "a.effe3", "b.effe2", "b.effe3", "test1")

# MCMC settings
ni <- 4500
nb <- 1500
nt <- 5
nc <- 3

# Start Gibbs sampling
out <- bugs(data = win.data, inits = inits, parameters.to.save = params, model.file = "glm.txt", 
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, debug = TRUE)

print(out, dig = 3)			# Bayesian analysis
beta.vec				# Truth in data-generating process
print(glm(C ~ pop * length, family = poisson)$coef, dig = 4) # The ML solution


### 15.4.2. Forming predictions
# Create a vector with 100 wing lengths
original.wlength <- sort(runif(100, 4.5, 7.0))
wlength <- original.wlength - 5.75	# 5.75 is approximate mean 
					# (correct would be that from the original data really)

# Create matrices to contain prediction for each winglength and MCMC iteration
sel.sample <- sample(1:1800, size = 100)
mite.load.Pyr <- mite.load.MC <- mite.load.Ju <- array(dim = c(100, 100))

# Fill in these vectors: this is clumsy, but it works
for(i in 1:100) {
   for(j in 1:100) {
     mite.load.Pyr[i,j] <- exp(out$sims.list$alpha[sel.sample[j],1] + 
     	out$sims.list$beta[sel.sample[j],1] * wlength[i])
     mite.load.MC[i,j] <- exp(out$sims.list$alpha[sel.sample[j],2] + 
     	out$sims.list$beta[sel.sample[j],2] * wlength[i])
     mite.load.Ju[i,j] <- exp(out$sims.list$alpha[sel.sample[j],3] + 
     	out$sims.list$beta[sel.sample[j],3] * wlength[i])
   }
}

matplot(original.wlength, mite.load.Pyr, col = "grey", type = "l", 
las = 1, ylab = "Expected mite load", xlab = "Wing length (cm)")
for(j in 1:100){
   points(original.wlength, mite.load.MC[,j], col = "grey", type = "l")
   points(original.wlength, mite.load.Ju[,j], col = "grey", type = "l")
}
points(original.wlength, exp(out$mean$alpha[1] + out$mean$beta[1] * wlength), 
	col = "red", type = "l", lwd = 2)
points(original.wlength, exp(out$mean$alpha[2] + out$mean$beta[2] * wlength), 
	col = "green", type = "l", lwd = 2)
points(original.wlength, exp(out$mean$alpha[3] + out$mean$beta[3] * wlength), 
	col = "blue", type = "l", lwd = 2)

LCB.Pyr <- apply(mite.load.Pyr, 1, quantile, prob=0.025)
UCB.Pyr <- apply(mite.load.Pyr, 1, quantile, prob=0.975)
LCB.MC <- apply(mite.load.MC, 1, quantile, prob=0.025)
UCB.MC <- apply(mite.load.MC, 1, quantile, prob=0.975)
LCB.Ju <- apply(mite.load.Ju, 1, quantile, prob=0.025)
UCB.Ju <- apply(mite.load.Ju, 1, quantile, prob=0.975)

mean.rel <- cbind(exp(out$mean$alpha[1] + out$mean$beta[1] * wlength), 
exp(out$mean$alpha[2] + out$mean$beta[2] * wlength),
exp(out$mean$alpha[3] + out$mean$beta[3] * wlength))
covar <- cbind(original.wlength, original.wlength, original.wlength)

matplot(original.wlength, mean.rel, col = c("red", "green", "blue"), type = "l", lty = 1, lwd = 2, 
las = 1, ylab = "Expected mite load", xlab = "Wing length (cm)")
points(original.wlength, LCB.Pyr, col = "grey", type = "l", lwd = 1)
points(original.wlength, UCB.Pyr, col = "grey", type = "l", lwd = 1)
points(original.wlength, LCB.MC, col = "grey", type = "l", lwd = 1)
points(original.wlength, UCB.MC, col = "grey", type = "l", lwd = 1)
points(original.wlength, LCB.Ju, col = "grey", type = "l", lwd = 1)
points(original.wlength, UCB.Ju, col = "grey", type = "l", lwd = 1)






###############################################################
#                Chapter 16: Poisson GLMM                     #
###############################################################



### 16.2. Data generation
n.groups <- 16
n.years <- 30
n <- n.groups * n.years
pop <- gl(n = n.groups, k = n.years)

original.year <- rep(1:n.years, n.groups)
year <- (original.year-1)/29

Xmat <- model.matrix(~pop*year-1-year)
print(Xmat[1:91,], dig = 2) 		# Print top 91 rows

intercept.mean <- 3			# Choose values for the hyperparams
intercept.sd <- 1
slope.mean <- -2
slope.sd <- 0.6
intercept.effects<-rnorm(n = n.groups, mean = intercept.mean, sd = intercept.sd)
slope.effects <- rnorm(n = n.groups, mean = slope.mean, sd = slope.sd)
all.effects <- c(intercept.effects, slope.effects) # Put them all together

lin.pred <- Xmat[,] %*% all.effects	# Value of lin.predictor
C <- rpois(n = n, lambda = exp(lin.pred)) # Exponentiate and add Poisson noise
hist(C, col = "grey")			# Inspect what we’ve created

library("lattice")
xyplot(C ~ original.year | pop, ylab = "Red-backed shrike counts", xlab = "Year")



### 16.3. Analysis under a random-coefficients model


### 16.3.1. Analysis using R
library('lme4')
glmm.fit <- glmer(C ~ year + (1 | pop) + ( 0+ year | pop), family = poisson)
glmm.fit				# Inspect results


### 16.3.2. Analysis using WinBUGS
# Define model
sink("glmm.txt")
cat("
model {

# Priors
 for (i in 1:n.groups){		
    alpha[i] ~ dnorm(mu.int, tau.int)	# Intercepts
    beta[i] ~ dnorm(mu.beta, tau.beta)	# Slopes
}
 mu.int ~ dnorm(0, 0.001)		# Hyperparam for random intercepts
 tau.int <- 1 / (sigma.int * sigma.int)
 sigma.int ~ dunif(0, 10)

 mu.beta ~ dnorm(0, 0.001)		# Hyperparam for random slopes
 tau.beta <- 1 / (sigma.beta * sigma.beta)
 sigma.beta ~ dunif(0, 10)


# Poisson likelihood
 for (i in 1:n) {
    C[i] ~ dpois(lambda[i])
    lambda[i] <- exp(alpha[pop[i]] + beta[pop[i]]* year[i])
 }
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(C = C, pop = as.numeric(pop), year = year, n.groups = n.groups, n = n)

# Inits function
inits <- function(){ list(alpha = rnorm(n.groups, 0, 2), beta = 
rnorm(n.groups, 0, 2), mu.int = rnorm(1, 0, 1), sigma.int = rlnorm(1), 
mu.beta = rnorm(1, 0, 1), sigma.beta = rlnorm(1))}

# Parameters to estimate
parameters <- c("alpha", "beta", "mu.int", "sigma.int", "mu.beta", "sigma.beta")

# MCMC settings
ni <- 2000
nb <- 500
nt <- 2
nc <- 3

# Start Gibbs sampling
out <- bugs(win.data, inits, parameters, "glmm.txt", n.thin=nt, n.chains=nc, 
n.burnin=nb, n.iter=ni, debug = TRUE)

print(out, dig = 2)

# Compare with input values
intercept.mean  ;  intercept.sd  ;  slope.mean  ;  slope.sd






###############################################################
#                Chapter 17: Binomial ‘t-test’                #
###############################################################



### 17.2. Data generation
N <- 50					# Binomial total (Number of coin flips)
p.cr <- 13/50				# Success probability Cross-leaved
p.ch <- 29/50				# Success probability Chiltern gentian

C.cr <- rbinom(1, 50, prob = p.cr)   ;   C.cr # Add Binomial noise
C.ch <- rbinom(1, 50, prob = p.ch)   ;   C.ch # Add Binomial noise
C <- c(C.cr, C.ch)
species <- factor(c(0,1), labels = c("Cross-leaved gentian","Chiltern gentian"))



### 17.3. Analysis using R
summary(glm(cbind(C, N - C) ~ species, family = binomial))
predict(glm(cbind(C, N - C) ~ species, family = binomial), type = "response")



### 17.4. Analysis using WinBUGS
# Define model
sink("Binomial.t.test.txt")
cat("
model {

# Priors
 alpha ~ dnorm(0,0.01)
 beta ~ dnorm(0,0.01)

# Likelihood
 for (i in 1:n) {
    C[i] ~ dbin(p[i], N) 		# Note p before N
    logit(p[i]) <- alpha + beta *species[i]
 }
# Derived quantities
 Occ.cross <- exp(alpha) / (1 + exp(alpha))
 Occ.chiltern <- exp(alpha + beta) / (1 + exp(alpha + beta))
 Occ.Diff <- Occ.chiltern - Occ.cross	# Test quantity
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(C = C, N = 50, species = c(0,1), n = length(C))

# Inits function
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1))}

# Parameters to estimate
params <- c("alpha", "beta", "Occ.cross", "Occ.chiltern", "Occ.Diff")

# MCMC settings
nc <- 3
ni <- 1200
nb <- 200
nt <- 2

# Start Gibbs sampling
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, 
model.file="Binomial.t.test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, 
n.iter=ni, debug = TRUE)

print(out, dig = 3)

par(mfrow = c(3,1))
hist(out$sims.list$Occ.cross, col = "grey", xlim = c(0,1), main = "", 
	xlab = "Occupancy Cross-leaved", breaks = 30)
abline(v = out$mean$Occ.cross, lwd = 3, col = "red")
hist(out$sims.list$Occ.chiltern, col = "grey", xlim = c(0,1), main = "", 
	xlab = "Occupancy Chiltern", breaks = 30)
abline(v = out$mean$Occ.chiltern, lwd = 3, col = "red")
hist(out$sims.list$Occ.Diff, col = "grey", xlim = c(0,1), main = "", 
	xlab = "Difference in occupancy", breaks = 30)
abline(v = 0, lwd = 3, col = "red")






###############################################################
#               Chapter 18: Binomial ANCOVA                   #
###############################################################



### 18.2. Data generation
n.groups <- 3
n.sample <- 10
n <- n.groups * n.sample
x <- rep(1:n.groups, rep(n.sample, n.groups))
pop <- factor(x, labels = c("Jura", "Black Forest", "Alps"))

wetness.Jura <- sort(runif(n.sample, 0, 1))
wetness.BlackF <- sort(runif(n.sample, 0, 1))
wetness.Alps <- sort(runif(n.sample, 0, 1))
wetness <- c(wetness.Jura, wetness.BlackF, wetness.Alps)

N <- round(runif(n, 10, 50) )		# Get discrete Uniform values

Xmat <- model.matrix(~ pop*wetness)
print(Xmat, dig = 2) 

beta.vec <- c(-4, 1, 2, 6, 2, -5)

lin.pred <- Xmat[,] %*% beta.vec	# Value of lin.predictor
exp.p <- exp(lin.pred) / (1 + exp(lin.pred)) # Expected proportion

C <- rbinom(n = n, size = N, prob = exp.p) # Add binomial noise
hist(C)					# Inspect what we’ve created

par(mfrow = c(2,1))
matplot(cbind(wetness[1:10], wetness[11:20], wetness[21:30]), cbind(exp.p[1:10], 
exp.p[11:20], exp.p[21:30]), ylab = "Expected black", xlab = "", col = c("red","green","blue"), 
pch = c("J","B","A"), lty = "solid", type = "b", las = 1, cex = 1.2, main = "", lwd = 1)

matplot(cbind(wetness[1:10], wetness[11:20], wetness[21:30]), cbind(C[1:10]/N[1:10], 
C[11:20]/N[11:20], C[21:30]/N[21:30]), ylab = "Observed black", xlab = "Wetness index", 
col = c("red","green","blue"), pch = c("J","B","A"), las = 1, cex = 1.2, main = "")
par(mfrow = c(1,1))



### 18.3. Analysis using R
summary(glm(cbind(C, N-C) ~ pop * wetness, family = binomial))
beta.vec



### 18.4. Analysis using WinBUGS
# Define model
sink("glm.txt")
cat("
model {

# Priors
 for (i in 1:n.groups){		
    alpha[i] ~ dnorm(0, 0.01)		# Intercepts
    beta[i] ~ dnorm(0, 0.01)		# Slopes
 }
 

# Likelihood
 for (i in 1:n) {
    C[i] ~ dbin(p[i], N[i])
    logit(p[i]) <- alpha[pop[i]] + beta[pop[i]]* wetness[i] # Baseline Jura

# Fit assessments: Pearson residuals and posterior predictive check
    Presi[i] <- (C[i]-N[i]*p[i]) / sqrt(N[i]*p[i]*(1-p[i]))	# Pearson resi
    C.new[i] ~ dbin(p[i], N[i])		# Create replicate data set
    Presi.new[i] <- (C.new[i]-N[i]*p[i]) / sqrt(N[i]*p[i]*(1-p[i]))
 }

# Derived quantities
# Recover the effects relative to baseline level (no. 1)
 a.effe2 <- alpha[2] - alpha[1]		# Intercept Black Forest vs. Jura
 a.effe3 <- alpha[3] - alpha[1]		# Intercept Alps vs. Jura
 b.effe2 <- beta[2] - beta[1]		# Slope Black Forest vs. Jura
 b.effe3 <- beta[3] - beta[1]		# Slope Alps vs. Jura

# Custom tests
 test1 <- beta[3] - beta[2]		# Difference slope Alps -Black Forest

# Add up discrepancy measures
 fit <- sum(Presi[])
 fit.new <- sum(Presi.new[])
}
",fill=TRUE)
sink()


# Bundle data
win.data <- list(C = C, N = N, pop = as.numeric(pop), n.groups = n.groups, 
wetness = wetness, n = n)

# Inits function
inits <- function(){ list(alpha = rlnorm(n.groups, 3, 1), beta = rlnorm(n.groups, 2, 1))} # Note log-normal inits

# Parameters to estimate
params <- c("alpha", "beta", "a.effe2", "a.effe3", "b.effe2", "b.effe3", 
"test1", "Presi", "fit", "fit.new")

# MCMC settings
ni <- 1500
nb <- 500
nt <- 5
nc <- 3

# Start Gibbs sampler
out <- bugs(data = win.data, inits = inits, parameters.to.save = params, model.file = "glm.txt",
n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, debug = TRUE)

par(mfrow = c(1,2), cex = 1.5)
plot(out$mean$Presi, ylab = "Residual", las = 1)
abline(h = 0)
plot(wetness, out$mean$Presi, ylab = "Residual", las = 1)
abline(h = 0)

plot(out$sims.list$fit, out$sims.list$fit.new, main = "", xlab = "Discrepancy actual data", 
ylab = "Discrepancy ideal data")
abline(0,1, lwd = 2, col = "black")

mean(out$sims.list$fit.new > out$sims.list$fit)

print(out, dig = 3)			# Bayesian analysis
beta.vec				# Truth in data generation
print(glm(cbind(C, N-C) ~ pop * wetness, family = binomial)$coef, dig = 4) # The ML solution






###############################################################
#               Chapter 19: Binomial GLMM                     #
###############################################################



### 19.2. Data generation
n.groups <- 16
n.years <- 10
n <- n.groups * n.years
pop <- gl(n = n.groups, k = n.years)

prec <- runif(n, 0, 1)

N <- round(runif(n, 10, 50) )

Xmat <- model.matrix(~pop*prec-1-prec)
print(Xmat[1:91,], dig = 2) 		# Print top 91 rows

intercept.mean <- 1			# Select hyperparams
intercept.sd <- 1
slope.mean <- -2
slope.sd <- 1
intercept.effects<-rnorm(n = n.groups, mean = intercept.mean, sd = intercept.sd)
slope.effects <- rnorm(n = n.groups, mean = slope.mean, sd = slope.sd)
all.effects <- c(intercept.effects, slope.effects) # Put them all together

lin.pred <- Xmat %*% all.effects	# Value of lin.predictor
exp.p <- exp(lin.pred) / (1 + exp(lin.pred)) # Expected proportion

library("lattice")
xyplot(exp.p ~ prec | pop, ylab = "Expected woodchat shrike breeding success ",
xlab = "Spring precipitation index", main = "Expected breeding success")

C <- rbinom(n = n, size = N, prob = exp.p) # Add binomial variation
xyplot(C/N ~ prec | pop, ylab = "Realized woodchat shrike breeding success ", 
xlab = "Spring precipitation index", main = "Realized breeding success")



### 19.3. Analysis under a random-coefficients model


### 19.3.1. Analysis using R
library('lme4')
glmm.fit <- glmer(cbind(C, N-C) ~ prec + (1 | pop) + ( 0+ prec | pop), family = binomial)
glmm.fit


### 19.4.2. Analysis using WinBUGS
# Define model
sink("glmm.txt")
cat("
model {

# Priors
 for (i in 1:n.groups){		
    alpha[i] ~ dnorm(mu.int, tau.int)	# Intercepts
    beta[i] ~ dnorm(mu.beta, tau.beta)	# Slopes
}
 mu.int ~ dnorm(0, 0.001)		# Hyperparameter for random intercepts
 tau.int <- 1 / (sigma.int * sigma.int)
 sigma.int ~ dunif(0, 10)

 mu.beta ~ dnorm(0, 0.001)		# Hyperparameter for random slopes
 tau.beta <- 1 / (sigma.beta * sigma.beta)
 sigma.beta ~ dunif(0, 10)

# Binomial likelihood
 for (i in 1:n) {
    C[i] ~ dbin(p[i], N[i])
    logit(p[i]) <- alpha[pop[i]] + beta[pop[i]]* prec[i]
 }
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(C = C, N = N, pop = as.numeric(pop), prec = prec, 
n.groups = n.groups, n = n)

# Inits function
inits <- function(){ list(alpha = rnorm(n.groups, 0, 2), beta = rnorm(n.groups, 1, 1), 
mu.int = rnorm(1, 0, 1), mu.beta = rnorm(1, 0, 1))}

# Parameters to estimate
params <- c("alpha", "beta", "mu.int", "sigma.int", "mu.beta", "sigma.beta")

# MCMC settings
ni <- 2000
nb <- 500
nt <- 2
nc <- 3

# Start Gibbs sampling
out <- bugs(win.data, inits, params, "glmm.txt", n.thin=nt, n.chains=nc, 
n.burnin=nb, n.iter=ni, debug = TRUE)

print(out, dig = 2)

# Compare with input values
intercept.mean  ;  intercept.sd  ;  slope.mean  ;  slope.sd






###############################################################
#             Chapter 20: Site-occ model                      #
###############################################################



### 20.2. Data generation
n.site <- 150				# 150 sites visited

humidity <- sort(runif(n = n.site, min = -1, max =1))

alpha.occ <- 0				# Logit-linear intercept for humidity on occurrence
beta.occ <- 3				# Logit-linear slope for humidity on occurrence
occ.prob <- exp(alpha.occ + beta.occ * humidity) / (1 + exp(alpha.occ + beta.occ * humidity))

plot(humidity, occ.prob, ylim = c(0,1), xlab = "Humidity index", ylab = 
"Occurrence probability", main = "", las = 1)

true.presence <- rbinom(n = n.site, size = 1, prob = occ.prob)
true.presence				# Look at the true occupancy state of each site
sum(true.presence)			# Among the 150 visited sites

alpha.p <- 0				# Logit-linear intercept for humidity on detection
beta.p <- -5				# Logit-linear slope for humidity on detection
det.prob <- exp(alpha.p + beta.p * humidity) / (1 + exp(alpha.p + beta.p * humidity))
plot(humidity, det.prob, ylim = c(0,1), main = "", xlab = "Humidity index", 
ylab = "Detection probability", las = 1)

eff.det.prob <- true.presence * det.prob

R <- n.site
T <- 3
y <- array(dim = c(R, T))

# Simulate results of first through last surveys
for(i in 1:T){
    y[,i] <- rbinom(n = n.site, size = 1, prob = eff.det.prob)
}

y	# Look at the data
sum(apply(y, 1, sum) > 0)		# Apparent distribution among 150 visited sites

obs <- as.numeric(apply(y, 1, sum) > 0)
naive.analysis <- glm(obs ~ humidity, family = binomial)
summary(naive.analysis)
lin.pred <- naive.analysis$coefficients[1] + naive.analysis$coefficients[2] * humidity
plot(humidity, exp(lin.pred) / (1 + exp(lin.pred)), ylim = c(0,1), 
main = "", xlab = "Humidity index", ylab = "Predicted probability of occurrence", las = 1)



### 20.3. Analysis using WinBUGS
# Define model
sink("model.txt")
cat("
model {

# Priors
 alpha.occ ~ dunif(-10, 10)		# Set A of priors
 beta.occ ~ dunif(-10, 10)
 alpha.p ~ dunif(-10, 10)
 beta.p ~ dunif(-10, 10)
# alpha.occ ~ dnorm(0, 0.01) 		# Set B of priors
# beta.occ ~ dnorm(0, 0.01)
# alpha.p ~ dnorm(0, 0.01)
# beta.p ~ dnorm(0, 0.01)

# Likelihood
 for (i in 1:R) { #start initial loop over the R sites
 # True state model for the partially observed true state
    z[i] ~ dbern(psi[i])		# True occupancy z at site i
    logit(psi[i]) <- alpha.occ + beta.occ * humidity[i]

    for (j in 1:T) { # start a second loop over the T replicates
       # Observation model for the actual observations
       y[i,j] ~ dbern(eff.p[i,j])	# Detection-nondetection at i and j
       eff.p[i,j] <- z[i] * p[i,j]
       logit(p[i,j]) <- alpha.p + beta.p * humidity[i]

       # Computation of fit statistic (for Bayesian p-value)
       Presi[i,j] <- abs(y[i,j]-p[i,j])	 # Absolute residual
       y.new[i,j]~dbern(eff.p[i,j])
       Presi.new[i,j] <- abs(y.new[i,j]-p[i,j])
    }
 }

fit <- sum(Presi[,])# Discrepancy for actual data set
fit.new <- sum(Presi.new[,]) 		# Discrepancy for replicate data set

# Derived quantities
 occ.fs <- sum(z[])			# Number of occupied sites among 150
}
",fill=TRUE)
sink()

# Bundle data
win.data <- list(y = y, humidity = humidity, R = dim(y)[1], T = dim(y)[2])

# Inits function
zst <- apply(y, 1, max)			# Good starting values for latent states essential !
inits <- function(){list(z = zst, alpha.occ=runif(1, -5, 5), beta.occ = runif(1, -5, 5), 
alpha.p = runif(1, -5, 5), beta.p = runif(1, -5, 5))}

# Parameters to estimate
params <- c("alpha.occ","beta.occ", "alpha.p", "beta.p", "occ.fs", "fit", "fit.new")

# MCMC settings
nc <- 3
nb <- 2000
ni <- 12000
nt <- 5

# Start Gibbs sampler
out <- bugs(win.data, inits, params, "model.txt", n.chains=nc, 
n.iter=ni, n.burn = nb, n.thin=nt, debug = TRUE)

plot(out$sims.list$fit, out$sims.list$fit.new, main = "", xlab = 
"Discrepancy for actual data set", ylab = "Discrepancy for perfect 
data sets", las = 1)
abline(0,1, lwd = 2)

mean(out$sims.list$fit.new > out$sims.list$fit)

cat("\n *** Known truth ***\n\n")
alpha.occ  ;  beta.occ  ;  alpha.p  ;  beta.p
sum(true.presence) 			# True number of occupied sites, to be compared with occ.fs
sum(apply(y, 1, sum) > 0)		# Apparent number of occupied sites
cat("\n *** Our estimate of truth *** \n\n")
print(out, dig = 3)

plot(humidity, exp(lin.pred) / (1 + exp(lin.pred)), ylim = c(0,1), main = "", ylab = "Occurrence probability", 
xlab = "Humidity index", type = "l", lwd = 2, col = "red", las = 1)
points(humidity, occ.prob, ylim = c(0,1), type = "l", lwd = 2, col = "black")
lin.pred2 <- out$mean$alpha.occ + out$mean$beta.occ * humidity
points(humidity, exp(lin.pred2) / (1 + exp(lin.pred2)), ylim = c(0,1), type = "l", lwd = 2, col = "blue")






###############################################################
#                   Chapter 21: Nmix model                    #
###############################################################



### 21.2. Data generation
n.site <- 200
vege <- sort(runif(n = n.site, min = -1.5, max =1.5))

alpha.lam <- 2				# Intercept
beta1.lam <- 2				# Linear effect of vegetation
beta2.lam <- -2				# Quadratic effect of vegetation
lam <- exp(alpha.lam + beta1.lam * vege + beta2.lam * (vege^2))

par(mfrow = c(2,1))
plot(vege, lam, main = "", xlab = "", ylab = "Expected abundance", las = 1)

N <- rpois(n = n.site, lambda = lam)
table(N)				# Distribution of abundances across sites
sum(N > 0) / n.site			# Empirical occupancy

plot(vege, N, main = "", xlab = "Vegetation cover", ylab = "Realized abundance")
points(vege, lam, type = "l", lwd = 2)

par(mfrow = c(2,1))
alpha.p <- 1				# Intercept
beta.p <- -4				# Linear effect of vegetation
det.prob <- exp(alpha.p + beta.p * vege) / (1 + exp(alpha.p + beta.p * vege))
plot(vege, det.prob, ylim = c(0,1), main = "", xlab = "", ylab = "Detection probability")

expected.count <- N * det.prob
plot(vege, expected.count, main = "", xlab = "Vegetation cover", ylab = 
"Apparent abundance", ylim = c(0, max(N)), las = 1)
points(vege, lam, type = "l", col = "black", lwd = 2) # Truth

R <- n.site
T <- 3					# Number of replicate counts at each site
y <- array(dim = c(R, T))

for(j in 1:T){
    y[,j] <- rbinom(n = n.site, size = N, prob = det.prob)
}
y

sum(apply(y, 1, sum) > 0)		# Apparent distribution (proportion occupied sites)
sum(N > 0)				# True occupancy

C <- c(y)

site = 1:R				# ‘Short’ version of site covariate
site.p <- rep(site, T)			# ‘Long’ version of site covariate
vege.p <- rep(vege, T)			# ‘Long’ version of vegetation covariate
cbind(C, site.p, vege.p)		# Check that all went right

max.count <- apply(y, 1, max)
naive.analysis <- glm(max.count ~ vege + I(vege^2), family = poisson)
summary(naive.analysis)
lin.pred <- naive.analysis$coefficients[1] + naive.analysis$coefficients[2] * vege + 
naive.analysis$coefficients[3] * (vege*vege)

par(mfrow = c(1,1))
plot(vege, max.count, main = "", xlab = "Vegetation cover", ylab = "Abundance or count", 
ylim = c(0,max(N)), las = 1)
points(vege, lam, type = "l", col = "black", lwd = 2)
points(vege, exp(lin.pred), type = "l", col = "red", lwd = 2)



### 21.3. Analysis using WinBUGS
# Define model
sink("BinMix.txt")
cat("
model {

# Priors
 alpha.lam ~ dnorm(0, 0.1)
 beta1.lam ~ dnorm(0, 0.1)
 beta2.lam ~ dnorm(0, 0.1)
 alpha.p ~ dnorm(0, 0.1)
 beta.p ~ dnorm(0, 0.1)

# Likelihood
# Biological model for true abundance
 for (i in 1:R) {			# Loop over R sites
   N[i] ~ dpois(lambda[i])
   log(lambda[i]) <- alpha.lam + beta1.lam * vege[i] + beta2.lam * vege2[i]
 }

# Observation model for replicated counts
 for (i in 1:n) {			# Loop over all n observations
   C[i] ~ dbin(p[i], N[site.p[i]])
   logit(p[i]) <- alpha.p + beta.p * vege.p[i]
 }

# Derived quantities
 totalN <- sum(N[])			# Estimate total population size across all sites

}
",fill=TRUE)
sink()

# Bundle data
R = dim(y)[1]
n = dim(y)[1] * dim(y)[2]
vege2 = (vege * vege)
win.data <- list(R = R, vege = vege, vege2 = vege2, n = n, C = C, site.p = site.p, vege.p = vege.p)

# Inits function
Nst <- apply(y, 1, max) + 1
inits <- function(){list(N = Nst, alpha.lam=rnorm(1, 0, 1), beta1.lam=rnorm(1, 0, 1), 
beta2.lam=rnorm(1, 0, 1), alpha.p=rnorm(1, 0, 1), beta.p=rnorm(1, 0, 1))}

# Parameters to estimate
params <- c("N", "totalN", "alpha.lam", "beta1.lam", "beta2.lam", "alpha.p", "beta.p")

# MCMC settings
nc <- 3
nb <- 200
ni <- 1200
nt <- 5

# Start Gibbs sampler
out <- bugs(win.data, inits, params, "BinMix.txt", n.chains=nc, n.iter=ni, 
n.burn = nb, n.thin=nt, debug = TRUE)

cat("\n *** Our estimate of truth *** \n\n")
print(out, dig = 2)

cat("\n *** Compare with known truth ***\n\n")
alpha.lam  ;  beta1.lam  ;  beta2.lam  ;   alpha.p  ;  beta.p
sum(N) 					# True total population size across all sites
sum(apply(y, 1, max))			# Sum of site max counts


# Define model with new uniform priors
sink("BinMix.txt")
cat("
model {

# Priors (new)
 alpha.lam ~ dunif(-10, 10)
 beta1.lam ~ dunif(-10, 10)
 beta2.lam ~ dunif(-10, 10)
 alpha.p ~ dunif(-10, 10)
 beta.p ~ dunif(-10, 10)

# Likelihood
# Biological model for true abundance
 for (i in 1:R) {
   N[i] ~ dpois(lambda[i])
   log(lambda[i]) <- alpha.lam + beta1.lam * vege[i] + beta2.lam * vege2[i]
 }

# Observation model for replicated counts
 for (i in 1:n) {
   C[i] ~ dbin(p[i], N[site.p[i]])
   lp[i] <- alpha.p + beta.p * vege.p[i]
   p[i] <- exp(lp[i])/(1+exp(lp[i]))
 }

# Derived quantities
 totalN <- sum(N[])

# Assess model fit using Chi-squared discrepancy
 for (i in 1:n) {
# Compute fit statistic for observed data
     eval[i]<-p[i]*N[site.p[i]]
     E[i] <- pow((C[i] - eval[i]),2) / (eval[i] + 0.5)
# Generate replicate data and compute fit stats for them
     C.new[i] ~ dbin(p[i], N[site.p[i]])
     E.new[i] <- pow((C.new[i] - eval[i]),2) / (eval[i] + 0.5)
 }
 fit <- sum(E[])
 fit.new <- sum(E.new[])
}
",fill=TRUE)
sink()

# Parameters to estimate
params <- c("N", "totalN", "alpha.lam", "beta1.lam", "beta2.lam", 
"alpha.p", "beta.p", "fit", "fit.new")

# MCMC settings
nc <- 3
nb <- 10000
ni <- 60000
nt <- 50				# Takes >20 mins on my laptop

# Start Gibbs sampler
out <- bugs(win.data, inits, params, "BinMix.txt", n.chains=nc, n.iter=ni, 
n.burn = nb, n.thin=nt, debug = TRUE)

print(out, dig = 3)
which(out$summary[,8] > 1.1)

hist(out$sims.list$beta.p, col = "grey", main = "", xlab = "")

plot(out$sims.list$fit, out$sims.list$fit.new, main = "", xlab = 
"Discrepancy measure for actual data set", 
ylab = "Discrepancy measure for perfect data sets")
abline(0,1, lwd = 2, col = "black")

mean(out$sims.list$fit.new > out$sims.list$fit)

cat("\n *** Our estimate of truth *** \n\n")
print(out, dig = 2)

cat("\n *** Compare with known truth ***\n\n")
alpha.lam  ;  beta1.lam  ;  beta2.lam  ;   alpha.p  ;  beta.p
sum(N) 					# True total population size across all sites
sum(apply(y, 1, max))			# Sum of site max counts

par(mfrow = c(3,2))
hist(out$sims.list$alpha.lam, col = "grey", main = "alpha.lam", xlab = "")
abline(v = alpha.lam, lwd = 3, col = "black")
hist(out$sims.list$beta1.lam, col = "grey", main = "beta1.lam", xlab = "")
abline(v = beta1.lam, lwd = 3, col = "black")
hist(out$sims.list$beta2.lam, col = "grey", main = "beta2.lam", xlab = "")
abline(v = beta2.lam, lwd = 3, col = "black")
hist(out$sims.list$alpha.p, col = "grey", main = "alpha.p", xlab = "")
abline(v = alpha.p, lwd = 3, col = "black")
hist(out$sims.list$beta.p, col = "grey", main = "beta.p", xlab = "")
abline(v = beta.p, lwd = 3, col = "black")
hist(out$sims.list$totalN, col = "grey", , main = "Total N", xlab = "")
abline(v = sum(N), lwd = 3, col = "black")


sel <- sort(sample(1:200, size = 4))
sel

par(mfrow = c(2,2))
hist(out$sims.list$N[,sel[1]], col = "grey", xlim = c(Nst[sel[1]]-1, 
max(out$sims.list$N[,sel[1]])), main = "Site 48", xlab = "")
abline(v = Nst[sel[1]]-1, lwd = 3, col = "red")

hist(out$sims.list$N[,sel[2]], col = "grey", xlim = c(Nst[sel[2]]-1, 
max(out$sims.list$N[,sel[2]])), main = "Site 95", xlab = "")
abline(v = Nst[sel[2]]-1, lwd = 3, col = "red")

hist(out$sims.list$N[,sel[3]], col = "grey", xlim = c(Nst[sel[3]]-1, 
max(out$sims.list$N[,sel[3]])), main = "Site 134", xlab = "")
abline(v = Nst[sel[3]]-1, lwd = 3, col = "red")

hist(out$sims.list$N[,sel[4]], col = "grey", xlim = c(Nst[sel[4]]-1, 
max(out$sims.list$N[,sel[4]])) , main = "Site 137", xlab = "")
abline(v = Nst[sel[4]]-1, lwd = 3, col = "red")

y[sel,]

N[sel]

print(out$mean$N[sel], dig = 3)

par(mfrow = c(1,1))
plot(vege, N, main = "", xlab = "Vegetation cover", ylab = "Abundance", 
las = 1, ylim = c(0,max(N)))
points(sort(vege), lam[order(vege)], type = "l", col = "black", lwd = 2)
points(vege, exp(lin.pred), type = "l", col = "red", lwd = 2)
BinMix.pred <- exp(out$mean$alpha.lam + out$mean$beta1.lam * vege + out$mean$beta2.lam * (vege*vege))
points(vege, BinMix.pred, type = "l", col = "blue", lwd = 2)






### And now good luck with your code writing. Enjoy your newly gained modeling freedom !