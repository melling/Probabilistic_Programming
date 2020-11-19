library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Generate data ####
set.seed(0)
N <- 10
theta <- 0.2
y <- rbinom(N, 1, theta)

data <- list(N=N, y=y) # Named list

# Fit ####

fit <- stan('bern.stan', data=data)

print(fit)

## Plots ####

traceplot(fit)

traceplot(fit, 'theta')

plot(fit)

# theta <- extract(fit)
# theta <- extract(fit, 'theta')


theta <- extract(fit, 'theta')
theta <- unlist(theta, use.names=FALSE) 
head(theta)

# Plot theta ####

hist(theta, xlim=c(0,1), freq = FALSE)

## Add the beta(1,1) prior

curve(dbeta(x, 1, 1),
      from=0, to=1,
      add=TRUE, col='green', lwd=1.5)

## Add the Posterior

curve(dbeta(x, sum(y) + 1, N - sum(y) + 1),
      from=0, to=1,
      add=TRUE, col='blue', lwd=1.5)

# Fit2 ####

## Generate data: 2nd #####

N <- 10
theta <- 0.6
y <- rbinom(N, 1, theta)

data <- list(N=N, y=y) # Named list

## 2nd Fit #####

fit2 <- stan(fit=fit, data=data)
print(fit2)

theta2 <- extract(fit2, 'theta')
theta2 <- unlist(theta, use.names=FALSE) 
head(theta2)

hist(theta2, xlim=c(0,1), freq = FALSE)

# Optimization ####

# https://pystan.readthedocs.io/en/latest/optimizing.html

point_estimate <- optimizing(fit2@stanmodel, data=data)
