library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

set.seed(0)
N <- 10
theta <- 0.2
y <- rbinom(N, 1, theta)

data <- list(N=N, y=y) # Named list

fit <- stan('bern.stan', data=data)

print(fit)

traceplot(fit)

traceplot(fit, 'theta')

plot(fit)

# theta <- extract(fit)
# theta <- extract(fit, 'theta')


theta <- extract(fit, 'theta')
theta <- unlist(theta, use.names=FALSE) 
head(theta)

