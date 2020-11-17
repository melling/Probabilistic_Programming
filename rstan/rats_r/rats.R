library("rstan") # observe startup messages

options(mc.cores = parallel::detectCores())

rstan_options(auto_write = TRUE)


y <- as.matrix(read.table('https://raw.github.com/wiki/stan-dev/rstan/rats.txt', header = TRUE))
x <- c(8, 15, 22, 29, 36)
xbar <- mean(x)
N <- nrow(y)
T <- ncol(y)
# rats_fit <- stan('https://raw.githubusercontent.com/stan-dev/example-models/master/bugs_examples/vol1/rats/rats.stan')
rats_fit <- stan('rats.stan')

print(rats_fit)


# model <- stan_demo()

