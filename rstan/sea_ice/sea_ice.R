# Example from:
#   https://ourcodingclub.github.io/tutorials/stan-intro/
#   https://github.com/ourcodingclub/CC-Stan-intro

library(pacman)

# Load Data ####
# Adding stringsAsFactors = F means that numeric variables won't be
# read in as factors/categorical variables
seaice <- read.csv("seaice.csv", stringsAsFactors = F)

head(seaice)

# If for some reason the column names are not read in properly, you can change column names using:
colnames(seaice) <- c("year", "extent_north", "extent_south")

plot(extent_north ~ year, pch = 20, data = seaice)
# plot(extent_north ~ year, pch = ".", data = seaice)

# Linear Regression ####

lm1 <- lm(extent_north ~ year, data = seaice)
summary(lm1)
# plot(lm1)

abline(lm1, col = 2, lty = 2, lw = 3)

# Stan Example ####

## Prepare Data ####

x <- I(seaice$year - 1978)
y <- seaice$extent_north
N <- length(seaice$year)

# re-run that linear model with our new data

lm1 <- lm(y ~ x)
summary(lm1)

# extract some of the key summary statistics from our simple model

lm_alpha <- summary(lm1)$coeff[1]  # the intercept
lm_beta <- summary(lm1)$coeff[2]  # the slope
lm_sigma <- sigma(lm1)  # the residual error


# Format data for Stan

stan_data <- list(N = N, x = x, y = y)

library(rstan)
# library(gdata) # Old library
library(bayesplot)

# Compile model to verify it
# stanc("stan_model1.stan")
stan_model1 <- "stan_model1.stan"

fit <- stan(file = stan_model1, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)

fit

posterior <- extract(fit)
str(posterior)

# compare to our previous estimate with “lm”:
plot(y ~ x, pch = 20)

abline(lm1, col = 2, lty = 2, lw = 3)
abline( mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2)

# plot multiple estimates from the posterior ####
# 
for (i in 1:500) {
  abline(posterior$alpha[i], posterior$beta[i], col = "gray", lty = 1)
}

# Plot ...

plot(y ~ x, pch = 20)

for (i in 1:500) {
  abline(posterior$alpha[i], posterior$beta[i], col = "gray", lty = 1)
}

abline(mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2)

# #### ####
# Stan Model 2 ####
stan_model2 <- "stan_model2.stan"

fit2 <- stan(stan_model2, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)

posterior2 <- extract(fit2)

plot(y ~ x, pch = 20)

abline(alpha, beta, col = 4, lty = 2, lw = 2)
abline(mean(posterior2$alpha), mean(posterior2$beta), col = 3, lw = 2)
abline(mean(posterior$alpha), mean(posterior$beta), col = 36, lw = 3)

# Convergence Diagnostics ####

# 'Anything over an `n_eff` of 100 is usually "fine"' - Bob Carpenter

plot(posterior$alpha, type = "l")
plot(posterior$beta, type = "l")
plot(posterior$sigma, type = "l")

# Poor Convergence ####

fit_bad <- stan(stan_model1, data = stan_data, warmup = 25, iter = 50, chains = 4, cores = 2, thin = 1)

posterior_bad <- extract(fit_bad)

plot(posterior_bad$alpha, type = "l")
plot(posterior_bad$beta, type = "l")
plot(posterior_bad$sigma, type = "l")

# Parameter summaries ####

par(mfrow = c(1,3))

plot(density(posterior$alpha), main = "Alpha")
abline(v = lm_alpha, col = 4, lty = 2)

plot(density(posterior$beta), main = "Beta")
abline(v = lm_beta, col = 4, lty = 2)

plot(density(posterior$sigma), main = "Sigma")
abline(v = lm_sigma, col = 4, lty = 2)

# Probablility that beta is >0:
sum(posterior$beta>0)/length(posterior$beta) # 0

# Probablility that beta is >0.2:
sum(posterior$beta>0.2)/length(posterior$beta) # 0

# Diagnostic plots ####
traceplot(fit)

stan_dens(fit)
stan_hist(fit)

plot(fit, show_density = FALSE, ci_level = 0.5, outer_level = 0.95, fill_color = "salmon")

# stan_model2_GQ ####

stan_model2_GQ <- "stan_model2_GQ.stan"

fit3 <- stan(stan_model2_GQ, data = stan_data, iter = 1000, chains = 4, cores = 2, thin = 1)
y_rep <- as.matrix(fit3, pars = "y_rep")
dim(y_rep)

ppc_dens_overlay(y, y_rep[1:200, ])

ppc_stat(y = y, yrep = y_rep, stat = "mean")

ppc_scatter_avg(y = y, yrep = y_rep)

# Bayes Plot ####

available_ppc() # Bayes Plot options

color_scheme_view(c("blue", "gray", "green", "pink", "purple",
                    "red","teal","yellow"))

color_scheme_view("mix-blue-red")

color_scheme_set("blue")
