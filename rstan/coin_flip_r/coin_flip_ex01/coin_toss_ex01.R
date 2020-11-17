library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# Source:
# https://rstudio-pubs-static.s3.amazonaws.com/573160_29a59257088c4a4cb5f3e974695224bd.html

coin_tosses <- c(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1)
coin_toss_dat <- list(N = length(coin_tosses), 
                      y = coin_tosses) 

length(coin_tosses)

stan_model <- "
data {
    // # of obs; constrained to be greater than 0
    int<lower=0> N;  

    // define Y as an array of integers length N;
    //  each element either 0 and 1
    int<lower=0,upper=1> y[N]; 
    
}
parameters {
    real<lower=0,upper=1> theta;
}
model {
    // our prior for theta
    theta ~ beta(2,6); 

    // our likelihood for y
    y ~ bernoulli(theta);
}"

fit <- stan(model_code = stan_model, data = coin_toss_dat, iter = 1000, chains = 4)

summary(fit)
# mean(extracted_values$theta < .5)
# print(fit$theta)
theta7_2 = beta(7,2)

theta2_6 = beta(2,6)
