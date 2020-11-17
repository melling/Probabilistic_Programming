//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//
// Source:

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;

  int<lower=0> n[N]; // number of tosses
  int<lower=0> y[N]; // number of heads
}  
transformed data {}
parameters {
    real<lower=0, upper=1> p;
}
transformed parameters {}
model {
    p ~ beta(2, 2);
//    y ~ binomial(n, p);
    for (i in 1:N) {
      y[i] ~ binomial(n[i], p);
    }
 
}
generated quantities {}
