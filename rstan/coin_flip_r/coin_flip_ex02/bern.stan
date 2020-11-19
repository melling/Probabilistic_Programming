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

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0,upper=1> y[N];
}

parameters {
  real<lower=0, upper=1> theta;
}

model {
  theta ~ beta(1, 1);
  for (n in 1:N)
    y[n] ~ bernoulli(theta);
}
