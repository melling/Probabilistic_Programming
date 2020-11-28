// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}

// Here we are implicitly using uniform(-infinity, +infinity) priors for our parameters. These are also known as “flat” priors. Weakly informative priors (e.g. normal(0, 10) are more restricted than flat priors. 
// You can find more information about prior specification here:
// https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 y ~ normal(alpha + x * beta , sigma);
}

generated quantities {
} // The posterior predictive distribution",

