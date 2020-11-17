from pystan import stan
from pystan import StanModel

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import warnings
import pystan

import matplotlib.pyplot as plt
import seaborn as sns

coin_code = """
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
"""

coin_dat = {
            'N': 6,
             'n': [100,100, 100, 100, 100, 100],
             'y': [46, 62, 61, 69,56, 65],
            }

fit = pystan.stan(model_code=coin_code, data=coin_dat, iter=1000, chains=5,verbose=True)

print(fit)