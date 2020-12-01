data {
  int<lower=0> ntrain;
  int<lower=0> ntest;
  int survived[ntrain];
  int <lower=1,upper=2> sex[ntrain];
  int <lower=1,upper=2> psex[ntest];
  int <lower=1,upper=3> pclass[ntrain];
  int <lower=1,upper=3> ppclass[ntest];
  
}
parameters {
  real fsex[2];
  real intercept;
  real fpclass[3];
  real <lower=0> sdsex;
  real <lower=0> sdpclass;
  real <lower=0> sd1;
}
transformed parameters {
  real expect[ntrain];
  for (i in 1:ntrain) {
    expect[i] = inv_logit(
      intercept+
      fsex[sex[i]]+
      fpclass[pclass[i]]
      );
  }
  
}
model {        
  fsex ~ normal(0,sdsex);
  fpclass ~ normal(0,sdpclass);
  
  sdsex ~  normal(0,sd1);
  sdpclass ~ normal(0,sd1);
  sd1 ~ normal(0,3);
  intercept ~ normal(0,1);
  survived ~ bernoulli(expect);
}
generated quantities {
  real pred[ntest];
  for (i in 1:ntest) {
    pred[i] = inv_logit(
      intercept+
      fsex[psex[i]]+
      fpclass[ppclass[i]]
      
      );
  }
}