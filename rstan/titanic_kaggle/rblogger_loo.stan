data {
  int ntrain;
  int survived[ntrain];
  int np;
  int nterm;
  int terms[np];
  matrix [ntrain,np]  tx;
}
parameters {
  vector[np] f;
  real std[nterm];
  real stdhyp;
}
model {        
  stdhyp ~ normal(0,2);
  std ~ normal(0,stdhyp);
  for (i in 1:np) {
    f[i] ~ normal(0,std[terms[i]]);
  }
  survived ~ bernoulli_logit(tx*f);
}
generated quantities {
  vector [ntrain] log_lik;
  for (i in 1:ntrain) {
    // log_lik[i] = bernoulli_logit_log(survived[i], tx[i]*f);
    log_lik[i] = bernoulli_logit_lpmf(survived[i] | tx[i]*f);

  }
}
