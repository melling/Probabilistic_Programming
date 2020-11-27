data {
  int<lower=0> ntrain;
  int<lower=0> ntest;
  int survived[ntrain];
  int <lower=1,upper=2> sex[ntrain];
  int <lower=1,upper=2> psex[ntest];
  int <lower=1,upper=3> pclass[ntrain];
  int <lower=1,upper=3> ppclass[ntest];
  int <lower=1,upper=3> embarked[ntrain];
  int <lower=1,upper=3> pembarked[ntest];
  int <lower=1,upper=3> oe[ntrain];
  int <lower=1,upper=3> poe[ntest];
  int <lower=1,upper=7> cabchar[ntrain];
  int <lower=1,upper=7> pcabchar[ntest];
  int <lower=1,upper=10> age[ntrain];
  int <lower=1,upper=10> page[ntest];
  int <lower=1,upper=13> ticket[ntrain];
  int <lower=1,upper=13> pticket[ntest];
  int <lower=1,upper=4> title[ntrain];
  int <lower=1,upper=4> ptitle[ntest];
  int <lower=1,upper=4> sibsp[ntrain];
  int <lower=1,upper=4> psibsp[ntest];
  int <lower=1,upper=4> parch[ntrain];
  int <lower=1,upper=4> pparch[ntest];
  int <lower=1,upper=5> fare[ntrain];
  int <lower=1,upper=5> pfare[ntest];
  
}
parameters {
  real fsex[2];
  real intercept;
  real fpclass[3];
  real fembarked[3];
  real foe[3];
  real fcabchar[7];
  real fage[10];
  real fticket[13];
  real ftitle[4];
  real fparch[4];
  real fsibsp[4];
  real ffare[5];
  real <lower=0> sdsex;
  real <lower=0> sdpclass;
  real <lower=0> sdembarked;
  real <lower=0> sdoe;
  real <lower=0> sdcabchar;
  real <lower=0> sdage;
  real <lower=0> sdticket;
  real <lower=0> sdtitle;
  real <lower=0> sdparch;
  real <lower=0> sdsibsp;
  real <lower=0> sdfare;
  
  real <lower=0> sd1;
}
transformed parameters {
  real expect[ntrain];
  for (i in 1:ntrain) {
    expect[i] = inv_logit(
      intercept+
      fsex[sex[i]]+
      fpclass[pclass[i]]+
      fembarked[embarked[i]]+
      foe[oe[i]]+
      fcabchar[cabchar[i]]+
      fage[age[i]]+
      fticket[ticket[i]]+
      ftitle[title[i]]+
      fsibsp[sibsp[i]]+
      fparch[parch[i]]+
      ffare[fare[i]]
      );
  }
  
}
model {        
  fsex ~ normal(0,sdsex);
  fpclass ~ normal(0,sdpclass);
  fembarked ~ normal(0,sdembarked);
  foe ~ normal(0,sdoe);
  fcabchar ~ normal(0,sdcabchar);
  fage ~ normal(0,sdage);
  fticket ~ normal(0,sdticket);
  ftitle ~ normal(0,sdtitle);
  fsibsp ~ normal(0,sdsibsp);
  fparch ~ normal(0,sdparch);
  ffare ~ normal(0,sdfare);
  
  sdsex ~  normal(0,sd1);
  sdpclass ~ normal(0,sd1);
  sdembarked ~ normal(0,sd1);
  sdoe ~ normal(0,sd1);
  sdcabchar ~ normal(0,sd1);
  sdage ~ normal(0,sd1);
  sdticket ~ normal(0,sd1);
  sdtitle ~ normal(0,sd1);
  sdsibsp ~ normal(0,sd1);
  sdparch ~ normal(0,sd1);
  sdfare ~ normal(0,sd1);
  sd1 ~ normal(0,1);
  intercept ~ normal(0,1);
  
  survived ~ bernoulli(expect);
}
generated quantities {
  real pred[ntest];
  for (i in 1:ntest) {
    pred[i] = inv_logit(
      intercept+
      fsex[psex[i]]+
      fpclass[ppclass[i]]+
      fembarked[pembarked[i]]+
      foe[poe[i]]+
      fcabchar[pcabchar[i]]+
      fage[page[i]]+
      fticket[pticket[i]]+
      ftitle[ptitle[i]]+
      fsibsp[psibsp[i]]+
      fparch[pparch[i]]+
      ffare[pfare[i]]
      
      );
  }
}
