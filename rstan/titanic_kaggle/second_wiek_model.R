# https://wiekvoet.blogspot.com/2015/09/predicting-titanic-deaths-on-kaggle-vi.html

# preparation and data reading section
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# read and combine
train <- read.csv('input/train_titanic.csv')
train$status <- 'train'
test  <- read.csv('input/test.csv')
test$status <- 'test'
test$Survived <- NA
tt <- rbind(test,train)

# generate variables
tt$Embarked[tt$Embarked==''] <- 'S'
tt$Embarked <- factor(tt$Embarked)
tt$Pclass <- factor(tt$Pclass)
tt$Survived <- factor(tt$Survived)
tt$age <- tt$Age
tt$age[is.na(tt$age)] <- 999
tt$age <- cut(tt$age,c(0,2,5,9,12,15,21,55,65,100,1000))

tt$Title <- sapply(tt$Name,function(x) strsplit(as.character(x),'[.,]')[[1]][2])
tt$Title <- gsub(' ','',tt$Title)
tt$Title[tt$Title=='Dr' & tt$Sex=='female'] <- 'Miss'
tt$Title[tt$Title %in% c('Capt','Col','Don','Sir','Jonkheer','Major','Rev','Dr')] <- 'Mr'
tt$Title[tt$Title %in% c('Lady','Ms','theCountess','Mlle','Mme','Ms','Dona')] <- 'Miss'
tt$Title <- factor(tt$Title)
# changed cabin character
tt$cabchar <- substr(tt$Cabin,1,1)
tt$cabchar[tt$cabchar %in% c('F','G','T')] <- 'X';
tt$cabchar <- factor(tt$cabchar)
tt$ncabin <- nchar(as.character(tt$Cabin))
tt$cn <- as.numeric(gsub('[[:space:][:alpha:]]','',tt$Cabin))
tt$oe <- factor(ifelse(!is.na(tt$cn),tt$cn%%2,-1))
tt$Fare[is.na(tt$Fare)]<- median(tt$Fare,na.rm=TRUE)
tt$ticket <- sub('[[:digit:]]+$','',tt$Ticket)
tt$ticket <- toupper(gsub('(\\.)|( )|(/)','',tt$ticket))
tt$ticket[tt$ticket %in% c('A2','A4','AQ3','AQ4','AS')] <- 'An'
tt$ticket[tt$ticket %in% c('SCA3','SCA4','SCAH','SC','SCAHBASLE','SCOW')] <- 'SC'
tt$ticket[tt$ticket %in% c('CASOTON','SOTONO2','SOTONOQ')] <- 'SOTON'
tt$ticket[tt$ticket %in% c('STONO2','STONOQ')] <- 'STON'
tt$ticket[tt$ticket %in% c('C')] <- 'CA'
tt$ticket[tt$ticket %in% c('SOC','SOP','SOPP')] <- 'SOP'
tt$ticket[tt$ticket %in% c('SWPP','WC','WEP')] <- 'W'
tt$ticket[tt$ticket %in% c('FA','FC','FCC')] <- 'F'
tt$ticket[tt$ticket %in% c('PP','PPP','LINE','LP','SP')] <- 'PPPP'
tt$ticket <- factor(tt$ticket)
tt$fare <- cut(tt$Fare,breaks=c(min(tt$Fare)-1,quantile(tt$Fare,seq(.2,.8,.2)),max(tt$Fare)+1))

tt$Sex <- factor(tt$Sex)

train <- tt[tt$status=='train',]
test <- tt[tt$status=='test',]

#end of preparation and data reading

options(width=90)

data_in <- list(
  survived = c(0,1)[train$Survived],
  ntrain = nrow(train),
  ntest=nrow(test),
  sex=c(1:2)[train$Sex],
  psex=c(1:2)[test$Sex],
  pclass=c(1:3)[train$Pclass],
  ppclass=c(1:3)[test$Pclass],
  embarked=c(1:3)[train$Embarked],
  pembarked=c(1:3)[test$Embarked],
  oe=c(1:3)[train$oe],
  poe=c(1:3)[test$oe],
  cabchar=c(1:7)[train$cabchar],
  pcabchar=c(1:7)[test$cabchar],
  age=c(1:10)[train$age],
  page=c(1:10)[test$age],
  ticket=c(1:13)[train$ticket],
  pticket=c(1:13)[test$ticket],
  title=c(1:4)[train$Title],
  ptitle=c(1:4)[test$Title],
  sibsp=c(1:4,rep(4,6))[train$SibSp+1],
  psibsp=c(1:4,rep(4,6))[test$SibSp+1],
  parch=c(1:4,rep(4,6))[train$Parch+1],
  pparch=c(1:4,rep(4,6))[test$Parch+1],
  fare=c(1:5)[train$fare],
  pfare=c(1:5)[test$fare]
)

parameters=c('intercept','sd1',
             'fsex','fpclass','fembarked',
             'foe','fcabchar','fage',
             'fticket','ftitle',
             'fsibsp','fparch',
             'ffare',
             'sdsex','sdpclass','sdembarked',
             'sdoe','sdcabchar','sdage',
             'sdticket','sdtitle',
             'sdsibsp','sdparch',
             'sdfare')

#fit1 <- stan(model_code = "second_wiek_model.stan", 
my_code <- '
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
'             
fit1 <- stan(model_code = my_code, 
             data = data_in, 
             pars=parameters,
             iter = 1000, 
             chains = 4,
             open_progress=FALSE)

fit1
#plot(fit1,ask=TRUE)
#traceplot(fit1,ask=TRUE)
fit2 <- stan(model_code = my_code, 
             data = data_in, 
             fit=fit1,
             pars=c('pred'),
             iter = 2000, 
             warmup =200,
             chains = 4,
             open_progress=FALSE)

fit3 <- as.matrix(fit2)[,-419]
#plots of individual passengers
#plot(density(fit3[,1]))

#plot(density(fit3[,18]))
#plot(density(as.numeric(fit3),adjust=.3))
decide1 <- apply(fit3,2,function(x) mean(x)>.5)
decide2 <- apply(fit3,2,function(x) median(x)>.5)
#table(decide1,decide2)

out <- data.frame(
  PassengerId=test$PassengerId,
  Survived=as.numeric(decide1),
  row.names=NULL)

write.csv(x=out,
          file='stanlin.csv',
          row.names=FALSE,
          quote=FALSE)