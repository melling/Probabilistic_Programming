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

# tt$Sex <- as.factor(tt$Sex)
tt$Sex <- factor(tt$Sex)

# tt$Sex[tt$Sex == 'male'] <- 1
# tt$Sex[tt$Sex == 'female'] <- 2
# tt$Sex <- as.numeric(tt$Sex)

train <- tt[tt$status=='train',]
test <- tt[tt$status=='test',]
#summary(train)
#train$Sex
#end of preparation and data reading
# c(1:2)[c("male", "female")]
# c(1:2)[c(3, 4)]
# train$Sex[train$Sex == 'male'] <- 1
# train$Sex[train$Sex == 'female'] <- 2
# train$Sex <- as.numeric(train$Sex)
# train$Sex
# sex <- factor(c("male", "female", "female", "male"))
# as.numeric("male")
# c(1:2)[train$Sex]
# typeof(train$Sex)
# unique(train$Sex)
# train$Name
# c(0:2)[train$Pclass]
# c(1:7)[train$cabchar]
# train$Pclass

options(width=90)

data_in <- list(
  survived = c(0,1)[train$Survived],
  ntrain = nrow(train),
  ntest=nrow(test),
  # sex = train$Sex, # c(1:2)[train$Sex],
  # psex = test$Sex, # c(1:2)[test$Sex],
  
  sex = c(1:2)[train$Sex],
  psex = c(1:2)[test$Sex],
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

parameters <- c("intercept", "sd1",
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


model = stan_model("second_wiek_model.stan")
# 
# fit_model = sampling(model, data_in, warmup=500, iter=2000, chains=4,
#                par=parameters,
# #               control = list(adapt_delta=0.99),
# #               sample_file="sampleFile.txt",  diagnostic_file="diag.txt",
#                show_messages=TRUE, verbose=TRUE)

# fit_model

fit_pred = sampling(model, data_in, warmup=200, iter=2000, chains=4,
               par=c("pred"),
              
               #               control = list(adapt_delta=0.99),
#               sample_file="sampleFile.txt",  diagnostic_file="diag.txt",
               show_messages=TRUE, verbose=TRUE)

fit_pred

# Original Model ####

# fit1 <- stan(model_name = "second_wiek_model.stan",
#              data = data_in,
#              pars = parameters,
#              iter = 1000,
#              #include = TRUE,
#              chains = 1,
# #             thin = 1,
# #              algorithm="NUTS",
#              open_progress=FALSE)

# fit1
# y = alpha + beta_1 * x1 + beta_2 * x2 + beta3*x3 + ...

#plot(fit1,ask=TRUE)
#traceplot(fit1,ask=TRUE)
# fit2 <- stan(model_name = "second_wiek_model.stan", 
#              data = data_in, 
#              fit=fit1,
#              pars=c('pred'),
#              iter = 2000, 
#              warmup = 200,
#              chains = 4,
#              open_progress=FALSE)

fit2 = fit_pred

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
