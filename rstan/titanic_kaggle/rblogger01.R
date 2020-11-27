#
# https://www.r-bloggers.com/2015/10/predicting-titanic-deaths-on-kaggle-vii-more-stan/

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(loo)

# read and combine ####
train <- read.csv('input/train_titanic.csv')
train$status <- 'train'
test  <- read.csv('input/test.csv')
test$status <- 'test'
test$Survived <- NA
tt <- rbind(test,train)

# generate variables ####

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
# changed cabin character ####
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
tt$sibsp=factor(c(1:4,rep(4,6))[tt$SibSp+1])
tt$parch=factor(c(1:4,rep(4,6))[tt$Parch+1])


train <- tt[tt$status=='train',]
test <- tt[tt$status=='test',]

# end of preparation and data reading

options(width=90)
des.matrix <- function(formula,data) {
  form2 <- strsplit(as.character(formula),'~',fixed=TRUE)
  resp <- form2[[length(form2)]]
  form3 <- strsplit(resp,'+',fixed=TRUE)[[1]]
  la <- lapply(form3,function(x) 
    model.matrix(as.formula(paste('~' , x, '-1' )),data) )
  nterm <- c(1,sapply(la,ncol))
  terms <- rep(1:length(nterm),nterm)
  ntrain <- nrow(data)
  mat <- do.call(cbind,la)
  mat <- cbind(rep(1,ntrain),mat)
  np <- ncol(mat)
  list( 
    survived = c(0,1)[data$Survived],
    np=np,
    ntrain=nrow(data),
    terms=terms,
    nterm=max(terms),
    tx=mat)
}

data_in <- des.matrix(~ Sex+Pclass,data=train)

parameters=c('std','f','log_lik')

fit1 <- stan(model_name = "rblogger_loo", 
    data = data_in, 
    pars=parameters,
    iter = 1000, 
    chains = 1,
    init = "random",
    control=list(stepsize=0.001, adapt_delta=0.9999, max_treedepth = 14),
    open_progress=FALSE)
#fit1

#log_lik1 <- extract_log_lik(fit1)
#loo1 <- loo(log_lik1)
#print(loo1, digits = 3)

print.mySmodel <- function(x) {
  print(x$loo1)
  cat('n')
  invisible(x)
}
