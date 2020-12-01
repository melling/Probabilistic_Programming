
# Titanic Kaggle solution using RStan

Found these on the Internet.  Links provided to the original sources

## Example 1

http://wiekvoet.blogspot.com/2015/09/predicting-titanic-deaths-on-kaggle-vi.html

### Files

- second_wiek_model.R
- second_wiek_model.stan


### Issues

```
no parameter intercept, sd1, fsex, fpclass, fembarked, foe, fcabchar, fage, fticket, ftitle, fsibsp, fparch, ffare, sdsex, sdpclass, sdembarked, sdoe, sdcabchar, sdage, sdticket, sdtitle, sdsibsp, sdparch, sdfare; sampling not done
no parameter intercept, sd1, fsex, fpclass, fembarked, foe, fcabchar, fage, fticket, ftitle, fsibsp, fparch, ffare, sdsex, sdpclass, sdembarked, sdoe, sdcabchar, sdage, sdticket, sdtitle, sdsibsp, sdparch, sdfare; sampling not done
no parameter intercept, sd1, fsex, fpclass, fembarked, foe, fcabchar, fage, fticket, ftitle, fsibsp, fparch, ffare, sdsex, sdpclass, sdembarked, sdoe, sdcabchar, sdage, sdticket, sdtitle, sdsibsp, sdparch, sdfare; sampling not done
no parameter intercept, sd1, fsex, fpclass, fembarked, foe, fcabchar, fage, fticket, ftitle, fsibsp, fparch, ffare, sdsex, sdpclass, sdembarked, sdoe, sdcabchar, sdage, sdticket, sdtitle, sdsibsp, sdparch, sdfare; sampling not done
here are whatever error messages were returned
```

## Example 2

https://wiekvoet.blogspot.com/2015/10/predicting-titanic-deaths-on-kaggle-vii.html

This example uses Leave One Out.

### Files

- rblogger01.R
- rblogger_loo.stan

### Issues

```
WARNING: empty program

no parameter pred; sampling not done

Stan model 'second_wiek_model.stan' does not contain samples.
```

#plot(fit1,ask=TRUE)
#traceplot(fit1,ask=TRUE)
fit2 <- stan(model_name = "second_wiek_model.stan", 
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