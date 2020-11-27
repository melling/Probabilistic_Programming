
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
