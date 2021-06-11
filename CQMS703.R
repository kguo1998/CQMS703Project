library(forecast)
library(fpp2)
library(fpp)
library(GGally)
path<-file.choose()
data<-read.csv(path, header=TRUE)
cp<-ts(data)
ts.plot(cp, main="BNS Closing Prices", ylab="Prices")
len<-length(cp)

#Optimal alpha value estimation
expsmooth<-ses(cp)
alpha<-expsmooth[["model"]][["par"]][["alpha"]]
#[1] 0.9999

#Fitting to AR(2) model
fit1<-ar(cp, aic=FALSE, order.max=2)
residual1<-residuals(fit1)
plot(residual1)
coeff<-fit1[["ar"]]
coeff1<-coeff[1]
coeff2<-coeff[2]

#Forecasting values of Yt from t=3 to t=148
t<-3:148
tminus1<-t-1
tminus2<-t-2
manualpred1<-coeff1*(cp[tminus1]) + coeff2*(cp[tminus2])

#box-cox transformation
lambda<- BoxCox.lambda(cp)
#[1] -0.5015102
bc_cp<-BoxCox(cp,lambda)
plot(bc_cp)

#Fitting model to box-cox transform
fit2<-ar(bc_cp, aic=FALSE, order.max=2)
residual2<-residuals(fit2)
plot(residual2)
bc_coeff<-fit2[["ar"]]
bc_coeff1<-bc_coeff[1]
bc_coeff2<-bc_coeff[2]

#Forecasting values of transformed Yt from t=3 to t=148
t<-3:148
tminus1<-t-1
tminus2<-t-2
manualpred2<-bc_coeff1*(cp[tminus1]) + bc_coeff2*(cp[tminus2])

#Determining Accuracy for each model
accuracy_model1<-accuracy(manualpred1,cp[3:len])
accuracy_model2<-accuracy(manualpred2,cp[3:len])
accuracy_model3<-accuracy(expsmooth)
