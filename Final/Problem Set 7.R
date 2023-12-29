library(readr)
library(stargazer)
library(sandwich)
library(lmtest)
library(car)
library(haven)
library(readxl)
library(AER)
library(forecast)
library(scales)
library(urca)
library(dynlm)
library(stats)
mydata = read.table("PCECTPI.csv", header = T,
                    sep = ",", skip = 0)
tsPCEP = ts(mydata, start = c(1962,3), frequency = 4)
tsPCEP = window(tsPCEP, start=c(1962,3), end=c(2019,4))

#Compute the inflation rate
tsInfl = 400*diff(log(tsPCEP[,"PCECTPI"]))

#Plot the value of Infl from 1963Q1 through 2019Q4
plot(400*diff(log(tsPCEP[,"PCECTPI"])),
     col = "steelblue",
     lwd = 2,
     ylab = "Percentage Rate Per Year",
     xlab = "Date",
     main = "U.S. Inflation Rate",
     cex.main=1
)
abline(h=0)

#Compute the first four autocorrelations of ∆Infl.
tsChangeInfl = diff(tsInfl)
acfPlot = acf(tsChangeInfl, main = "",
              lag.max = 4,
              plot = T)
acfPlot$acf

#Plot the value of ∆Infl from 1963Q1 through 2019Q4
plot(tsChangeInfl,
     col = "steelblue",
     lwd = 2,
     ylab = "Change in Annual Percentage",
     xlab = "Date",
     main = "Change in U.S. Inflation Rate",
     cex.main=1
)
abline(h=0)

# AR(1) Run an OLS regression of ∆Infl on an intercept and its first lag
tsChangeInfl
r1<- dynlm(tsChangeInfl~L(tsChangeInfl ,1),data = tsChangeInfl,
           start = c(1962,4),end = c(2019,4)) 
r1Var=vcovHC(r1,type = "HC1")
r1Se=sqrt(diag(r1Var))

# AR(2) Run an OLS regression of ∆Infl on an intercept and its first 2 lags
r2<- dynlm(tsChangeInfl~L(tsChangeInfl ,1) + L(tsChangeInfl ,2),
           data = tsChangeInfl,start = c(1962,4),
           end = c(2019,4)) 
r2Var=vcovHC(r2,type = "HC1")
r2Se=sqrt(diag(r2Var))

#estimation results from AR(1) & AR(2)
stargazer(r1, r2, se=list(r1Se,r2Se),
          type ="text",
          title = "AR(1) and AR(2) Estimation Results",
          model.names = F,
          model.numbers = F,
          column.labels = c("AR(1)","AR(2)"),
          header = F,
          font.size = "small",
          dep.var.labels.include = F,
          label = "t1")

#Estimate an AR(p) model for p = 1, 2, . . . , 8, for AIC and BIC values
r1<- dynlm(tsChangeInfl~L(tsChangeInfl ,1),data = tsChangeInfl,
           start = c(1962,4),end = c(2019,4)) 
r2<- dynlm(tsChangeInfl~L(tsChangeInfl ,1) + L(tsChangeInfl ,2),
           data = tsChangeInfl,start = c(1962,4),
           end = c(2019,4)) 
r3<- dynlm(tsChangeInfl~L(tsChangeInfl ,1) + L(tsChangeInfl ,2) + L(tsChangeInfl ,3),
           data = tsChangeInfl,start = c(1962,4),
           end = c(2019,4)) 
r4<- dynlm(tsChangeInfl~L(tsChangeInfl ,1) + L(tsChangeInfl ,2) + L(tsChangeInfl ,3) 
           + L(tsChangeInfl ,4),
           data = tsChangeInfl,start = c(1962,4),
           end = c(2019,4)) 
r5<- dynlm(tsChangeInfl~L(tsChangeInfl ,1) + L(tsChangeInfl ,2) + 
             L(tsChangeInfl ,3) + L(tsChangeInfl ,4) + L(tsChangeInfl ,5),
           data = tsChangeInfl,start = c(1962,4),
           end = c(2019,4)) 
r6<- dynlm(tsChangeInfl~L(tsChangeInfl ,1) + L(tsChangeInfl ,2) + 
             L(tsChangeInfl ,3) + L(tsChangeInfl ,4) + L(tsChangeInfl ,5)
           + L(tsChangeInfl ,6),
           data = tsChangeInfl,start = c(1962,4),
           end = c(2019,4)) 
r7<- dynlm(tsChangeInfl~L(tsChangeInfl ,1) + L(tsChangeInfl ,2) + 
             L(tsChangeInfl ,3) + L(tsChangeInfl ,4) + L(tsChangeInfl ,5)
           + L(tsChangeInfl ,6) + L(tsChangeInfl ,7),
           data = tsChangeInfl,start = c(1962,4),
           end = c(2019,4)) 
r8<- dynlm(tsChangeInfl~L(tsChangeInfl ,1) + L(tsChangeInfl ,2) + 
             L(tsChangeInfl ,3) + L(tsChangeInfl ,4) + L(tsChangeInfl ,5)
           + L(tsChangeInfl ,6) + L(tsChangeInfl ,7) + L(tsChangeInfl ,8),
           data = tsChangeInfl,start = c(1962,4),
           end = c(2019,4)) 
tmp = data.frame("m1"=c(AIC(r1),BIC(r1)),
                 "m2"=c(AIC(r2),BIC(r2)),
                 "m3"=c(AIC(r3),BIC(r3)),
                 "m4"=c(AIC(r4),BIC(r4)),
                 "m5"=c(AIC(r5),BIC(r5)),
                 "m6"=c(AIC(r6),BIC(r6)),
                 "m7"=c(AIC(r7),BIC(r7)),
                 "m8"=c(AIC(r8),BIC(r8)))
rownames(tmp) = c("AIC","BIC") 
tmp


#Use the AR(2) model to predict the change in inflation from 2019Q4 to 2020Q1
#that is, to predict the value of Infl2020Q1
m1 = arima(tsChangeInfl, order = c(2,0,0))
stargazer(m1,
          type ="latex",
          title = "AR(2) Estimation Results",
          model.names = F,
          model.numbers = F,
          column.labels = "AR(2)",
          header = F,
          font.size = "small",
          dep.var.labels.include = F,
          label = "t1"
)
fc = forecast(m1, h = 10, level = seq(5, 99, 10))
fc$mean
tsInfl
plot(fc,
     main = "Forecasted Change in Inflation Rate",
     showgap = F,
     fcol = "red",
     flty = 2,
     col = "steelblue",
     lwd = 2)
abline(h=0)
