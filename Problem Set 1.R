# Lab 1: Panel data analysis
library(plm)
library(sandwich)
library(stargazer)
library(car)
library(readxl)
library(lmtest)

mydata <- id

str(id)
stargazer(id, type = "text")
dd <- mydata[with(mydata, country=="United States" & year==2000), 3]
ddd <- mydata[with(mydata, country=="United States" & year==year), 3]
mean(ddd)

#comment out multiple lines with control + shift + c
# insert R chuck , google it
#r3$residuls
#if we want to know what is in r3, we use names(r3)


r1=lm(dem_ind~log_gdppc ,data=mydata) 
r1_vcov=vcovHC(r1, type = "HC1") 
r1_se=sqrt(diag(r1_vcov))
stargazer(r1, se=list(r1_se), type="text", title="Estimaiton Results")

r2=plm(dem_ind~log_gdppc ,data=mydata ,index=c("country", "year"), model="within")
r2_vcov=vcovHC(r2, type = "HC1") 
r2_se=sqrt(diag(r2_vcov))
stargazer(r2, se=list(r2_se), type="text", title="Estimaiton Results")

r3=plm(dem_ind~log_gdppc ,data=mydata ,index=c("country", "year"),
       model="within",effect = "twoways") 
r3_vcov=vcovHC(r3, type = "HC1") 
r3_se=sqrt(diag(r3_vcov))
stargazer(r3, se=list(r3_se), type="text", title="Estimaiton Results")

stargazer(r1,r2,r3,se=list(r1_se,r2_se,r3_se), 
          title="Estimation Results", type = "text", 
          keep.stat = c("n","rsq","adj.rsq"), 
          dep.var.labels.include =F,
          model.names = F)


#to generate dummified model, we simple use "+ state (entity) after regressors"
r10=plm(dem_ind~log_gdppc + country + year,data=mydata ,index=c("country", "year"), model="within")
