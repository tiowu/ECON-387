library(readr)
library(AER)
library(stargazer)
library(sandwich)
library(lmtest)
library(car)
urban_df = read_csv("urban.csv")
str(urban_df)
summary(urban_df)
stargazer(urban_df, type = "text")

# Model 1 OLS 
m1=lm(VEHICLES~logDENSITY + logHHINCOME + COLLEGE + WORKERS, data=urban_df)
m1_vcov=vcovHC(m1, type = "HC1")
m1_se=sqrt(diag(m1_vcov))

# Model 2 OLS
# important for working through a room-sharing argument by showing SAMEGENDER correlates with BEDROOMS
m2=lm(BEDROOMS~SAMEGENDER + logHHINCOME + COLLEGE + WORKERS, data=urban_df)
m2_vcov=vcovHC(m2, type = "HC1")
m2_se=sqrt(diag(m2_vcov))

# Model 3 OLS
# Presenting the first stage results of TSLS
m3=lm(logDENSITY~SAMEGENDER + logHHINCOME + COLLEGE + WORKERS, data=urban_df)
m3_vcov=vcovHC(m3, type = "HC1")
m3_se=sqrt(diag(m3_vcov))

# Model 4 TSLS
# Presenting the second stage results of TSLS using SAMEGENDER as the IV
m4=ivreg(VEHICLES~logDENSITY + logHHINCOME + COLLEGE + WORKERS | logHHINCOME + COLLEGE + WORKERS + SAMEGENDER, data=urban_df)
m4_vcov=vcovHC(m4, type = "HC1")
m4_se=sqrt(diag(m4_vcov))

stargazer(m1,m2,m3,m4, se=list(m1_se,m2_se,m3_se, m4_se), 
          title="Estimation Results", type = "text", 
          keep.stat = c("n","rsq","adj.rsq", "ser", "f"), 
          dep.var.labels.include = TRUE,
          model.names = TRUE)

summary(m4, vcov = sandwich, diagnostics = TRUE)








