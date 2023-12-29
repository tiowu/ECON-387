library(readr)
library(AER)
library(stargazer)
library(sandwich)
library(lmtest)
library(car)
library(haven)
df1 = read_dta("maketable1.dta")
df1 = df1[df1$baseco == 1, ]

# generate a scatter plot of the relationship between 
# average expropriation risk and log of settler mortality
plot(avexpr ~ logem4, data = df1, col = "blue", pch = 20, 
     xlab = "Log of Settler Mortality", 
     ylab = "Average Expropriation Risk 1985-95",
     main = "First-stage Relationship Between Settler Mortality and Expropriation Risks")
text(df1$logem4, df1$avexpr, label = df1$shortnam, cex = 0.6, pos = 4)
abline(lm(avexpr~logem4, data=df1), col = "red", lwd = 2)
grid()

# Run an instrumental variable estimation of log GDP per capita on average expropriation risk 
# using log of settler mortality as the instrument
df2 = read_dta("maketable4.dta")
df2.01 = df2[df2$baseco == 1, ]

# Model 01 OLS
# Presenting the first stage results of TSLS
m01=lm(avexpr ~ logem4 + africa + lat_abst + rich4 + asia + loghjypl, data=df2.01)
m01_vcov=vcovHC(m01, type = "HC1")
m01_se=sqrt(diag(m01_vcov))

# Model 02 TSLS
# Presenting the second stage results of TSLS using logem4 as the IV
m02=ivreg(logpgp95 ~ avexpr + africa + lat_abst + rich4 + asia + loghjypl | logem4 + africa + lat_abst + rich4 + asia + loghjypl, data=df2.01)
m02_vcov=vcovHC(m02, type = "HC1")
m02_se=sqrt(diag(m02_vcov))

stargazer(m01, m02, se=list(m01_se,m02_se), 
          column.labels = c("simple lm", "1st Stage", "2nd Stage"),
          title= "Estimation Results", type = "text", 
          keep.stat = c("n","rsq","adj.rsq", "ser", "f"), 
          dep.var.labels.include = TRUE,
          model.names = TRUE)

