library(dynlm)
library(forecast)
library(stargazer)
library(scales)
library(urca)
library(sandwich)

#PCEP denote Personal Consumption Expenditures
# mydata = read.table("PCECTPI.csv", header = T, sep = ",", skip = 0)
# tsPCEP = ts(mydata, start = c(1962,3), frequency = 4)
# tsPCEP = window(tsPCEP, start=c(1962,3), end=c(2019,4))
PCECTPI = read.csv("PCECTPI.csv") 
head(PCECTPI)
PCECTPI = ts(PCECTPI$PCECTPI, start = c(1947, 1), frequency = 4)

#Compute the inflation rate
# tsInfl = 400*diff(log(tsPCEP[,"PCECTPI"]))
Infl = 400*diff(log(PCECTPI))
Infl = window(Infl, start = c(1962, 4), end = c(2019, 4))
Infl
plot(Infl,
     col = "steelblue",
     lwd = 2,
     ylab = "Percentage Points",
     xlab = "Date",
     main = "Change in Inflation",
     cex.main = 1
)

#Use the augmented Dickey-Fuller (ADF) test for a stochastic trend in DeltaInf
m1 = dynlm(d(Infl) ~ L(Infl) + d(L(Infl)) +
                  d(L(Infl, 2))
)
#Use the augmented Dickey-Fuller (ADF) test for a stochastic trend in Infl with 5 lags
m3 = dynlm(d(Infl) ~ L(Infl) + d(L(Infl)) +
             d(L(Infl, 2)) + d(L(Infl, 3)) + d(L(Infl, 4)) + d(L(Infl, 5))
)

#Use the augmented Dickey-Fuller (ADF) test for a deterministic trend in Infl.
m2 = dynlm(d(Infl) ~ trend(Infl,scale=F) +
             L(Infl) + d(L(Infl)) +
             d(L(Infl, 2))
)
stargazer(m1, m2, m3,
          type = "text",
          title = "ADF Regression Result",
          model.names = F,
          model.numbers = F,
          column.labels = c("ADF Model", "ADF Model"),
          header = F,
          font.size = "small",
          dep.var.labels.include = F,
          label = "t2"
)

ADFtest = ur.df(Infl, type = "trend",
                lags = 2,
                selectlags = "Fixed")
summary(ADFtest)

#Split Delta Infl ts data into traning sample set and validation sample set 
DeltaInf = diff(Infl)
DeltaInf1 = window(DeltaInf, start = c(1963, 1), end = c(2002, 4))
DeltaInf2 = window(DeltaInf, start = c(2003, 1), end = c(2019, 4))
DeltaInf1
DeltaInf2

abline(h = 0, col = "black")
#Using the AR(2) model for ∆Inf to compute ̂ ∆Inf l2003Q1|2002Q4,
#∆Inf l2003Q2|2003Q1, ...,̂ ∆Inf l2019Q4|2019Q3
# Forecast estimates
m4 = arima(DeltaInf1, order = c(2,0,0))
fc = forecast(m4, h = 68, level = seq(5, 99, 10)) 
fcm <- fc$mean
fcm
DeltaInf2

#calculate the mean of the squared errors 
#and take the square root of the mean squared errors to obtain the RMSFE.
errors <- (DeltaInf2 - fcm)
squared_errors <- errors^2
mse <- mean(squared_errors)
rmsfe <- sqrt(mse)
rmsfe

ar2 = dynlm(DeltaInf1 ~ L(DeltaInf1, 1:2)) 
summary(ar2)

#import old prices data sheet downloaded from FRED economic data
POILBREUSDQ = read.csv("POILBREUSDQ.csv") 
tsOP = ts(POILBREUSDQ, start = c(1990,1), frequency = 4)
plot(tsOP,
     col = "steelblue",
     lwd = 2,
     ylab = "U.S. dollar per Fluid barrel",
     xlab = "Years",
     main = "Oil Prices",
     cex.main = 1
)

# Optional question 4 
# Set the seed for reproducibility (optional)
set.seed(123)

# Generate T = 100 i.i.d. standard normal random variables
upsilon <- rnorm(100)

# Initialize Y1
Y <- numeric(100)
Y[1] <- 0.55 + upsilon[1]

# Generate Yt based on the recursive relationship
for (t in 2:100) {
  Y[t] <- 0.55 + Y[t-1] + upsilon[t]
}

# Generate T = 100 i.i.d. standard normal random variables
epsilon <- rnorm(100)

# Initialize X1
X <- numeric(100)
X[1] <- 0.85 + epsilon[1]

# Generate Xt based on the recursive relationship
for (t in 2:100) {
  X[t] <- 0.85 + X[t-1] + epsilon[t]
}

#Regress Y on a constant and X. Compute the OLS estimator, the regression R2, and the 
#(homoskedasticity-only) t-statistic testing the null hypothesis that β1 (the coefficient on X) is 0.
r1 = lm(Y~X) 
r1_vcov=vcovHC(r1, type = "HC1") 
r1_se=sqrt(diag(r1_vcov))
stargazer(r1, se=list(r1_se), type="text", title="Estimaiton Results")
t_statistic <- summary(r1)$coef[2, "t value"]
cat("Homoskedasticity-Only t-Statistic for Beta1:", t_statistic, "\n")

# Number of simulations
num_simulations <- 1000

# Initialize vectors to store results
r_squared_values <- numeric(num_simulations)
t_statistic_values <- numeric(num_simulations)

# Simulations loop
for (i in 1:num_simulations) {
  # Step 1: Generate ν and Y
  upsilon <- rnorm(100)
  Y <- numeric(100)
  Y[1] <- 0.55 + upsilon[1]
  for (t in 2:100) {
    Y[t] <- 0.55 + Y[t-1] + upsilon[t]
  }
  
  # Step 2: Generate ε and X
  epsilon <- rnorm(100)
  X <- numeric(100)
  X[1] <- 0.85 + epsilon[1]
  for (t in 2:100) {
    X[t] <- 0.85 + X[t-1] + epsilon[t]
  }
  
  # Step 3: Regress Y on a constant and X
  lm_result <- lm(Y ~ X)
  
  # Extract and store results
  r_squared_values[i] <- summary(lm_result)$r.squared
  t_statistic_values[i] <- summary(lm_result)$coef[2, "t value"]
}

# Construct histograms
par(mfrow = c(1, 2))  # Set up a 1x2 grid for plots
hist(r_squared_values, main = "Histogram of R^2", xlab = "R^2", col = "lightblue")
hist(t_statistic_values, main = "Histogram of t-Statistic", xlab = "t-Statistic", col = "lightgreen")

# Calculate percentiles
percentiles_r_squared <- quantile(r_squared_values, c(0.05, 0.5, 0.95))
percentiles_t_statistic <- quantile(t_statistic_values, c(0.05, 0.5, 0.95))

# Print percentiles
cat("5th percentile of R^2:", percentiles_r_squared[1], "\n")
cat("50th percentile of R^2 (Median):", percentiles_r_squared[2], "\n")
cat("95th percentile of R^2:", percentiles_r_squared[3], "\n")

cat("\n5th percentile of t-Statistic:", percentiles_t_statistic[1], "\n")
cat("50th percentile of t-Statistic (Median):", percentiles_t_statistic[2], "\n")
cat("95th percentile of t-Statistic:", percentiles_t_statistic[3], "\n")

# Calculate fraction exceeding 1.96 in absolute value
fraction_exceeding <- mean(abs(t_statistic_values) > 1.96)
cat("\nFraction exceeding 1.96 in absolute value:", fraction_exceeding, "\n")


#for part c) T = 250
# Number of simulations
num_simulations <- 1000

# Initialize vectors to store results
r_squared_values <- numeric(num_simulations)
t_statistic_values <- numeric(num_simulations)

# Simulations loop
for (i in 1:num_simulations) {
  # Step 1: Generate ν and Y
  upsilon <- rnorm(250)
  Y <- numeric(250)
  Y[1] <- 0.55 + upsilon[1]
  for (t in 2:250) {
    Y[t] <- 0.55 + Y[t-1] + upsilon[t]
  }
  
  # Step 2: Generate ε and X
  epsilon <- rnorm(250)
  X <- numeric(250)
  X[1] <- 0.85 + epsilon[1]
  for (t in 2:250) {
    X[t] <- 0.85 + X[t-1] + epsilon[t]
  }
  
  # Step 3: Regress Y on a constant and X
  lm_result <- lm(Y ~ X)
  
  # Extract and store results
  r_squared_values[i] <- summary(lm_result)$r.squared
  t_statistic_values[i] <- summary(lm_result)$coef[2, "t value"]
}

# Calculate percentiles
percentiles_t_statistic <- quantile(t_statistic_values, c(0.05, 0.5, 0.95))
cat("\n5th percentile of t-Statistic:", percentiles_t_statistic[1], "\n")
cat("50th percentile of t-Statistic (Median):", percentiles_t_statistic[2], "\n")
cat("95th percentile of t-Statistic:", percentiles_t_statistic[3], "\n")
# Calculate fraction exceeding 1.96 in absolute value
fraction_exceeding <- mean(abs(t_statistic_values) > 1.96)
cat("\nFraction exceeding 1.96 in absolute value:", fraction_exceeding, "\n")

#for part c) T = 500
# Number of simulations
num_simulations <- 1000

# Initialize vectors to store results
r_squared_values <- numeric(num_simulations)
t_statistic_values <- numeric(num_simulations)

# Simulations loop
for (i in 1:num_simulations) {
  # Step 1: Generate ν and Y
  upsilon <- rnorm(500)
  Y <- numeric(500)
  Y[1] <- 0.55 + upsilon[1]
  for (t in 2:500) {
    Y[t] <- 0.55 + Y[t-1] + upsilon[t]
  }
  
  # Step 2: Generate ε and X
  epsilon <- rnorm(500)
  X <- numeric(500)
  X[1] <- 0.85 + epsilon[1]
  for (t in 2:500) {
    X[t] <- 0.85 + X[t-1] + epsilon[t]
  }
  
  # Step 3: Regress Y on a constant and X
  lm_result <- lm(Y ~ X)
  
  # Extract and store results
  r_squared_values[i] <- summary(lm_result)$r.squared
  t_statistic_values[i] <- summary(lm_result)$coef[2, "t value"]
}
# Calculate percentiles
percentiles_t_statistic <- quantile(t_statistic_values, c(0.05, 0.5, 0.95))
cat("\n5th percentile of t-Statistic:", percentiles_t_statistic[1], "\n")
cat("50th percentile of t-Statistic (Median):", percentiles_t_statistic[2], "\n")
cat("95th percentile of t-Statistic:", percentiles_t_statistic[3], "\n")
# Calculate fraction exceeding 1.96 in absolute value
fraction_exceeding <- mean(abs(t_statistic_values) > 1.96)
cat("\nFraction exceeding 1.96 in absolute value:", fraction_exceeding, "\n")

