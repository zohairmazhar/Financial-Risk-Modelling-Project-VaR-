setwd('/Users/zohairmazhar/Desktop/LSE/Year 3/FM321/Project')
sample_data <- read.csv('42472_Project_Stocks.csv')


sample_data$unadjustedprice <- sample_data$PRC
sample_data$price <- sample_data$PRC/sample_data$CFACPR

library(reshape2)
sample_data = dcast(sample_data, date ~ PERMNO, value.var = 'price')
names(sample_data)[c(2,3)] <- c('MSFT_PRC', 'COST_PRC')

sample_data$MSFT_SIMPLE_RETURN <- c(NA, diff(sample_data$MSFT_PRC)/head(sample_data$MSFT_PRC,-1))
sample_data$COST_SIMPLE_RETURN <- c(NA, diff(sample_data$COST_PRC)/head(sample_data$COST_PRC,-1))

sample_data$MSFT_LOG_RETURN <- log((1+sample_data$MSFT_SIMPLE_RETURN))
sample_data$COST_LOG_RETURN <- log((1+sample_data$COST_SIMPLE_RETURN))


library(lubridate)
sample_data[,1] = ymd(sample_data$date)

sample_data = sample_data[-1,]

View(sample_data)

library(ggplot2)
dev.new()
ggplot(data = sample_data, aes(x = date, y = MSFT_PRC)) +
  labs(x = 'Year', y = 'Price', title = 'Microsoft Stock Price') +
  geom_line(col = 'blue') +
  scale_x_date(
    date_breaks = "2 years",  
    date_labels = "%Y") +
  theme_minimal()

dev.new()
ggplot(data = sample_data, aes(x = date, y = COST_PRC)) +
  labs(x = 'Year', y = 'Price', title = 'Costco Stock Price') +
  geom_line(col = 'red') +
  scale_x_date(
    date_breaks = "2 years",  
    date_labels = "%Y") +
  theme_minimal()

dev.new()
ggplot(data = sample_data, aes(x = date, y = MSFT_LOG_RETURN)) +
labs(x = 'Year', y = 'Return', title = 'Microsoft Stock Returns') +
geom_line(col = 'blue') +
  scale_x_date(
    date_breaks = "2 years",  
    date_labels = "%Y") +
theme_minimal()

dev.new()
ggplot(data = sample_data, aes(x = date, y = COST_LOG_RETURN)) +
  labs(x = 'Year', y = 'Return', title = 'Costco Stock Returns') +
  geom_line(col = 'red') +
  scale_x_date(
    date_breaks = "2 years",  
    date_labels = "%Y"        
  ) +
  theme_minimal()


##############################################################################
##############################################################################

sample_data[which.min(sample_data$MSFT_LOG_RETURN),]
sample_data[which.min(sample_data$COST_LOG_RETURN),]

library(moments)
library(tseries)
library(tidyverse)
Summary_Stats <- function(returns) {
  Avg <- mean(returns)
  StdDev <- sd(returns)
  Max <- max(returns)
  Min <- min(returns)
  Skew <- skewness(returns)
  Kurt <- kurtosis(returns)
  
  LB_test <- Box.test(returns^2, lag = 10, type = "Ljung-Box")
  LB_stat <- LB_test$statistic
  LB_pval <- LB_test$p.value
  
  
  JB_test <- jarque.bera.test(returns)
  JB_stat <- JB_test$statistic
  JB_pval <- JB_test$p.value
  
  return(c(Avg, StdDev, Max, Min, Skew, Kurt, LB_stat, LB_pval, JB_stat, JB_pval))
}

Stats_MSFT <- Summary_Stats(sample_data$MSFT_LOG_RETURN)
Stats_COST <- Summary_Stats(sample_data$COST_LOG_RETURN)


Stats_table <- data.frame(
  Statistic = c("Average Return", "Std Dev of Return", "Max Return", "Min Return", 
                "Skewness", "Kurtosis", "Ljung-Box Test Statistic", "Ljung-Box p-value", 
                "Jarque-Bera Test Statistic", "Jarque-Bera p-value"),
  MSFT = round(Stats_MSFT,2),
  COST = round(Stats_COST,2)
)

print(Stats_table)

qchisq(p = 0.95, df = 10, lower.tail=TRUE)
CV = qchisq(p = 0.95, df = 2) 
CV 

retmsft <- sample_data$MSFT_LOG_RETURN

acf(retmsft^2, col = 'purple', main = 'Autocorrelation of MSFT returns squared')


retcost <- sample_data$COST_LOG_RETURN

acf(retcost^2, col = 'purple', main = 'Autocorrelation of COSTCO returns squared')

cor(retmsft, retcost)

library(car)
qqPlot(retmsft, distribution = 'norm', col = 'purple', envelope = FALSE, ylab = 'Quantiles of Returns', 
xlab = 'Standard Normal Quantiles ', main = 'MSFT', id = list(n = 0))
qqPlot(retcost, distribution = 'norm', col = 'purple', envelope = FALSE, ylab = 'Quantiles of Returns',
xlab = 'Standard Normal Quantiles', main = 'COSTCO', id = list(n = 0))

qqPlot(retmsft, distribution = 't', df = 3, col = 'purple', envelope = FALSE, ylab = 'Quantiles of Returns', 
       xlab = 'Standard Normal Quantiles ', main = 'MSFT', id = list(n = 0))
qqPlot(retcost, distribution = 't', df = 3, col = 'purple', envelope = FALSE, ylab = 'Quantiles of Returns',
       xlab = 'Standard Normal Quantiles', main = 'COSTCO', id = list(n = 0))


##################################################################################################################
#################################################################################################################

p <- 0.01
lambda <- 0.94
value <- 1
T=2250
WE=1000
WT=2250-WE
lambda=0.94
Burn=30

retmsft <- tail(retmsft,T)
date <- tail(sample_data$date, T)

VaR_msft <- as.data.frame(matrix(NA,ncol=2,nrow=length(retmsft)))
names(VaR_msft) <- c('date','retmsft')
VaR_msft$date <- date
VaR_msft$retmsft <- retmsft


VaR_msft$HS <- NA 
for(t in (WE+1):T) {
  t1 <- t-WE
  t2 <- t-1
  window <- VaR_msft$retmsft[t1:t2]
  VaR_msft$HS[t] <- -sort(window)[WE*p]*value
}

VaR_msft$EWMA <- NA
EWMA_variance <- var(retmsft[1:WE]) 
for (t in (WE + 1):T) {
  EWMA_variance <- lambda * EWMA_variance + (1 - lambda) * (VaR_msft$retmsft[t - 1]^2)  
  VaR_msft$EWMA[t] <- -sqrt(EWMA_variance) * qnorm(p) * value
}


library(rugarch)

VaR_msft$GARCH <- NA

GARCH_1_1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))

for(t in (WE+1):T) {
  t1 <- t-WE
  t2 <- t-1
  window <- VaR_msft$retmsft[t1:t2]
  fit <- ugarchfit(spec = GARCH_1_1, data = window, solver = "hybrid")
  s2 <- coef(fit)[1] +
    coef(fit)[2] * tail(window,1)^2 +
    coef(fit)[3] *tail(fit@fit$var,1 )
  VaR_msft$GARCH[t] <- -value*qnorm(p,sd=sqrt(s2))
}


VaR_msft$tGARCH <- NA
tGARCH_1_1 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std" 
)

for (t in (WE + 1):T) {
  t1 <- t - WE
  t2 <- t - 1
  window <- VaR_msft$retmsft[t1:t2]
  fit <- ugarchfit(spec = tGARCH_1_1, data = window, solver = "hybrid")
  s2 <- coef(fit)[1] +
    coef(fit)[2] * tail(window, 1)^2 +
    coef(fit)[3] * tail(fit@fit$var, 1)
  
  nu <- coef(fit)["shape"]
  
  VaR_msft$tGARCH[t] <- -value * qt(p, df = nu) * sqrt(s2) / sqrt(nu / (nu - 2))
}

View(VaR_msft)

VaR_msft_estimates <- tail(VaR_msft, 1250)
View(VaR_msft_estimates)

HS_msft <- VaR_msft_estimates$HS

dev.new()
ggplot(data = VaR_msft_estimates, aes(x = date, y = retmsft)) +
  labs(x = 'Year', y = 'Return', title = 'Microsoft VaR - Historical Simulations') +
  geom_line(col = 'blue') +
  geom_line(aes(y = -HS_msft),type='s',col="green")+
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") + 
  theme_minimal()

EWMA_msft <- VaR_msft_estimates$EWMA

dev.new()
ggplot(data = VaR_msft_estimates, aes(x = date, y = retmsft)) +
  labs(x = 'Year', y = 'Return', title = 'Microsoft VaR - EWMA') +
  geom_line(col = 'blue') +
  geom_line(aes(y = -EWMA_msft),type='s',col="green")+
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") +
  theme_minimal()

GARCH_msft <- VaR_msft_estimates$GARCH

dev.new()
ggplot(data = VaR_msft_estimates, aes(x = date, y = retmsft)) +
  labs(x = 'Year', y = 'Return', title = 'Microsoft VaR - GARCH') +
  geom_line(col = 'blue') +
  geom_line(aes(y = -GARCH_msft),type='s',col="green")+
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") +
  theme_minimal()

tGARCH_msft <- VaR_msft_estimates$tGARCH

dev.new()
ggplot(data = VaR_msft_estimates, aes(x = date, y = retmsft)) +
  labs(x = 'Year', y = 'Return', title = 'Microsoft VaR - tGARCH') +
  geom_line(col = 'blue') +
  geom_line(aes(y = -tGARCH_msft),type='s',col="green")+
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") +
  theme_minimal()

#######################################################################################
############################################################################################

retcost <- tail(retcost,T)

VaR_cost <- as.data.frame(matrix(NA,ncol=2,nrow=length(retcost)))
names(VaR_cost) <- c('date','retcost')
VaR_cost$date <- date
VaR_cost$retcost <- retcost


VaR_cost$HS <- NA 
for(t in (WE+1):T) {
  t1 <- t-WE
  t2 <- t-1
  window <- VaR_cost$retcost[t1:t2]
  VaR_cost$HS[t] <- -sort(window)[WE*p]*value
}

VaR_cost$EWMA <- NA
EWMA_variance <- var(retcost[1:WE]) 
for (t in (WE + 1):T) {
  EWMA_variance <- lambda * EWMA_variance + (1 - lambda) * (VaR_cost$retcost[t - 1]^2)  
  VaR_cost$EWMA[t] <- -sqrt(EWMA_variance) * qnorm(p) * value
}

VaR_cost$GARCH <- NA
for(t in (WE+1):T) {
  t1 <- t-WE
  t2 <- t-1
  window <- VaR_cost$retcost[t1:t2]
  fit <- ugarchfit(spec = GARCH_1_1, data = window, solver = "hybrid")
  s2 <- coef(fit)[1] +
    coef(fit)[2] * tail(window,1)^2 +
    coef(fit)[3] *tail(fit@fit$var,1 )
  VaR_cost$GARCH[t] <- -value*qnorm(p,sd=sqrt(s2))
}

VaR_cost$tGARCH <-NA
for (t in (WE + 1):T) {
  t1 <- t - WE
  t2 <- t - 1
  window <- VaR_cost$retcost[t1:t2]
  fit <- ugarchfit(spec = tGARCH_1_1, data = window, solver = "hybrid")
  s2 <- coef(fit)[1] +
    coef(fit)[2] * tail(window, 1)^2 +
    coef(fit)[3] * tail(fit@fit$var, 1)
  
  nu <- coef(fit)["shape"]
  
  VaR_cost$tGARCH[t] <- -value * qt(p, df = nu) * sqrt(s2) / sqrt(nu / (nu - 2))
}
View(VaR_cost)

VaR_cost_estimates <- tail(VaR_cost, 1250)
View(VaR_cost_estimates)

HS_cost <- VaR_cost_estimates$HS

dev.new()
ggplot(data = VaR_cost_estimates, aes(x = date, y = retcost)) +
  labs(x = 'Year', y = 'Return', title = 'Costco VaR - Historical Simulations') +
  geom_line(col = 'red') +
  geom_line(aes(y = -HS_cost),type='s',col="purple")+
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") + 
  theme_minimal()

EWMA_cost <- VaR_cost_estimates$EWMA

dev.new()
ggplot(data = VaR_cost_estimates, aes(x = date, y = retcost)) +
  labs(x = 'Year', y = 'Return', title = 'Costco VaR - EWMA') +
  geom_line(col = 'red') +
  geom_line(aes(y = -EWMA_cost),type='s',col="purple")+
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") + 
  theme_minimal()

GARCH_cost <- VaR_cost_estimates$GARCH

dev.new()
ggplot(data = VaR_cost_estimates, aes(x = date, y = retcost)) +
  labs(x = 'Year', y = 'Return', title = 'Costco VaR - GARCH') +
  geom_line(col = 'red') +
  geom_line(aes(y = -GARCH_cost),type='s',col="purple")+
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") + 
  theme_minimal()

tGARCH_cost <- VaR_cost_estimates$tGARCH

dev.new()
ggplot(data = VaR_cost_estimates, aes(x = date, y = retcost)) +
  labs(x = 'Year', y = 'Return', title = 'Costco VaR - tGARCH') +
  geom_line(col = 'red') +
  geom_line(aes(y = -tGARCH_cost),type='s',col="purple")+
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") + 
  theme_minimal()
##########################################################################################################

sample_data$Portfolio_Return <- 0.7 * sample_data$MSFT_LOG_RETURN + 0.3 * sample_data$COST_LOG_RETURN

dev.new()
ggplot(data = sample_data, aes(x = date, y = Portfolio_Return)) +
  labs(x = 'Year', y = 'Return', title = 'Portfolio Returns') +
  geom_line(col = 'purple') +
  scale_x_date(
    date_breaks = "2 years",  
    date_labels = "%Y"        
  ) +
  theme_minimal()


sample_data[which.min(sample_data$Portfolio_Return),]
sample_data[which.max(sample_data$Portfolio_Return),]

Summary_Pft_Stats <- function(returns) {
  Avg <- mean(returns)
  StdDev <- sd(returns)
  Max <- max(returns)
  Min <- min(returns)
  Skew <- skewness(returns)
  Kurt <- kurtosis(returns)
  
  LB_test <- Box.test(returns^2, lag = 10, type = "Ljung-Box")
  LB_stat <- LB_test$statistic
  LB_pval <- LB_test$p.value
  
  
  JB_test <- jarque.bera.test(returns)
  JB_stat <- JB_test$statistic
  JB_pval <- JB_test$p.value
  
  return(c(Avg, StdDev, Max, Min, Skew, Kurt, LB_stat, LB_pval, JB_stat, JB_pval))
}

Stats_Portfolio <- Summary_Pft_Stats(sample_data$Portfolio_Return)


Stats_table <- data.frame(
  Statistic = c("Average Return", "Std Dev of Return", "Max Return", "Min Return", 
                "Skewness", "Kurtosis", "Ljung-Box Test Statistic", "Ljung-Box p-value", 
                "Jarque-Bera Test Statistic", "Jarque-Bera p-value"),
  Portfolio = round(Stats_Portfolio,3)
)

print(Stats_table)

 
###########################################################################################################

library(lubridate)
library(rugarch)
library(rmgarch)
library(ggplot2)

portfolio_returns <- as.matrix(tail(cbind(sample_data$MSFT_LOG_RETURN, sample_data$COST_LOG_RETURN), T))
dates <- tail(sample_data$date, T)

VaR_portfolio <- data.frame(
  date = dates,
  ret_portfolio = 0.7 * portfolio_returns[, 1] + 0.3 * portfolio_returns[, 2],
  HS = NA,
  DCC = NA,
  EWMA_DCC = NA
)

for(t in (WE+1):T) {
  t1 <- t-WE
  t2 <- t-1
  window <- VaR_portfolio$ret_portfolio[t1:t2]
  VaR_portfolio$HS[t] <- -sort(window)[WE*p]*value
}

uspec <- multispec(list(
  ugarchspec(mean.model = list(armaOrder = c(0, 0)), variance.model = list(model = "sGARCH")),
  ugarchspec(mean.model = list(armaOrder = c(0, 0)), variance.model = list(model = "sGARCH"))
))
dccspec <- dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = "mvnorm")
dccfit <- dccfit(dccspec, data = portfolio_returns)
dcc_var <- rcov(dccfit)

weights <- matrix(c(0.7, 0.3), ncol = 1)
for (t in (WE + 1):T) {
  cov_matrix <- matrix(c(
    dcc_var[1, 1, t], dcc_var[1, 2, t],
    dcc_var[2, 1, t], dcc_var[2, 2, t]
  ), nrow = 2, ncol = 2)
  portfolio_var <- as.numeric(t(weights) %*% cov_matrix %*% weights)
  VaR_portfolio$DCC[t] <- -sqrt(portfolio_var) * qnorm(0.01)
}


ewma_cov_matrix <- cov(portfolio_returns[1:WE, , drop = FALSE])
for (t in (WE + 1):T) {
  dcc_cov_matrix <- matrix(c(
    dcc_var[1, 1, t], dcc_var[1, 2, t],
    dcc_var[2, 1, t], dcc_var[2, 2, t]
  ), nrow = 2, ncol = 2)
  ewma_cov_matrix <- lambda * ewma_cov_matrix + (1 - lambda) * dcc_cov_matrix
  portfolio_var <- as.numeric(t(weights) %*% ewma_cov_matrix %*% weights)
  VaR_portfolio$EWMA_DCC[t] <- -sqrt(portfolio_var) * qnorm(0.01)
}

VaR_portfolio_estimates <- tail(VaR_portfolio, 1250)
View(VaR_portfolio_estimates)

dev.new()
ggplot(data = VaR_portfolio_estimates, aes(x = date)) +
  geom_line(aes(y = ret_portfolio), col = 'purple', size = 1) +
  geom_line(aes(y = -HS), col = 'green', size = 1) +
  labs(x = 'Date', y = 'Return', title = 'Portfolio VaR - HS') +
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") + 
  theme_minimal()

dev.new()
ggplot(data = VaR_portfolio_estimates, aes(x = date)) +
  geom_line(aes(y = ret_portfolio), col = 'purple', size = 1) +
  geom_line(aes(y = -DCC), col = 'green', size = 1) +
  labs(x = 'Date', y = 'Return', title = 'Portfolio VaR - DCC-GARCH') +
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") + 
  theme_minimal()

dev.new()
ggplot(data = VaR_portfolio_estimates, aes(x = date)) +
  geom_line(aes(y = ret_portfolio), col = 'purple', size = 1) +
  geom_line(aes(y = -EWMA_DCC), col = 'green', size = 1, type = 's') +
  labs(x = 'Date', y = 'Return', title = 'Portfolio VaR - DCC-EWMA') +
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") + 
  theme_minimal()

######################################################################################################
portfolio_returns <- as.matrix(tail(cbind(sample_data$MSFT_LOG_RETURN, sample_data$COST_LOG_RETURN), T))
dates <- tail(sample_data$date, T)

VaR_portfolio <- data.frame(
  date <- dates,
  ret_portfolio = 0.7 * portfolio_returns[, 1] + 0.3 * portfolio_returns[, 2],
  HS <- NA,
  DCC <- NA,
  EWMA_DCC = NA
)

for(t in (WE+1):T) {
  t1 <- t-WE
  t2 <- t-1
  window <- VaR_portfolio$ret_portfolio[t1:t2]
  VaR_portfolio$HS[t] <- -sort(window)[WE*p]*value
}

uspec <- multispec(list(
  ugarchspec(mean.model = list(armaOrder = c(0, 0)), variance.model = list(model = "sGARCH")),
  ugarchspec(mean.model = list(armaOrder = c(0, 0)), variance.model = list(model = "sGARCH"))
))
dccspec <- dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = "mvnorm")
dccfit <- dccfit(dccspec, data = portfolio_returns)
dcc_var <- rcov(dccfit)

weights <- matrix(c(0.7, 0.3), ncol = 1)
for (t in (WE + 1):T) {
  cov_matrix <- matrix(c(
    dcc_var[1, 1, t], dcc_var[1, 2, t],
    dcc_var[2, 1, t], dcc_var[2, 2, t]
  ), nrow = 2, ncol = 2)
  portfolio_var <- as.numeric(t(weights) %*% cov_matrix %*% weights)
  VaR_portfolio$DCC[t] <- -sqrt(portfolio_var) * qnorm(0.01)
}


ewma_cov_matrix <- cov(portfolio_returns[1:WE, , drop = FALSE])
for (t in (WE + 1):T) {
  dcc_cov_matrix <- matrix(c(
    dcc_var[1, 1, t], dcc_var[1, 2, t],
    dcc_var[2, 1, t], dcc_var[2, 2, t]
  ), nrow = 2, ncol = 2)
  ewma_cov_matrix <- lambda * ewma_cov_matrix + (1 - lambda) * dcc_cov_matrix
  portfolio_var <- as.numeric(t(weights) %*% ewma_cov_matrix %*% weights)
  VaR_portfolio$EWMA_DCC[t] <- -sqrt(portfolio_var) * qnorm(0.01)
}

VaR_portfolio_estimates <- tail(VaR_portfolio, 1250)
View(VaR_portfolio_estimates)

dev.new()
ggplot(data = VaR_portfolio_estimates, aes(x = date)) +
  geom_line(aes(y = ret_portfolio), col = 'purple', size = 1) +
  geom_line(aes(y = -HS), col = 'green', size = 1) +
  labs(x = 'Date', y = 'Return', title = 'Portfolio VaR - HS') +
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") + 
  theme_minimal()

dev.new()
ggplot(data = VaR_portfolio_estimates, aes(x = date)) +
  geom_line(aes(y = ret_portfolio), col = 'purple', size = 1) +
  geom_line(aes(y = -DCC), col = 'green', size = 1) +
  labs(x = 'Date', y = 'Return', title = 'Portfolio VaR - DCC-GARCH Method') +
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") + 
  theme_minimal()

dev.new()
ggplot(data = VaR_portfolio_estimates, aes(x = date)) +
  geom_line(aes(y = ret_portfolio), col = 'purple', size = 1) +
  geom_line(aes(y = -EWMA_DCC), col = 'green', size = 1) +
  labs(x = 'Date', y = 'Return', title = 'Portfolio VaR - DCC-EWMA Method') +
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y") + 
  theme_minimal()


dcc_corr <- rcor(dccfit)


time_varying_corr <- data.frame(
  date = dates[(WE + 1):T],
  correlation = dcc_corr[1, 2, (WE + 1):T]
)



dev.new()
ggplot(data = time_varying_corr, aes(x = date, y = correlation)) +
  geom_line(color = 'orange', size = 1) +
  labs(
    x = 'Date',
    y = 'Correlation',
    title = 'Time-Varying Correlation: DCC-GARCH'
  ) +
  scale_x_date(
    date_breaks = '1 years',  
    date_labels = "%Y"
  ) +
  theme_minimal()

#################################################################################
p <- 0.01

bern_test <- function(p, VaR_estimates, actual_returns) {
  violations <- ifelse(actual_returns < -VaR_estimates, 1, 0)
  WT <- length(violations)
  nu1 <- sum(violations)      
  nu0 <- WT - nu1             
  
  phat <- nu1 / WT
  
  log_LR <- log(p) * nu1 + log(1 - p) * nu0
  log_LU <- log(phat) * nu1 + log(1 - phat) * nu0
  
  LR <- 2 * (log_LU - log_LR)
  
  return(list(
    "Violations" = nu1,
    "Non-Violations" = nu0,
    "Observed p-hat" = phat,
    "Likelihood Ratio (LR)" = LR
  ))
}

bern_test(p, VaR_msft_estimates$HS, VaR_msft_estimates$retmsft)
bern_test(p, VaR_msft_estimates$EWMA, VaR_msft_estimates$retmsft)
bern_test(p, VaR_msft_estimates$GARCH, VaR_msft_estimates$retmsft)
bern_test(p, VaR_msft_estimates$tGARCH, VaR_msft_estimates$retmsft)

qchisq(0.95,df=1, lower.tail = TRUE)

bern_test(p, VaR_cost_estimates$HS, VaR_cost_estimates$retcost)
bern_test(p, VaR_cost_estimates$EWMA, VaR_cost_estimates$retcost)
bern_test(p, VaR_cost_estimates$GARCH, VaR_cost_estimates$retcost)
bern_test(p, VaR_cost_estimates$tGARCH, VaR_cost_estimates$retcost)

bern_test(p, VaR_portfolio_estimates$HS, VaR_portfolio_estimates$ret_portfolio)
bern_test(p, VaR_portfolio_estimates$DCC, VaR_portfolio_estimates$ret_portfolio)
bern_test(p, VaR_portfolio_estimates$EWMA_DCC, VaR_portfolio_estimates$ret_portfolio)


