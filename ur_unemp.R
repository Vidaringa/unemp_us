

library(quantmod)
library(tidyverse)
library(forecast)
library(forecastHybrid)


getSymbols('UNRATE', src='FRED')

# -------------------------------------------------------------------------

ur <- tail(UNRATE, 120)

ur_train <- head(ur, -1)
ur_train <- ts(ur_train$UNRATE, start = 2009, frequency = 12)


# ARIMA
ur_fit <- auto.arima(ur_train,
                     stepwise = FALSE,
                     approximation = FALSE)

ur_fc <- forecast(ur_fit, h = 1)
ur_fit %>% forecast(h = 12) %>% autoplot(include = 50)


# ETS
ets_fit <- ets(ur_train)
ets_fit %>% forecast(h = 12) %>% autoplot(include = 100)




# -------------------------------------------------------------------------
library(caret)

obs <- seq(100, 500, 100)
max_obs <- max(obs)

accu <- matrix(NA, ncol = length(obs), nrow = (nrow(UNRATE) - max_obs))


for(i in seq_along(obs)) {
  for(j in 1:nrow(ur_test)){
    ur_loop <- tail(UNRATE, -j)
    ur_train <- tail(head(ur_loop, max_obs), obs[i])
    
    ur_test_loop <- tail(UNRATE, -(j+1))
    ur_test <- tail(head(ur_test_loop$UNRATE, max_obs), 1)
    
    ur_train_ts <- ts(ur_train, frequency = 12)
    fit <- auto.arima(ur_train_ts,
                      stepwise = FALSE)
    
    fc <- forecast(fit, h = 1)$mean
    
    accu[[j, i]] <- RMSE(fc, ur_test)
    
    
  }
  
}