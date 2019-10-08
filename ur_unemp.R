

library(quantmod)
library(tidyverse)
library(forecast)
library(forecastHybrid)
library(caret)

# Paralell
library(foreach)
library(doParallel)
library(parallel)

getSymbols('UNRATE', src='FRED')

# -------------------------------------------------------------------------

# ur <- tail(UNRATE, 120)
# 
# ur_train <- head(ur, -1)
# ur_train <- ts(ur_train$UNRATE, start = 2009, frequency = 12)
# 
# 
# # ARIMA
# ur_fit <- auto.arima(ur_train,
#                      stepwise = FALSE,
#                      approximation = FALSE)
# 
# ur_fc <- forecast(ur_fit, h = 1)
# ur_fit %>% forecast(h = 12) %>% autoplot(include = 50)
# 
# 
# # ETS
# ets_fit <- ets(ur_train)
# ets_fit %>% forecast(h = 12) %>% autoplot(include = 100)




# -------------------------------------------------------------------------


obs <- seq(100, 500, 50)
train_obs <- 1:max(obs)
val_obs <- (max(train_obs) + 1):(nrow(UNRATE) - 18) # Nota 18 í test set

max_obs <- max(obs)

spa <- matrix(NA, ncol = length(obs), nrow = length(val_obs))
loop_list <- list()



kjarnar <- parallel::detectCores() - 2
registerDoParallel(cores = kjarnar)

byrja <- Sys.time()

# Loop
loop_list <- foreach(i = seq_along(val_obs), .packages = c("forecast")) %dopar% {
  gogn_loop <- UNRATE$UNRATE[i:(i + max_obs)]
  
  for(j in seq_along(obs)) {
    gogn_inner_loop <- tail(gogn_loop, obs[j])
    ts_inner <- ts(gogn_inner_loop, frequency = 12)
    
    fit <- auto.arima(ts_inner,
                      stepwise = FALSE)
    
    fc <- forecast(fit, h = 1)$mean
    
    spa[j, i] <- fc
  }
  return(spa)
}

Sys.time() - byrja

doParallel::stopImplicitCluster()










# 
# for(i in seq_along(test_obs)) {
#   gogn_loop <- UNRATE$UNRATE[i:(i + max_obs)]
#   
#   for(j in seq_along(obs)) {
#     gogn_inner_loop <- tail(gogn_loop, obs[j])
#     ts_inner <- ts(gogn_inner_loop, frequency = 12)
#     
#     fit <- auto.arima(ts_inner,
#                       stepwise = FALSE)
#     
#     fc <- forecast(fit, h = 1)$mean
#     
#     spa[j, i] <- fc
#   }
# }








for(i in seq_along(obs)) {
  for(j in seq_along(train_obs)){
    
    ur_loop <- UNRATE[j:(j + max_obs), ]
    
    ur_train <- tail(ur_loop, obs[i])
    ur_train <- tail(head(ur_loop, max_obs), obs[i])
    
    # ur_test_loop <- tail(UNRATE, -(j+1))
    # ur_test <- tail(head(ur_test_loop$UNRATE, max_obs), 1)
    
    ur_train_ts <- ts(ur_train, frequency = 12)
    fit <- auto.arima(ur_train_ts,
                      stepwise = FALSE)
    
    accu[[j, i]] <- forecast(fit, h = 1)$mean
    
    
  }
  
}
