# Tune-a fjölda athugana sem ég nota síðan í ARIMA líkan



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
    
    spa[i, j] <- fc
  }
  return(spa)
}

Sys.time() - byrja

doParallel::stopImplicitCluster()



spa_for <- matrix(NA, ncol = length(obs), nrow = length(val_obs))

byrja <- Sys.time()

for(i in seq_along(val_obs)) {
  gogn_loop <- UNRATE$UNRATE[i:(i + max_obs)]
  
  for(j in seq_along(obs)) {
    gogn_inner_loop <- tail(gogn_loop, obs[j])
    ts_inner <- ts(gogn_inner_loop, frequency = 12)
    
    fit <- auto.arima(ts_inner,
                      stepwise = FALSE)
    
    fc <- forecast(fit, h = 1)$mean
    
    spa_for[i, j] <- fc
  }
}

Sys.time() - byrja

# 5.8 klst 


saveRDS(spa_for, "spa_obs.rds")


# Accuracy

gogn_val <- UNRATE$UNRATE[(val_obs + 1), ] %>% as.data.frame()
gogn_val <- gogn_val %>% rownames_to_column(var = "date")
df_spa <- as.data.frame(spa_for) %>% as_tibble()
colnames(df_spa) <- paste0("obs_", obs)

df_spa$raun <- gogn_val$UNRATE
df_spa$date <- gogn_val$date

df_rmse <- df_spa %>% 
  pivot_longer(cols = starts_with("obs"),
               values_to = "gildi",
               names_to = "obs") %>% 
  group_by(obs) %>% 
  summarise(rmse = RMSE(gildi, raun))


# out of sample

ur_train <- head(UNRATE, -18) %>%
  as.data.frame() %>% 
  rownames_to_column(var = "date") %>% 
  as_tibble()

ur_test <- tail(UNRATE, 18) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "date") %>% 
  as_tibble()

ur_train_ts <- ts(ur_train$UNRATE, frequency = 12)

fit_500 <- auto.arima(ur_train_ts,
                      stepwise = FALSE)

fc_500 <- forecast(fit_500,
                   h = 18)

fc_500 %>% 
  autoplot(include = 20) %>% 
  autolayer(UNRATE, series = ur_test)

plot(fc_500$mean, type = "l")
lines(ur_test$UNRATE, col = "red")
