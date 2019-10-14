
# Data and wrangling
library(quantmod)
library(tidyverse)
library(lubridate)

# Forecast
library(forecast)
# library(forecastHybrid)
# library(caret)

# Paralell
library(foreach)
library(doParallel)
library(parallel)

getSymbols('UNRATE', src='FRED')
getSymbols('ICSA', src = 'FRED')


# -------------------------------------------------------------------------

ur <- UNRATE %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "date") %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date))

icsa <- ICSA %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "date") %>% 
  as_tibble()


# -------------------------------------------------------------------------
# Skv. https://www.census.gov/programs-surveys/cps/technical-documentation/methodology/collecting-data.html
# er interview week sú vika sem inniheldur 12 hvers mánaðar er sú vika sem könnunin fer fram
# Nota þær upplýsingar til að filtera út alla dagsetningar um eða undir 

dags_vika_12 <- seq.Date(from = as.Date(icsa$date[1]),
                         to = as.Date(tail(icsa$date, 1)),
                         by = "day")

df_dags <- tibble(date = dags_vika_12) %>% 
  mutate(vika = week(date),
         ar_vika = paste0(year(date),"-", vika)) %>% 
  filter(day(date) == 12)



# -------------------------------------------------------------------------

icsa <- icsa %>% 
  mutate(date = as.Date(date),
         vika = week(date),
         ar_vika = paste0(year(date), "-", vika))

icsa <- icsa %>% 
  filter(ar_vika %in% df_dags$ar_vika)

icsa <- icsa %>% 
  mutate(date = floor_date(date, "month"))


ur <- ur %>% 
  left_join(icsa) %>% 
  na.omit()


# -------------------------------------------------------------------------

# unemployment
ur_ts <- ts(ur$UNRATE,
            start = 1967,
            frequency = 12)

ur_train <- window(ur_ts,
                   end = c(2016, 12))

ur_test <- window(ur_ts,
                  start = c(2017, 1))

  
# initial claims
init_ts <- ts(ur$ICSA,
              start = 1967,
              frequency = 12)


init_train <- window(init_ts,
                     end = c(2016, 12))

init_test <- window(init_ts,
                    start = c(2017, 1))


# Fyrir out of sample spána í lokin
init_total <- ts(icsa$ICSA,
                 start = 1967,
                 frequency = 12)

# Líkan -------------------------------------------------------------------

arima_ur_xreg <- auto.arima(ur_train,
                       xreg = init_train,
                       stepwise = FALSE)

arima_ur <- auto.arima(ur_train,
                       stepwise = FALSE)

# ARIMAX virðist nákvæmara.
accuracy(arima_ur_xreg)
accuracy(arima_ur)


# Out of sample fyrir september

arima_xreg_full <- auto.arima(head(ur_ts, -1),
                              xreg = head(init_ts, -1),
                              stepwise = FALSE,
                              approximation = FALSE)

arima_full <- auto.arima(head(ur_ts, -1),
                         stepwise = FALSE,
                         approximation = FALSE)

ets_full <- ets(head(ur_ts, -1))

arima_xreg_full %>% 
  forecast(h = 1, xreg = tail(init_total, 1))

arima_full %>% 
  forecast(h = 1)

ets_full %>% 
  forecast(h = 1)


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
