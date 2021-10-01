library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
theme_set(theme_minimal())
library(prophet) # https://facebook.github.io/prophet/


#
# Decomposition ----

# Data 
ap <- AirPassengers
ts(ap, frequency = 12, start = c(1949,1))

# EDA
plot(ap)
# - stabalize variance
ap_log <- log(ap)
plot(ap_log)
# - decomposition
ap_decomp <- decompose(ap_log)
plot(ap_decomp, type = 'b', col = 'blue')

#
# Forecasting ----
library(forecast)
ap_arima <- auto.arima(ap_log)
attributes(ap_arima)
# - acf & pcf plots
acf(ap_arima$residuals)
pacf(ap_arima$residuals)
# - Ljung-Box test
Box.test(ap_arima$residuals, lag = 20, type = "Ljung-Box")
# - Residual Plot
hist(ap_arima$residuals, col = "red",
     xlab = "Error", freq = TRUE)
# - Forecast
ap_F <- forecast(object = ap_arima, h = 48)
autoplot(ap_F)
accuracy(ap_F)

# EXAMPLE: Tom Brady ----
library(wikipediatrend)
tb <- wp_trend(page = "Tom_Brady", 
               from = "2013-01-01", to = "2015-12-31")
# - EDA
summary(tb)
qplot(date, views, data = tb)
# - Missing Data & Log Transform
tb$views[tb$views == 0] <- NA
ds <- tb$date
y <- log(tb$views)
tb_df <- data.frame(ds, y)
qplot(ds, y, data = tb_df)
# - Model
tb_mod <- prophet(df = tb_df)
attributes(tb_mod)
# - Prediction
tb_future <- make_future_dataframe(tb_mod, periods = 365)
tb_forecast <- predict(tb_mod, tb_future)
tail(tb_forecast[c('ds','yhat','yhat_lower','yhat_upper')])
exp(8.089780)
# - Plot: Prediction
plot(tb_mod, tb_forecast)
prophet_plot_components(tb_mod, tb_forecast)

# EXAMPLE: Ethereum Price ----

# Data
ethereum <- read_csv(file = "Data/Ethereum.csv")

# EDA
# - convert date format
ethereum <- ethereum %>% 
  mutate(Date = dmy(Date))
# - Plot
qplot(Date, Close, data = ethereum,
      geom = 'line',
      main = "Ethereum Closing Prices (2015-2019)")
# - Dataframe: Log Transform 
ethereum_df <- 
  tibble(ds = ethereum$Date,
         y = ethereum$Close %>% log())
qplot(ds, y, data = ethereum_df,
      geom = 'line',
      main = "Ethereum Closing Prices in Log Scale (2015-2019)")
# - Model
ethereum_Model <- prophet(df = ethereum_df)
# - Forecast
ethereum_Future <- make_future_dataframe(m = ethereum_Model,
                                         periods = 365)
ethereum_Future %>% tail()
ethereum_Forecast <- predict(ethereum_Model, ethereum_Future)
# - Plot: Forecast (Log Scale)
plot(ethereum_Model, ethereum_Forecast)
dyplot.prophet(ethereum_Model, ethereum_Forecast)
prophet_plot_components(ethereum_Model, ethereum_Forecast)
# - Model Performance
ethereum_Pref_df <- 
  tibble(pred = ethereum_Forecast$yhat[1:1544],
         actual = ethereum_Model$history$y)
ethereum_Pref_df %>% 
  ggplot(aes(actual, pred)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, color = "red")

ethereum_CV <- cross_validation(ethereum_Model, horizon = 365, units = "days")
performance_metrics(ethereum_CV, rolling_window = 0.1)
plot_cross_validation_metric(ethereum_CV, metric = 'rmse', rolling_window = 0.1)

# EXAMPLE: Covid 19 ----

# Data
covid <- covid19.analytics::covid19.data(case = 'ts-confirmed')
# - clean
covid_US <- covid %>% 
  filter(Country.Region == "US") %>%
  select(-Province.State, -Lat, -Long) %>% 
  pivot_longer(cols = -Country.Region,
               names_to = "Date",
               values_to = "Count") %>% 
  select(-Country.Region) %>% 
  mutate(Date = ymd(Date))

# EDA
qplot(Date, Count, data = covid_US,
      geom = 'line',
      main = "USA: Covid-19 Confirmed Cases")

# Forecasting
# - restructure for prophet
covid_US_df <- 
  tibble(ds = covid_US$Date,
         y = covid_US$Count)
covid_US_Model <- prophet(covid_US_df)
# Predictions
covid_US_Future <- make_future_dataframe(covid_US_Model, periods = 28)
covid_US_Forecast <- predict(covid_US_Model, covid_US_Future)
# - plot: predictions
plot(covid_US_Model, covid_US_Forecast)
dyplot.prophet(covid_US_Model, covid_US_Forecast)
prophet_plot_components(covid_US_Model, covid_US_Forecast)

# Model Performance
covid_US_Resid_df <- 
  tibble(pred = covid_US_Forecast$yhat[1:618],
         actual = covid_US_Model$history$y)
covid_US_Resid_df %>% 
  ggplot(aes(actual, pred)) +
  geom_point(alpha = 0.1) +
  geom_abline(slope = 1, color = "red")

# EXAMPLE: Bike Rentals ----

# Data
bike <- read_csv("Data/day.csv")
# - clean
bike <- bike %>% 
  mutate(Date = ymd(dteday))

# EDA
qplot(Date, cnt, data = bike,
      geom = 'line',
      main = "Bike Rentals in DC")

# Model
# - 1
bike_df <- 
  tibble(ds = bike$Date,
         y = bike$cnt)
bike_Model <- prophet(bike_df)

# Forecast
bike_Future <- make_future_dataframe(bike_Model, periods = 10)
bike_Forecast <- predict(bike_Model, bike_Future)
# - plot
plot(bike_Model, bike_Forecast)
dyplot.prophet(bike_Model, bike_Forecast)
prophet_plot_components(bike_Model, bike_Forecast)

# Model Performance
bike_MP_df <- 
  tibble(pred = bike_Forecast$yhat[1:731],
         actual = bike_Model$history$y)

bike_MP_df %>% 
  ggplot(aes(actual, pred)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = FALSE, 
              color = 'red')
lm(pred~actual, data = bike_MP_df) %>% summary()

# Model 2 (Add Holidays)
bike_Spec <- prophet() %>% 
  add_country_holidays(country_name = "US")
bike_Model_2 <- fit.prophet(bike_Spec, bike_df)
# Forecast
bike_Future_2 <- make_future_dataframe(bike_Model_2, periods = 10)
bike_Forecast_2 <- predict(bike_Model_2, bike_Future_2)
# - plot
plot(bike_Model_2, bike_Forecast_2)
dyplot.prophet(bike_Model_2, bike_Forecast_2)
prophet_plot_components(bike_Model_2, bike_Forecast_2)
# Model Performance
bike_MP_df_2 <- 
  tibble(pred = bike_Forecast_2$yhat[1:731],
         actual = bike_Model_2$history$y)

bike_MP_df_2 %>% 
  ggplot(aes(actual, pred)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = FALSE, 
              color = 'red')
lm(pred~actual, data = bike_MP_df_2) %>% summary()

# Model 3 (Add Temperature)
# - add temp data
bike_df <- bike_df %>% 
  mutate(temp = bike$temp)
# - model
bike_Spec_3 <- prophet() %>% 
  add_country_holidays(country_name = "US") %>%
  add_regressor(name = "temp")
bike_Model_3 <- fit.prophet(bike_Spec_3, bike_df)

# - Forecast
temp <- c(bike_df$temp, runif(10, 0.1,0.3))
bike_Future_3 <- bike_Future_2 %>% 
  mutate(temp = temp)
bike_Forecast_3 <- predict(bike_Model_3, bike_Future_3)
# - plot: forecast 
plot(bike_Model_3, bike_Forecast_3)
dyplot.prophet(bike_Model_3, bike_Forecast_3)
prophet_plot_components(bike_Model_3, bike_Forecast_3)
# - Model Performance
bike_MP_df_3 <- 
  tibble(pred = bike_Forecast_3$yhat[1:731],
         actual = bike_Model_3$history$y)

bike_MP_df_3 %>% 
  ggplot(aes(actual, pred)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = FALSE, 
              color = 'red')
lm(pred~actual, data = bike_MP_df_3) %>% summary()

# Model 3 (Add Humidity)
# - add temp data
bike_df <- bike_df %>% 
  mutate(temp = bike$temp,
         hum = bike$hum)
# - model
bike_Spec_4 <- prophet() %>% 
  add_country_holidays(country_name = "US") %>%
  add_regressor(name = "temp") %>% 
  add_regressor(name = "hum")
bike_Model_4 <- fit.prophet(bike_Spec_4, bike_df)

# - Forecast
hum <- c(bike_df$hum, runif(10, 0.4,0.8))
bike_Future_4 <- bike_Future_3 %>% 
  mutate(hum = hum)
bike_Forecast_4 <- predict(bike_Model_4, bike_Future_4)
# - plot: forecast 
plot(bike_Model_4, bike_Forecast_4)
dyplot.prophet(bike_Model_4, bike_Forecast_4)
prophet_plot_components(bike_Model_4, bike_Forecast_4)
# - Model Performance
bike_MP_df_4 <- 
  tibble(pred = bike_Forecast_4$yhat[1:731],
         actual = bike_Model_4$history$y)

bike_MP_df_4 %>% 
  ggplot(aes(actual, pred)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = FALSE, 
              color = 'red')
lm(pred~actual, data = bike_MP_df_4) %>% summary()
