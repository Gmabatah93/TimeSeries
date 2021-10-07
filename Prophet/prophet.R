library(readr)
library(dplyr)
library(ggplot2)
theme_set(theme_minimal())
library(prophet)

# Saturated Growth: ----

# Data
visits <- read_csv("https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_R.csv")
# - carrying capacity
visits$cap <- 8.5

# Model
visits_Model <- prophet(visits, growth = "logistic")

# Forecasting
# - future dataframe
visits_Future <- make_future_dataframe(visits_Model, periods = 1826)
visits_Future$cap <- 8.5
# - prediction
visits_Forecast <- predict(visits_Model, visits_Future)
plot(visits_Model, visits_Forecast)

visits$y <- 10 - visits$y
visits$cap <- 6
visits$floor <- 1.5
visits_Future$cap <- 6
visits_Future$floor <- 1.5
visits_Model_Min <- prophet(visits, growth = "logistic")
visits_Forecast <- predict(visits_Model_Min, visits_Future)
plot(visits_Model_Min, visits_Forecast)

# Trend Changepoints: ----

# Data
pm <- read_csv("https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_peyton_manning.csv")

# Model
pm_Model <- prophet(pm)
# - adjust prior scale
pm_Model_2 <- prophet(pm, changepoint.prior.scale = 0.5)
pm_Model_3 <- prophet(pm, changepoint.prior.scale = 0.001)

# Forecasting
# - future dataframe
pm_Future <- make_future_dataframe(pm_Model, periods = 731)
# - prediction
pm_Forecast <- predict(pm_Model, pm_Future)
plot(pm_Model, pm_Forecast)
# - add changepoints
plot(pm_Model, pm_Forecast) + 
  add_changepoints_to_plot(pm_Model)

pm_Future_2 <- make_future_dataframe(pm_Model_2, periods = 731)
pm_Forecast_2 <- predict(pm_Model_2, pm_Future_2)
plot(pm_Model_2, pm_Forecast_2)

pm_Future_3 <- make_future_dataframe(pm_Model_3, periods = 731)
pm_Forecast_3 <- predict(pm_Model_3, pm_Future_3)
plot(pm_Model_3, pm_Forecast_3)

# Specify changepoint location
pm_Model_4 <- prophet(pm, changepoints =  c('2014-01-01'))
pm_Future_4 <- make_future_dataframe(pm_Model_4, periods = 731)
pm_Forecast_4 <- predict(pm_Model_4, pm_Future_4)
plot(pm_Model_4, pm_Forecast_4)

# Seasonality, Holiday Effects, And Regressors: ----

# Data
pm <- read_csv("https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_peyton_manning.csv")
playoffs <- data_frame(
  holiday = 'playoff',
  ds = as.Date(c('2008-01-13', '2009-01-03', '2010-01-16',
                 '2010-01-24', '2010-02-07', '2011-01-08',
                 '2013-01-12', '2014-01-12', '2014-01-19',
                 '2014-02-02', '2015-01-11', '2016-01-17',
                 '2016-01-24', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
superbowls <- data_frame(
  holiday = 'superbowl',
  ds = as.Date(c('2010-02-07', '2014-02-02', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
holidays <- bind_rows(playoffs, superbowls)

# Model
pm_Model <- prophet(pm, holidays = holidays)

# Forecasting
# - future data
pm_Future <- make_future_dataframe(pm_Model, periods = 731)
# - prediction
pm_Forecast <- predict(pm_Model, pm_Future)

pm_Forecast %>% 
  select(ds, playoff, superbowl) %>% 
  filter(abs(playoff + superbowl) > 0) %>%
  tail(10)

prophet_plot_components(pm_Model, pm_Forecast)
