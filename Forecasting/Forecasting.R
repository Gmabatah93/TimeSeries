library(fpp3)
library(tsibble)
theme_set(theme_minimal())

# Chapter 2: Time Series Graphics ----

# tsibble() Objects
x_year <- tsibble(Year = 2015:2019,
                  Observation = c(123, 39, 78, 52, 110),
                  index = Year)
x_month <- tsibble(Month = c("2019 Jan","2019 Feb","2019 Mar","2019 Apr","2019 May") %>% yearmonth(),
                   Observation = c(50,23,34,30,25),
                   index = Month)

olympic_running
olympic_running %>% distinct(Sex)

PBS
PBS %>% 
  filter(ATC2 == "A10") %>% 
  select(Month, Concession, Type, Cost)
PBS_a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC/1e6)

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
prison <- prison %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)

# Time Plots
melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000) %>% 
  autoplot(Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

autoplot(PBS_a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

# Seasonal Plots
PBS_a10 %>%
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales") +
  expand_limits(x = ymd(c("1972-12-28", "1973-12-04")))

vic_elec %>%
  gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y="MW", title="Electricity demand: Victoria")
vic_elec %>%
  gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y="MW", title="Electricity demand: Victoria")
vic_elec %>%
  gg_season(Demand, period = "year") +
  labs(y="MW", title="Electricity demand: Victoria")

# Seasonal Subseries Plots
PBS_a10 %>%
  gg_subseries(Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips)) 
holidays %>%   
  autoplot(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")
holidays %>% 
  gg_season(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")
holidays %>%
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

# Scatter Plots
vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Demand) +
  labs(y = "GW",
       title = "Half-hourly electricity demand: Victoria")
vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Temperature) +
  labs(y = "Degrees Celsius",
       title = "Half-hourly temperatures: Melbourne, Australia")
vic_elec %>%
  filter(year(Time) == 2014) %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)")

visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))
visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")
visitors %>%
  pivot_wider(values_from=Trips, names_from=State) %>%
  GGally::ggpairs(columns = 2:9)

# Lag Plots
recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)
recent_production %>%
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

# Autocorrelation
recent_production %>% ACF(Beer, lag_max = 9)
recent_production %>%
  ACF(Beer) %>%
  autoplot() + labs(title="Australian beer production")

PBS_a10 %>%
  ACF(Cost, lag_max = 48) %>%
  autoplot() +
  labs(title="Australian antidiabetic drug sales")

# White Noise
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + labs(title = "White noise", y = "")
y %>%
  ACF(wn) %>%
  autoplot() + labs(title = "White noise")

# Chapter 3: Time Series Decomposition ----

# Population Adjustments
global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP/Population) +
  labs(title= "GDP per capita", y = "$US")

# Inflation Adjustments
print_retail <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing") %>%
  group_by(Industry) %>%
  index_by(Year = year(Month)) %>%
  summarise(Turnover = sum(Turnover))
aus_economy <- global_economy %>%
  filter(Code == "AUS")
print_retail %>%
  left_join(aus_economy, by = "Year") %>%
  mutate(Adjusted_turnover = Turnover / CPI * 100) %>%
  pivot_longer(c(Turnover, Adjusted_turnover),
               values_to = "Turnover") %>%
  mutate(name = factor(name,
                       levels=c("Turnover","Adjusted_turnover"))) %>%
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Turnover: Australian print media industry",
       y = "$AU")

# Time Series Components
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)
us_retail_employment %>% 
  autoplot(Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")
dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))
dcmp %>%components()
dcmp %>% components() %>% autoplot()
  dcmp %>% components() %>%
  as_tsibble() %>%
  autoplot(Employed, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")
dcmp %>% components() %>%
    as_tsibble() %>%
    autoplot(Employed, colour = "gray") +
    geom_line(aes(y=season_adjust), colour = "#0072B2") +
    labs(y = "Persons (thousands)",
         title = "Total employment in US retail")

# Moving Averages
global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")
aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  mutate(`5-MA` = slider::slide_dbl(Exports, mean,
                                    .before = 2, 
                                    .after = 2, 
                                    .complete = TRUE))
aus_exports %>%
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports") +
  guides(colour = guide_legend(title = "series"))

# X-11 Method

# STL Decomposition
us_retail_employment %>%
  model(STL(Employed ~ trend(window = 7) + season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

# Chapter 4: Time Series Features ----

# Simple Statistics
tourism %>%
  features(Trips, list(mean = mean)) %>%
  arrange(mean)

tourism %>% features(Trips, quantile)

# ACF 
tourism %>% features(Trips, feat_acf)

# STL
tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

tourism %>%
  features(Trips, feat_stl) %>%
  filter(
    seasonal_strength_year == max(seasonal_strength_year)
  ) %>%
  left_join(tourism, by = c("State", "Region", "Purpose")) %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))

# Chapter 5: Forecaster's Toolbox ----

# WorkFlow
# - tidy
gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)
# - visualize
gdppc %>%
  filter(Country == "Sweden") %>%
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")
# - specify & estimate
fit <- gdppc %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))
# - evaluate
# - forcast
fit %>%
  forecast(h = "3 years") %>%
  filter(Country == "Sweden") %>%
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")

# Simple Forcasting Methods
bricks <- aus_production %>% filter_index("1970 Q1" ~ "2004 Q4")
# - mean method
bricks %>% model(MEAN(Bricks))
# - naive method
bricks %>% model(NAIVE(Bricks))
# - seasonal naive method
bricks %>% model(SNAIVE(Bricks ~ lag("year")))
# - drift method
bricks %>% model(RW(Bricks ~ drift()))

# - Example: Australian Quarterly Beer Production
train <- aus_production %>% filter_index("1992 Q1" ~ "2006 Q4")
beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )
beer_fc <- beer_fit %>% forecast(h = 14)

beer_fc %>%
  autoplot(train, level = NULL) +
  autolayer(filter_index(aus_production, "2007 Q1" ~ .),colour = "black") +
  labs(y = "Megalitres",
       title = "Forecasts for quarterly beer production") +
  guides(colour = guide_legend(title = "Forecast"))

# - Example: Google Daily Closing Stock Price

# Chapter 6: Judgemental Forecasts ----
# Chapter 7: Regression Models ----
# Chapter 8: Exponential Smoothing ----

# Simple Exponential Smoothing
algeria_economy <- global_economy %>% filter(Country == "Algeria")
algeria_economy %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Algeria")
# - model
fit <- algeria_economy %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
fc <- fit %>%
  forecast(h = 5)
fc %>%
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  labs(y="% of GDP", title="Exports: Algeria") +
  guides(colour = "none")

# Holts Linear Trend 
aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population / 1e6)
autoplot(aus_economy, Pop) +
  labs(y = "Millions", title = "Australian population")
# - model
fit <- aus_economy %>%
  model(AAN = ETS(Pop ~ error("A") + trend("A") + season("N")))
fc <- fit %>% forecast(h = 10)
fc %>% 
  autoplot(aus_economy)

# Chapter 9: ARIMA Models ----

# Stationarity & Differencing
google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, ljung_box, lag = 10)

PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6) %>%
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12),
    `Doubly differenced log sales` =
      difference(difference(log(Cost), 12), 1)
  ) %>%
  pivot_longer(-Month, names_to="Type", values_to="Sales") %>%
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales",
      "Doubly differenced log sales"))
  ) %>%
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Corticosteroid drug sales", y = NULL)

# - unit test
google_2015 %>% features(Close, unitroot_kpss)
google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, unitroot_kpss)
# - determines the number of 1st differences to use
google_2015 %>% features(Close, unitroot_ndiffs)
# - determine the appropriate number of seasonal differences to use
aus_total_retail <- aus_retail %>%
  summarise(Turnover = sum(Turnover))
aus_total_retail %>%
  mutate(log_turnover = log(Turnover)) %>%
  features(log_turnover, unitroot_nsdiffs)
aus_total_retail %>%
  mutate(log_turnover = difference(log(Turnover), 12)) %>%
  features(log_turnover, unitroot_ndiffs)

# ARIMA
# - data
global_economy %>%
  filter(Code == "EGY") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian Exports")
# - model
fit <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))
fit %>% report()
# - forecast
fit %>% forecast(h=10) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")
# - ACF & PACF
global_economy %>%
  filter(Code == "EGY") %>%
  ACF(Exports) %>%
  autoplot()
global_economy %>%
  filter(Code == "EGY") %>%
  PACF(Exports) %>%
  autoplot()
