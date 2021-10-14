library(fpp3)
theme_set(theme_minimal())

#
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
dcmp %>% components()

dcmp %>% components() %>% autoplot()
dcmp %>% components() %>%
  as_tsibble() %>%
  autoplot(Employed, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")
dcmp %>% 
  components() %>%
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
  mutate(`3-MA` = slider::slide_dbl(Exports, mean,
                                    .before = 1, 
                                    .after = 1, 
                                    .complete = TRUE),
         `5-MA` = slider::slide_dbl(Exports, mean,
                                    .before = 2, 
                                    .after = 2, 
                                    .complete = TRUE),
         `7-MA` = slider::slide_dbl(Exports, mean,
                                    .before = 3, 
                                    .after = 3, 
                                    .complete = TRUE),
         `9-MA` = slider::slide_dbl(Exports, mean,
                                    .before = 4, 
                                    .after = 4, 
                                    .complete = TRUE)
  )
aus_exports %>%
  autoplot(Exports) +
  geom_line(aes(y = `3-MA`), colour = "blue") +
  geom_line(aes(y = `5-MA`), colour = "red") +
  geom_line(aes(y = `7-MA`), colour = "yellow") +
  geom_line(aes(y = `9-MA`), colour = "green") +
  labs(y = "% of GDP",
       title = "Total Australian exports",
       subtitle = "5-MA") +
  guides(colour = guide_legend(title = "series"))

# Estimating the trend-cycle with seasonal data
us_retail_employment_ma <- us_retail_employment %>%
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )
us_retail_employment_ma %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

# Additive Method
us_retail_employment %>%
  model(
    classical_decomposition(Employed, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")

# X-11 Method
x11_dcmp <- us_retail_employment %>%
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) %>%
  components()
x11_dcmp %>% 
  autoplot() +
  labs(title =
         "Decomposition of total US retail employment using X-11.")

# SEATS
seats_dcmp <- us_retail_employment %>%
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) %>%
  components()
autoplot(seats_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using SEATS")

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
# - data
algeria_economy <- global_economy %>% filter(Country == "Algeria")
# - eda
algeria_economy %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Algeria")
algeria_economy %>% ACF(y = Exports) %>% autoplot()
algeria_economy %>%
  model(stl = STL(Exports)) %>% 
  components() %>% 
  autoplot()

# - model
fit <- 
  algeria_economy %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
fit %>% glance()
fit %>% tidy()
fit %>% augment()
fit %>% components()
# - forecast
fc <- fit %>%
  forecast(h = 5)
fc %>%
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  labs(y="% of GDP", title="Exports: Algeria") +
  guides(colour = "none")

# Holts Linear Trend
# - data
aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population / 1e6)
# - eda
aus_economy %>%  
  autoplot(Pop) +
  labs(y = "Millions", title = "Australian population")
aus_economy %>% ACF(y = Exports) %>% autoplot()
aus_economy %>% 
  model(stl = STL(Pop)) %>% 
  components() %>% 
  autoplot()
# - model
fit <- aus_economy %>%
  model(AAN = ETS(Pop ~ error("A") + trend("A") + season("N")))
fit %>% glance()
fit %>% tidy()
fit %>% augment()
fit %>% components()
# - forecast
fc <- fit %>% forecast(h = 10)
fc %>% 
  autoplot(aus_economy)
# - model: damped
aus_economy %>%
  model(`Holt's method` = ETS(Pop ~ error("A") + trend("A") + season("N")),
        `Damped Holt's method` = ETS(Pop ~ error("A") + trend("Ad", phi = 0.9) + season("N"))) %>%
  forecast(h = 15) %>%
  autoplot(aus_economy, level = NULL) +
  labs(title = "Australian population", y = "Millions") +
  guides(colour = guide_legend(title = "Forecast"))


# - Internet Usage
www_usage <- as_tsibble(WWWusage)

www_usage %>% 
  autoplot(value) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")
www_usage %>% ACF(y = value) %>% autoplot()
www_usage %>% 
  model(stl = STL(value)) %>% 
  components() %>% 
  autoplot()

www_usage %>%
  stretch_tsibble(.init = 10) %>%
  model(SES = ETS(value ~ error("A") + trend("N") + season("N")),
        Holt = ETS(value ~ error("A") + trend("A") + season("N")),
        Damped = ETS(value ~ error("A") + trend("Ad") + season("N"))
  ) %>%
  forecast(h = 1) %>%
  accuracy(www_usage)


fit <- www_usage %>%
  model(Damped = ETS(value ~ error("A") + trend("Ad") + season("N")))
fit %>% tidy()
fit %>%
  forecast(h = 10) %>%
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")




# Holt-Winters
# - data
aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)/1e3)
# - eda
autoplot(aus_holidays, Trips) +
  labs(title = "Australian Trips")
aus_holidays %>% ACF(y = Trips) %>% autoplot()
aus_holidays %>%
  model(stl = STL(Trips)) %>% 
  components() %>% 
  autoplot()
# - model
fit <- aus_holidays %>%
  model(additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
        multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M")),
        additive_Damped = ETS(Trips ~ error("A") + trend("Ad") + season("A")),
        multiplicative_Damped = ETS(Trips ~ error("M") + trend("Ad") + season("M")))
fit %>% glance()
fit %>% tidy()
fit %>% augment()
fit %>% components()
# - forecast
fc <- fit %>% forecast(h = "3 years")
fc %>%
  autoplot(aus_holidays, level = NULL) +
  labs(title="Australian domestic tourism",
       y="Overnight trips (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

# - damped
sth_cross_ped <- pedestrian %>%
  filter(Date >= "2016-07-01",
         Sensor == "Southern Cross Station") %>%
  index_by(Date) %>%
  summarise(Count = sum(Count)/1000)

sth_cross_ped %>% autoplot(Count)
sth_cross_ped %>% ACF(y = Count) %>% autoplot()
sth_cross_ped %>%
  model(stl = STL(Count)) %>% 
  components() %>% 
  autoplot()

fit <- sth_cross_ped %>%
  filter(Date <= "2016-07-31") %>%
  model(hw = ETS(Count ~ error("M") + trend("Ad") + season("M")))
fit %>% glance()
fit %>% tidy()
fit %>% augment()
fit %>% components()

fc <- fit %>% forecast(h = "2 weeks") 
fc %>% 
  autoplot(sth_cross_ped %>% filter(Date <= "2016-08-14")) +
  labs(title = "Daily traffic: Southern Cross",
       y="Pedestrians ('000)")


#
# Chapter 9: ARIMA Models ----

# Stationarity & Differencing

# ARIMA
# - data
egy_economy <- global_economy %>%
  filter(Code == "EGY")
# - eda
egy_economy %>% 
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian Exports")
egy_economy %>% gg_tsdisplay(plot_type = "partial")
egy_economy %>% ACF(Exports) %>% autoplot()
egy_economy %>% PACF(Exports) %>% autoplot()
egy_economy %>% features(Exports, unitroot_kpss)
egy_economy %>% features(Exports, unitroot_ndiffs)
egy_economy %>% 
  mutate(diff_close = difference(Exports)) %>% 
  features(diff_close, ljung_box, lag = 10)
# - model
fit <- egy_economy %>% model(ARIMA(Exports))
fit %>% report()
fit %>% glance()
fit %>% tidy()
fit %>% augment()

fit2 <- egy_economy %>% model(ARIMA(Exports ~ pdq(4,0,0)))
fit2 %>% report()
# - forecast
fit %>% 
  forecast(h=10) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")
fit2 %>% 
  forecast(h=10) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")


# - data
caf_economy <- global_economy %>%
  filter(Code == "CAF")
# - eda
caf_economy %>% 
  autoplot(Exports) +
  labs(title="Central African Republic exports", y="% of GDP")
caf_economy %>% features(Exports, unitroot_kpss)
caf_economy %>% features(Exports, unitroot_ndiffs)
caf_economy %>% gg_tsdisplay(difference(Exports), plot_type='partial') # ACF: MA(3) | PACF: AR(2)
# - fit
fit <- caf_economy %>% 
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE))
fit %>% report()
fit %>% tidy()
fit %>% pivot_longer(!Country, names_to = "Model name",
                     values_to = "Orders")
fit %>% glance() %>% arrange(AICc) %>% select(.model:BIC)
# - diagnostic
fit %>%
  select(search) %>% 
  gg_tsresiduals()
fit %>% 
  augment() %>%
  filter(.model=='search') %>%
  features(.innov, ljung_box, lag = 10, dof = 3)
# - forecast
fit %>%
  forecast(h=5) %>%
  filter(.model=='search') %>%
  autoplot(global_economy)

# Seasonal ARIMA
# - data
leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) %>%
  mutate(Employed = Employed/1000) %>%
  select(Month, Employed)
# - eda
leisure %>% 
  autoplot(Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

leisure %>% features(Employed, unitroot_kpss)
leisure %>% features(Employed, unitroot_ndiffs)
leisure %>%
  gg_tsdisplay(difference(Employed, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")
