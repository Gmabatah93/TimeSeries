library(fpp3)
theme_set(theme_minimal())

#
# Prision ----
# data
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
prison <- prison %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)
# Olympic Running: ----
# data
olympic_running
# eda
olympic_running %>% distinct(Length)
olympic_running %>% distinct(Sex)

# Ansett Airlines ----
# data
ansett
melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000)
# eda: time plot
melsyd_economy %>%  
  autoplot(Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")


#
# Pharmaceutical Benefit Scheme ----

# data 
PBS

# eda
PBS %>% distinct(Concession)
PBS %>% distinct(Type)
PBS %>% 
  filter(ATC2 == "A10") %>% 
  select(Month, Concession, Type, Cost)
PBS_a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC/1e6)

# data: Anatomical Therapeutic Chemical (a10)
PBS_a10
# eda: 
# - time plot
PBS_a10 %>% 
  autoplot(Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales",
       subtitle = "a10")
# - seasonal plot
PBS_a10 %>%
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales") +
  expand_limits(x = ymd(c("1972-12-28", "1973-12-04")))
# - seasonal subseries plot
PBS_a10 %>%
  gg_subseries(Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")
# - lag
PBS_a10 %>% gg_lag(Cost, geom = "point") +
  labs(x = "lag(Beer, k)")
# - acf
PBS_a10 %>% ACF(Cost, lag_max = 48) %>% autoplot() +
  labs(title="Australian antidiabetic drug sales")




# data: Monthly Corticosteroid Drug Sales in Australia
h02 <- PBS %>% 
  filter(ATC2 == "H02") %>% 
  summarise(Cost = sum(Cost)/1e6)

# eda
# - plot: time plot
h02 %>% autoplot(Cost)
h02 %>% autoplot(log(Cost))
# - plot: ACF
h02 %>% gg_tsdisplay(difference( log(Cost), lag = 12 ), 
                     plot_type = 'partial', lag_max = 24)


# model
# - fit
fit <- h02 %>% model(
    A_301_012 = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)),
    auto = ARIMA(log(Cost))
  )
# - diagnotics
fit %>% report()
# - residuals
fit %>% 
  select(A_301_012) %>% 
  gg_tsresiduals(lag_max = 36)
fit %>% 
  augment() %>%
  filter(.model == "A_301_012") %>% 
  features(.innov, ljung_box, lag = 36, dof = 6)
# - forecast
fit %>% 
  select(A_301_012) %>% 
  forecast() %>% 
  autoplot(h02)
#
# Victoria (electricity demand) ----
# data: 
vic_elec
# eda: 
# - time plot
vic_elec %>% 
  autoplot()
# - time plot: (2014) 
vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Demand) +
  labs(y = "GW",
       title = "Half-hourly electricity demand: Victoria")
# - time plot: Temperature (2014)
vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Temperature) +
  labs(y = "Degrees Celsius",
       title = "Half-hourly temperatures: Melbourne, Australia")
# - seasonal plot (Daily)
vic_elec %>%
  gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y="MW", title="Electricity demand: Victoria")
# - seasonal plot (Weekly)
vic_elec %>%
  gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y="MW", title="Electricity demand: Victoria")
# - seasonal plot (Yearly)
vic_elec %>%
  gg_season(Demand, period = "year") +
  labs(y="MW", title="Electricity demand: Victoria")
# - scatter plot
vic_elec %>%
  filter(year(Time) == 2014) %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)")


#
# Australian Holiday Tourism ----
# data
holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips)) 
visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))
# eda
# - time plot
holidays %>%   
  autoplot(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")
# - seasonal plot (Yearly)
holidays %>% 
  gg_season(Trips, period = "year") +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")
# - seasonal subseries plot
holidays %>%
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")
# - time plot:
visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")
# - scatter plot matrix
visitors %>%
  pivot_wider(values_from=Trips, names_from=State) %>%
  GGally::ggpairs(columns = 2:9)


#
# Australian Beer Production ----
# data
recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)
# eda
# - time plot
recent_production %>% autoplot(Beer)
# - seasonal plot
recent_production %>% gg_season(Beer, period = "year") 
# - lag plot
recent_production %>% gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")
# - acf
recent_production %>% ACF(Beer, lag_max = 9)
recent_production %>% ACF(Beer) %>% autoplot() + 
  labs(title="Australian beer production")


#
# Global Economy (Sweden) ----

# data
global_economy
# - Population Adjustment 
gdppc <- global_economy %>% mutate(GDP_per_capita = GDP / Population)

# eda 
# - Sweden
gdppc %>% 
  filter(Country == "Sweden") %>% 
  autoplot() +
  labs(y = "$US", title = "GDP per capita for Sweden")

# model
fit <- gdppc %>% 
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

# forecast
# - All
fit %>% forecast(h = 3)
# - Sweden
fit %>% forecast(h = 3) %>% 
  filter(Country == "Sweden") %>% 
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")


#
# Global Economy (Australia) ----
# data
global_economy
# - filter: Austrialian Economy
aus_economy <- global_economy %>% filter(Code == "AUS")
# - filter: (Population Adjusted)
aus_economy_Adj <- aus_economy %>% mutate(Pop = Population/1e6)

# eda
# - time plot
aus_economy %>% 
  autoplot(GDP) +
  labs(title= "GDP", y = "$US")
# - time plot (population adjusted)
aus_economy %>%
  autoplot(GDP/Population) +
  labs(title= "GDP per capita", y = "$US")
# - moving average
aus_exports <- aus_economy %>%
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



# eda
# - plot: time plot
aus_economy_Adj %>% 
  autoplot(Pop) +
  labs(y = "Millions", title = "Australian population")
# - plot: ACF
aus_economy_Adj %>% ACF(Pop) %>% autoplot()
# - plot: stl dcmp
aus_economy_Adj_dcmp <- aus_economy_Adj %>% model(stl = STL(Pop))
aus_economy_Adj_dcmp %>% components() %>% autoplot()

# model
# - fit
fit <- aus_economy_Adj %>%
  model(Mean = MEAN(Pop),
        Naive = NAIVE(Pop),
        Drift = NAIVE(Pop ~ drift()),
        SES = ETS(Pop ~ error("A") + trend("N") + season("N")),
        Holt = ETS(Pop ~ error("A") + trend("A") + season("N")),
        D_Holt = ETS(Pop ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  )

# diagnostics
# - all
fit %>% report()
# - SES
fit %>% select(SES) %>% report()
# - Holt 
fit %>% select(Holt) %>% report()
# - Holt (damped)
fit %>% select(D_Holt) %>% report()

# forecast
fit %>% forecast(h = 10) %>% 
  autoplot(aus_economy_Adj, level = NULL)

# metrics
fit %>% forecast(h = 1) %>% accuracy(aus_economy_Adj)
#
# Global Economy (Algeria) ----

# data 
global_economy
# - filter: Algeria
algeria_economy <- global_economy %>% filter(Country == "Algeria")

# eda
# - plot: time plot
algeria_economy %>% autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Algeria")
# - plot: acf
algeria_economy %>% ACF(Exports) %>% autoplot()
# - plot: stl decomposition
algeria_dcmp <- algeria_economy %>% model(stl = STL(Exports))
algeria_dcmp %>% components()
algeria_dcmp %>% components() %>% as_tsibble() %>%
  autoplot(Exports, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")
algeria_dcmp %>% components() %>% autoplot()

# model
# - fit
fit <- algeria_economy %>%
  model(Mean = MEAN(Exports),
        Naive = NAIVE(Exports),
        Drift = NAIVE(Exports ~ drift()),
        SES = ETS(Exports ~ error("A") + trend("N") + season("N"))
  )

# diagnostics
# - all
fit %>% report()
# - SES
fit %>% select(SES) %>% report()

# forecast
fit %>% forecast(h = 5) %>% 
  autoplot(algeria_economy, level = NULL)

#
# Global Economy (Eygpt) ----

# data
global_economy
# - filter: Eygpt
egypt_economy <- global_economy %>% filter(Code == "EGY")

# eda
# - plot: time plot
egypt_economy %>% 
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Eygpt")
# - plot: acf
egypt_economy %>% ACF(Exports) %>% autoplot()
egypt_economy %>% PACF(Exports) %>% autoplot() # last sig spike "4"
# - plot: stl dcmp
dcmp <- egypt_economy %>% model(stl = STL(Exports))
dcmp %>% components() %>% autoplot()
# - null: data is stationary
egypt_economy %>% features(Exports, unitroot_kpss)

# Model: Benchmark
# - fit
fit_benchmark <- egypt_economy %>%
  model(Mean = MEAN(Exports),
        Naive = NAIVE(Exports),
        Drift = NAIVE(Exports ~ drift()))
# - diagnostic
fit_benchmark %>% glance()
# - forecast
fit_benchmark %>% forecast(h = 10) %>% autoplot(egypt_economy)
fit_benchmark %>% forecast(h = 10) %>% accuracy(egypt_economy)

# Model: Exponential Smoothig
# - fit
fit_exp <- egypt_economy %>% 
  model(Holt = ETS(Exports ~ error("A") + trend("A") + season("N")))
# - diagnostic
fit_exp %>% report()
# - forecast
fit_exp %>% forecast(h = 5) %>% autoplot(egypt_economy)
fit_exp %>% forecast(h = 5) %>% accuracy(egypt_economy)

# Model: ARIMA 
# - fit
fit_arima <- egypt_economy %>% 
  model(ARIMA(Exports))
fit_arima_manual <- egypt_economy %>% 
  model(ARIMA(Exports ~ pdq(4,0,0)))
# - diagnostic
fit_arima %>% report()
fit_arima_manual %>% report()
# - forecast
fit_arima %>% forecast(h = 10) %>% autoplot(egypt_economy)

#
# Global Economy (Central African Republic) ----

# data
global_economy
# - filter: CAF
caf_economy <- global_economy %>% filter(Code == "CAF")

# eda
# - plot: time plot
caf_economy %>% 
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Central African Republic")
# - plot: stl dcmp
dcmp <- caf_economy %>% model(stl = STL(Exports))
dcmp %>% components() %>% autoplot()
# - plot: acf
caf_economy %>% ACF(Exports) %>% autoplot()
caf_economy %>% PACF(Exports) %>% autoplot() # last sig spike "2"
# - null: data is stationary
caf_economy %>% features(Exports, unitroot_kpss) # not 
# - determine the number of differences
caf_economy %>% features(Exports, unitroot_ndiffs)
# - null: data is stationary
caf_economy %>% 
  mutate(diff_Exports = difference(Exports)) %>% 
  features(Exports, unitroot_kpss) 
caf_economy %>% 
  mutate(diff_Exports = difference(Exports)) %>% 
  features(Exports, ljung_box, lag = 10) 

# - plots: (differenced)
caf_economy %>% autoplot(difference(Exports)) + ggtitle("Differenced")
caf_economy %>% ACF(difference(Exports)) %>% autoplot() # Suggests "MA(3)"
caf_economy %>% PACF(difference(Exports)) %>% autoplot() # Suggests "AR(2)"

# Model: Benchmark
# - fit
fit_benchmark <- caf_economy %>%
  model(Mean = MEAN(Exports),
        Naive = NAIVE(Exports),
        Drift = NAIVE(Exports ~ drift()),
        Holt = ETS(Exports ~ error("A") + trend("A") + season("N"))
  )
# - diagnostic
fit_benchmark %>% glance()
# - forecast
fit_benchmark %>% forecast(h = 10) %>% autoplot(caf_economy, level = NULL)
fit_benchmark %>% forecast(h = 10) %>% accuracy(egypt_economy)


# Model: ARIMA 
# - fit
fit_arima <- caf_economy %>% 
  model(A_210 = ARIMA(Exports ~ pdq(2,1,0)),
        A_013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise = FALSE))
# - diagnostic
fit_arima %>% pivot_longer(cols = !Country, 
                           names_to = "Model_Name", values_to = "Orders")
fit_arima %>% report() %>% arrange(AICc) # best = search_310
# - forecast
fit_arima %>% forecast(h = 10) %>% autoplot(caf_economy, level = NULL)
# - residuals
fit_arima %>% select(search) %>% gg_tsresiduals()
# - null: residuals are white noise
fit_arima %>% augment() %>% 
  filter(.model == "search") %>% 
  features(.innov, ljung_box, lag = 10, dof = 3)

#
# US Employment ----
# data
us_employment
# - filter: Retail Trade
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

# eda
us_retail_employment %>% 
  autoplot(Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")
# - acf
us_retail_employment %>% ACF(Employed) %>% autoplot()
# - classical decomposition 
us_retail_employment %>% model(classical_decomposition(Employed, type = "additive")) %>%
  components() %>% autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")
# - stl decomposition
dcmp <- us_retail_employment %>% model(stl = STL(Employed))
dcmp %>% components()
dcmp %>% components() %>% as_tsibble() %>%
  autoplot(Employed, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")
dcmp %>% components() %>% autoplot()

dcmp_7 <- us_retail_employment %>% 
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) %>%
  components() %>% 
  select(-.model)
dcmp_7 %>% 
  model(NAIVE(season_adjust)) %>% 
  forecast() %>% 
  autoplot(dcmp_7)

fit_dcmp <- us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))
fit_dcmp %>%
  forecast() %>%
  autoplot(us_retail_employment)+
  labs(y = "Number of people",
       title = "US retail employment")
fit_dcmp %>% gg_tsresiduals()

us_retail_employment %>% 
  model(STL(Employed ~ trend(window = 7) + season(window = "periodic")),
            robust = TRUE) %>%
  components() %>%
  autoplot()
# - X-11 decomposition
x11_dcmp <- us_retail_employment %>%
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) %>%
  components()
x11_dcmp %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )
x11_dcmp %>% autoplot() +
  labs(title = "Decomposition of total US retail employment using X-11.")
# - SEATS
seats_dcmp <- us_retail_employment %>%
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) %>%
  components()
seats_dcmp %>% 
  autoplot() + 
  labs(title = "Decomposition of total US retail employment using SEATS")

#
# Austrialian Retail ----
# data
aus_retail
# - filter: News & Book
print_retail <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing") 
# - summarise: Turnover 
print_retail <- print_retail %>% 
  group_by(Industry) %>%
  index_by(Year = year(Month)) %>%
  summarise(Turnover = sum(Turnover))

# eda
# - time plot
print_retail %>% autoplot(Turnover)
# - time plot (Inflation Adjustment)
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


#
# Australian Tourism ----

# data
tourism
# - filter: Holidays
aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)/1e3)

# eda
# - summary statistics
tourism %>% 
  features(Trips, list(mean = mean)) %>% 
  arrange(-mean)
tourism %>% 
  features(Trips, quantile)
# - acf
tourism %>% 
  features(Trips, feat_acf)
# - decomposition
tourism %>% 
  features(Trips, feat_stl) %>% 
  ggplot(aes(trend_strength, seasonal_strength_year, color = Purpose)) +
  geom_point() +
  facet_wrap(~State)
tourism %>%
  features(Trips, feat_stl) %>%
  filter(seasonal_strength_year == max(seasonal_strength_year)) %>%
  left_join(tourism, by = c("State", "Region", "Purpose")) %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))


# eda
# - plot: time
aus_holidays %>% autoplot()
# - plot: ACF
aus_holidays %>% ACF() %>% autoplot()
# - plot: dcmp
dcmp <- aus_holidays %>% model(stl = STL(Trips))
dcmp %>% components() %>% autoplot()

# model
# - fit
fit <- aus_holidays %>%
  model(
    SES = ETS(Trips ~ error("A") + trend("N") + season("N")),
    Holt = ETS(Trips ~ error("A") + trend("A") + season("N")),
    HW_additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    HW_multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M"))
  )
# - forecast
fit %>% forecast(h = "3 years") %>% autoplot(aus_holidays, level = NULL)
# - metrics
fit %>% forecast(h = "3 years") %>% accuracy(aus_holidays)
#
# Australian Production ----

# data
aus_production
# - filer: 1992 >
recent_production <- aus_production %>% filter(year(Quarter) >= 1992)
# - filter: Year 1970 - 2004
production <- aus_production %>% filter_index("1992 Q1" ~ "2006 Q4")

# BRICKS
# eda
production %>% autoplot(Bricks) +
  labs(x = "Time", y = "Brick Production")

# model
# - mean method
fit_bricks_Mean <- production %>% model(MEAN(Bricks))
fit_bricks_Mean %>% report()
# - naive method
fit_bricks_Naive <- production %>% model(NAIVE(Bricks))
fit_bricks_Naive %>% report()
# - seasonal naive method
fit_bricks_SNaive <- production %>% model(SNAIVE(Bricks ~ lag("year")))
fit_bricks_SNaive %>% report()
# - drift method
fit_bricks_RW <- production %>% model(RW(Bricks ~ drift()))

# forecast
# - mean method
fit_bricks_Mean %>% forecast(h = 10) %>% 
  autoplot(production)
# - naive method
fit_bricks_Naive %>% forecast(h = 10) %>% 
  autoplot(production)
# - seasonal naive method
fit_bricks_SNaive %>% forecast(h = 10) %>% 
  autoplot(production)
# - drift method
fit_bricks_RW %>% forecast(h = 10) %>% 
  autoplot(production)


# BEER
# eda
production %>% autoplot(Beer) +
  labs(x = "Time", y = "Beer Production")

# model
# - train set
beer_train <- recent_production %>% 
  filter(year(Quarter) <= 2007)
# - fit
fit_beer <- beer_train %>% 
  model(
    Mean = MEAN(Beer),
    Naive = NAIVE(Beer),
    SNaive = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )
fit_beer %>% glance()
fit_beer %>% augment()

# forecast
# - forcast
fc_beer <- fit_beer %>% forecast(h = 10)
# - plot
fc_beer %>% 
  autoplot(recent_production, level = NULL) +
  labs(y = "Megaliters", title = "Forecast: Quarterly Beer Production")

# diagnostic
# - accuracy
fc_beer %>% accuracy(recent_production)
#
# Google Stock ----

# data
gafa_stock
# - filter: Google, Year >= 2015
google_stock <- gafa_stock %>% 
  filter(Symbol == "GOOG",
         year(Date) >= 2015) %>% 
  mutate(day = row_number()) %>% 
  update_tsibble(index = day, regular = TRUE)
# - filter: Year = 2015
google_2015 <- google_stock %>% filter(year(Date) == 2015)


# eda
google_2015 %>% autoplot(Close) +
  labs(title = "Google Stock (2015)")
# - null: data is stationary
google_2015 %>% features(Close, unitroot_kpss)
# - determine the appropriate number of differences
google_2015 %>% features(Close, unitroot_ndiffs)
# - null: data is stationary
google_2015 %>% 
  mutate(diff_close = difference(Close)) %>% 
  features(diff_close, ljung_box, lag = 10)
google_2015 %>% 
  mutate(diff_close = difference(Close)) %>% 
  features(diff_close, unitroot_kpss)

# model
fit_google <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    Naive = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )

# diagnostics
# - model
fit_google %>% select(Naive) %>% report()
# - residuals
fit_google %>% select(Naive) %>% augment()
# - plot: summary
fit_google %>% select(Naive) %>% gg_tsresiduals()
# - statistical tests
fit_google %>% select(Naive) %>% augment() %>% 
  features(.innov, box_pierce, lag = 10, dof = 0) # H0:  
fit_google %>% select(Naive) %>% augment() %>% 
  features(.innov, ljung_box, lag = 10, dof = 0) # H0: 


# forecast
# - data: 2015
# - plot
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  autoplot(google_2015)

fit_google %>% select(Naive) %>% 
  forecast(h = 10) %>% 
  hilo()

# - data: January 2016
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
# - fc
fc_google <- fit_google %>% forecast(google_jan_2016)
fc_google %>% 
  autoplot(bind_rows(google_2015, google_jan_2016), level = NULL)
# - metrics
fc_google %>% accuracy(google_stock)
# - bootstraping
boot_google <- fit_google %>% select(Naive) %>% 
  generate(h = 30, times =  5, bootstrap = TRUE)

google_2015 %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = boot_google, colour = as.factor(.rep)),
            data = boot_google) +
  labs(title="Google daily closing stock price", y="$US" )
#

# Egg Prices ----

# data
eggs <- prices %>% 
  select(year, eggs) %>% 
  filter(!is.na(eggs))

# eda
eggs %>% autoplot()
eggs %>% ACF() %>% autoplot()

# model
fit_eggs <- eggs %>% model(RW(log(eggs) ~ drift()))
fit_eggs %>% report()

# forecast
fit_eggs %>% forecast(h = 50) %>% 
  autoplot(eggs, 
           level = 80, point_forecast = lst(mean,median))
#
# Internet Usage ----

# data
www_usage <- as_tsibble(WWWusage)

# eda
# - plot: time
www_usage %>% 
  autoplot() +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")
# - plot: acf
www_usage %>% ACF(value) %>% autoplot()
# - plot: stl dcmp
dcmp <- www_usage %>% model(stl = STL(value))
dcmp %>% components() %>% autoplot()

# model
# - fit
fit <- www_usage %>%
  stretch_tsibble(.init = 10) %>%
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") + season("N"))
  )
# - diagnostics
fit %>% forecast(h = 1) %>% accuracy(www_usage)
# - final fit
fit_final <- www_usage %>%
  model(
    Damped = ETS(value ~ error("A") + trend("Ad") + season("N"))
  )
fit_final %>% report()
# - plot: forecast
fit_final %>%
  forecast(h = 10) %>%
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

#
# Leisure ----

# data 
us_employment
# - filter: Leisure & Hospitality
leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) %>%
  # population adjustment
  mutate(Employed = Employed/1000) %>%
  select(Month, Employed)

# eda
# - plot: time plot
leisure %>% 
  autoplot(Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")
# - plot: time plot (seasonal difference)
leisure %>% 
  autoplot(difference(Employed, 12)) +
  labs(title = "US employment: leisure and hospitality",
       subtitle = "Seasonal Difference",
       y="Number of people (millions)")
# - plot: time plot (seasonal & 1st difference)
leisure %>% 
  autoplot(difference(Employed, 12) %>% difference()) +
  labs(title = "US employment: leisure and hospitality",
       subtitle = "Seasonal Difference",
       y="Number of people (millions)")
# - plot: stl dcmp
leisure %>% 
  model(stl = STL(Employed)) %>% 
  components() %>% 
  autoplot()
# - plot: ACF & PACF
leisure %>% gg_tsdisplay(Employed, plot_type = "partial")
leisure %>% gg_tsdisplay(difference(Employed, 12), plot_type = "partial", lag = 36)
leisure %>% gg_tsdisplay(difference(Employed, 12) %>% difference(),
                         plot_type = "partial", lag = 36)
                      # - ACF lag2: non-seasonal MA(2)
                      # - ACF lag12: seasonal MA(1)     (0,1,2)(0,1,1)12
                      # - ACF lag2: non-seasonal MA(2)
                      # - PACF lag                      (2,1,0)(0,1,1)12                   
    
# null: data is stationary
leisure %>% features(Employed, unitroot_kpss) # not 
# - determine the number of differences
leisure %>% features(Employed, unitroot_ndiffs)
# - null: data is stationary
leisure %>% 
  mutate(diff_Employed = difference(Employed)) %>% 
  features(diff_Employed, unitroot_kpss) 
leisure %>% 
  mutate(diff_Employed = difference(Employed)) %>% 
  features(diff_Employed, ljung_box, lag = 10)

# Model: ARIMA
# - fit
fit_arima <- leisure %>%
  model(
    arima_012_011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima_210_011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
# - diagnostics
fit_arima %>% pivot_longer(everything(), names_to = "Model name",
                           values_to = "Orders")
glance(fit_arima) %>% arrange(AICc) %>% select(.model:BIC)
# - residuals
fit_arima %>% select(auto) %>% gg_tsresiduals(lag = 36)
# - null: residuals are white noise
augment(fit_arima) %>% features(.innov, ljung_box, lag=24, dof=4)
# - plot
fit_arima %>% forecast(h=36) %>%
  filter(.model=='auto') %>%
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

#
# Chapter 7: Regression Models ----

# Simple Linear Regression
# - data
us_change
# - eda
us_change %>%
  pivot_longer(c(Consumption, Income), names_to="Series") %>%
  autoplot(value) +
  labs(y = "% change")
us_change %>%
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# - fit
fit <- us_change %>% model(TSLM(Consumption ~ Income))
fit %>% report()

# Multiple Linear Regression
# - eda
us_change %>% 
  pivot_longer(c(Production,Savings,Unemployment), names_to="Series") %>% 
  ggplot(aes(Quarter, value)) +
  geom_line(aes(color = Series)) +
  facet_wrap(~Series, ncol = 1, scales = "free_y")
us_change %>% 
  GGally::ggpairs(columns = 2:6)
# - fit
fit <- us_change %>% model(tslm = TSLM(Consumption ~ Income + Production + Unemployment + Savings))
fit %>% report()
# - diagnostics
fit %>% 
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL, title = "Percent change in US consumption expenditure") +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))
fit %>% 
  augment() %>%
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)
fit %>% gg_tsresiduals()
fit %>% 
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 5)
us_change %>%
  left_join(residuals(fit), by = "Quarter") %>%
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") %>%
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")
augment(fit) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")


# Australian quarterly beer production
# - data
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
# - eda
recent_production %>%
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")
# - fit
fit <- recent_production %>% model(TSLM(Beer ~ trend() + season()))
fit %>% report()
fit %>% 
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))
fit %>% 
  augment() %>%
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

fit2 <- us_change %>% model(lm = TSLM(Consumption ~ Income + Savings + Unemployment))

# - forecast
fc <- forecast(fit)
fc %>%
  autoplot(recent_production) +
  labs(title = "Forecasts of beer production using regression", y = "megalitres")

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) %>%
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease = new_data(us_change, 4) %>%
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenario")

fc2 <- forecast(fit2, new_data = future_scenarios)
us_change %>%
  autoplot(Consumption) +
  autolayer(fc2) +
  labs(title = "US consumption", y = "% change")
#
# Chapter 10: Dynamic Regression Models ----
# - eda
us_change %>%
  pivot_longer(c(Consumption, Income),
               names_to = "var", values_to = "value") %>%
  ggplot(aes(x = Quarter, y = value)) +
  geom_line(aes(color = var), show.legend = FALSE) +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "US consumption and personal income",
       y = "Quarterly % change")
# - model
fit <- us_change %>% model(ARIMA(Consumption ~ Income))
fit %>% report()
# - diagnostics
bind_rows(`Regression residuals` = as_tibble(residuals(fit, type = "regression")),
          `ARIMA residuals` = as_tibble(residuals(fit, type = "innovation")), # ARIMA residuals
          .id = "type") %>%
  mutate(type = factor(type, levels=c("Regression residuals", "ARIMA residuals"))) %>%
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

fit %>% gg_tsresiduals()
fit %>% # H0: White Noise 
  augment() %>% 
  features(.innov, ljung_box, dof = 5, lag = 8)
# - forecasting
us_change_future <- 
  us_change %>%  
  new_data(8) %>%
  mutate(Income = mean(us_change$Income))
fit %>% 
  forecast(new_data = us_change_future) %>%
  autoplot(us_change) +
  labs(y = "Percentage change")

# Electiricity Demand
# - data 
vic_elec_daily <- vic_elec %>%
  filter(year(Time) == 2014) %>%
  index_by(Date = date(Time)) %>%
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) %>%
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))
# - eda
vic_elec_daily %>%
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() +
  labs(y = "Electricity demand (GW)",
       x = "Maximum daily temperature")
vic_elec_daily %>%
  pivot_longer(c(Demand, Temperature)) %>%
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") + 
  ylab("")
# - fit
fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) +
                (Day_Type == "Weekday")))
fit %>% report()
# - diagnostics
fit %>% gg_tsresiduals()
fit %>% 
  augment() %>%
  features(.innov, ljung_box, dof = 9, lag = 14)
# - forecast
vic_elec_future <- 
  new_data(vic_elec_daily, 14) %>%
  mutate(
    Temperature = 26,
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
fit %>% 
  forecast(vic_elec_future) %>%
  autoplot(vic_elec_daily) +
  labs(title="Daily electricity demand: Victoria",
       y="GW")

# Air Passengers'
# - data
aus_airpassengers
# - eda 
aus_airpassengers %>%
  autoplot(Passengers) +
  labs(y = "Passengers (millions)",
       title = "Total annual air passengers")
# - model
fit_deterministic <- 
  aus_airpassengers %>%
  model(deterministic = ARIMA(Passengers ~ 1 + trend() + pdq(d = 0)))
fit_stochastic <- 
  aus_airpassengers %>%
  model(stochastic = ARIMA(Passengers ~ pdq(d = 1)))

fit_deterministic %>% report()
fit_stochastic %>% report()
# - forecast
aus_airpassengers %>%
  autoplot(Passengers) +
  autolayer(fit_stochastic %>% forecast(h = 20),
            colour = "#0072B2", level = 95) +
  autolayer(fit_deterministic %>% forecast(h = 20),
            colour = "#D55E00", alpha = 0.65, level = 95) +
  labs(y = "Air passengers (millions)",
       title = "Forecasts from trend models")

# TV Advertising & Insurance
# - data
insurance
# - eda
insurance %>%
  pivot_longer(Quotes:TVadverts) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Insurance advertising and quotations")
# - model
fit <- insurance %>%
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) %>%
  # Estimate models
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts)),
    lag2 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts) + lag(TVadverts, 2)),
    lag3 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts) + lag(TVadverts, 2) + lag(TVadverts, 3))
  )
fit %>% glance() # choose optimal lag
fit_best <- insurance %>% 
  model(ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts)))
fit_best %>% report()
# - forecast
insurance_future <- new_data(insurance, 20) %>%
  mutate(TVadverts = 8)
fit_best %>%
  forecast(insurance_future) %>%
  autoplot(insurance) +
  labs(
    y = "Quotes",
    title = "Forecast quotes with future advertising set to 8"
  )
