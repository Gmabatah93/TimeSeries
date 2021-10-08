# Forecasting At Scale

## The Prophet Forecasting Model
<img src="Images/ProphetModel.PNG" width="500">

- **g:** trend function which models non-periodic changes in the value of the
time series
- **s:** represents periodic changes (e.g., weekly and yearly seasonality),
- **h:** effects of holidays which occur on potentially irregular schedules over
one or more days
- **e:** idiosyncratic changes which are not
accommodated by the model

**_ADVATAGES_**
- **Flexibility:** We can easily accommodate seasonality with multiple periods and let the
analyst make different assumptions about trends
- Unlike with ARIMA models, the measurements do not need to be regularly spaced,
and we do not need to interpolate missing values e.g. from removing outliers.
- The forecasting model has easily interpretable parameters that can be changed by
the analyst to impose assumptions on the forecast. Moreover, analysts typically do
have experience with regression and are easily able to extend the model to include
new components

### Trend Model

**_Nonlinear: Saturated Growth_**

_When forecasting growth, there is usually some maximum achievable point: total market size, total population size, etc. This is called the carrying capacity, and the forecast should saturate at this point._

**_Linear: w/ Change Points_**

**_Trend Forecast Uncertainty_**

### Seasonality

### Holidays and Events

### Analyst-in-the-Loop Modeling

1. **_Capacities:_** Analysts may have external data for the total market size and can apply
that knowledge directly by specifying capacities.

2. **_Changepoints:_** Known dates of changepoints, such as dates of product changes, can
be directly specified.

3. **_Holidays and seasonality:_** Analysts that we work with have experience with which
holidays impact growth in which regions, and they can directly input the relevant
holiday dates and the applicable time scales of seasonality.

4. **_Smoothing parameters:_** By adjusting τ an analyst can select from within a range
of more global or locally smooth models. The seasonality and holiday smoothing
parameters (σ, ν) allow the analyst to tell the model how much of the historical
seasonal variation is expected in the future.
