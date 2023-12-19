# IIP Data Analysis with Time Series and Regression Models
## Overview
This R script analyzes the Index of Industrial Production (IIP) data using time series and regression models. The dataset includes various economic indicators such as GDP, mining, manufacturing, electricity, general IIP, inflation, exports, imports, and GDP per capita.

## Libraries
Install and load the required R libraries.
```
library(ggplot2)
library(reshape2)
library(pastecs)
library(forecast)
library(lmtest)
library(urca)
library(PerformanceAnalytics)
library(FinTS)
library(TSA)
library(tseries)
library(fUnitRoots)
library(dynlm)
```

## Data Loading
Load the IIP dataset. Enter the file address in ```[Enter file address]```.
```
iip_data <- read.csv('[Enter file address]', header = TRUE)
```
Create time series objects for relevant variables. Note that the yers may differ based on your specific dataset.
```
gdp_data <- ts(iip_data$gdp, start = c(1996, 1), frequency = 4)
mining_data <- ts(iip_data$mining, start = c(1996, 1), frequency = 4)
# Repeat for other variables
```

## ARIMA Analysis
Perform ARIMA analysis on the dataset
```
# Plot and summarize mining data
plot(mining_data, ylab = "IIP Mining", main = "IIP Mining Quarterly")
stat.desc(mining_data)

# Autocorrelation and unit root tests
acf(mining_data)
adf.test(mining_data)

# Decompose and plot components
mining_component <- decompose(mining_data)
plot(mining_component)

# Seasonal adjustment and differencing
mining_seasonal_adjusted <- mining_data - mining_component$seasonal
mining_stationary_2 <- diff(mining_seasonal_adjusted, differences = 1)
plot(mining_stationary_2)

# Fit ARIMA model and make forecasts
fit_mining <- auto.arima(mining_data, trace = TRUE)
fc_mining <- forecast(fit_mining, h = 12, level = seq(5, 99, 10))
# Plot forecasts
plot(fc_mining, main = "Forecast of Mining", showgap = FALSE, fcol = "red", flty = 2)
```

## Mining ADL (AutoRegressive Distributed Lag) Analysis
Fit ADL model for data:
```
mining_adl <- dynlm(mining_data ~ L(mining_data) + L(mining_data, 2) + 
                      L(export_data) +
                      L(gdp_per_capita_data) + L(gdp_per_capita_data, 2) + L(gdp_per_capita_data, 3),
                       start = c(1996, 1))
summary(mining_adl)

# Forecast using the ADL model
fc_mining_adl <- forecast(mining_adl, h = 12, level = seq(5, 99, 10))
# Plot ADL forecasts
plot(fc_mining_adl, main = "Forecast of Mining Differences", showgap = FALSE, fcol = "red", flty = 2)
```
## Explroatory Data Analysis
Visualize indicators:
```
plot(inflation_data, ylab = "Consumer Price Index", main = "Quarterly Consumer Price Index")
plot(gdp_per_capita_data, ylab = "Average Income", main = "Average Income in 2015 USD")
# Repeat for other variables
```

Repeat for other industries as well. 

This script provides a comprehensive analysis of the IIP data, including time series decomposition, ARIMA modeling, ADL modeling, and exploratory data analysis. Adjustments and additional analyses can be made based on specific research questions and objectives.
