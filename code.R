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

iip_data <- read.csv('[Enter file address here]', header = TRUE)
gdp_data <- ts(iip_data$gdp, start = c(1996, 1), frequency = 4)
mining_data <- ts(iip_data$mining, start = c(1996, 1), frequency = 4)
manufacturing_data <- ts(iip_data$manufacturing, start = c(1996, 1), frequency = 4)
electricity_data <- ts(iip_data$electricity, start = c(1996, 1), frequency = 4)
general_data <- ts(iip_data$general, start = c(1996, 1), frequency = 4)
inflation_data <- ts(iip_data$inflation, start = c(1996, 1), frequency = 4)
export_data <- ts(iip_data$export, start = c(1996, 1), frequency = 4)
import_data <- ts(iip_data$import, start = c(1996, 1), frequency = 4)
gdp_per_capita_data <- ts(iip_data$gdp_per_capita, start = c(1996, 1), frequency = 4)


#### Mining ARIMA #############

mining_data <- ts(iip_data$mining, start = c(1996, 1), frequency = 4)
plot(mining_data, ylab = "IIP Mining", main = "IIP Mining Quarterly")
plot(inflation_data, ylab = "Consumer Price Index", main = "Quarterly Consumer Price Index")
stat.desc(mining_data)

acf(mining_data)
adf.test(mining_data)

mining_component <- decompose(mining_data)
plot(mining_component)

urkpssTest(mining_data, type = c("tau"), lags = c("short"), use.lag = NULL, doplot = TRUE)

mining_seasonal_adjusted <- mining_data- mining_component$seasonal
mining_stationary_2 <- diff(mining_seasonal_adjusted, differences = 1)

plot(mining_stationary_2)

fit_mining <- auto.arima(mining_data, trace = TRUE)
par(mfrow = c(2,2))
plot(fit_mining)
fc_mining <- forecast(fit_mining, h = 12, level = seq(5, 99, 10))
plot(fc_mining, 
     main = "Forecast of Mining",
     showgap = F, 
     fcol =  "red",
     flty = 2) ##### R-Squared = 0.7721527 ########

fit_mining_diff <- auto.arima(mining_stationary_2, trace = TRUE)
fc_mining_diff <- forecast(fit_mining_diff, h = 12, level = seq(5, 99, 10))

plot(fc_mining_diff, 
     main = "Forecast of Mining Differences",
     showgap = F, 
     fcol =  "red",
     flty = 2) ####### R-Squared = 1 ############

###########################

################# Mining ADL #####################

mining_adl <- dynlm(mining_data ~ L(mining_data) + L(mining_data, 2) + 
                      L(export_data) +
                      L(gdp_per_capita_data) + L(gdp_per_capita_data, 2) + L(gdp_per_capita_data, 3),
                       start = c(1996, 1))
summary(mining_adl) #### R -Squared = 0.4037 ######

fc_mining_adl <- forecast(mining_adl, h = 12, level = seq(5, 99, 10))

plot(fc_mining_adl, 
     main = "Forecast of Mining Differences",
     showgap = F, 
     fcol =  "red",
     flty = 2)
#########################

######## Manufacturing ARIMA #####################

manufacturing_data <- ts(iip_data$manufacturing, start = c(1996, 1), frequency = 4)
plot(manufacturing_data, ylab = "IIP Manufacturing", main = "IIP Manufacturing Quarterly")

stat.desc(manufacturing_data)

acf(manufacturing_data)
adf.test(manufacturing_data)

manufacturing_component <- decompose(manufacturing_data)
plot(manufacturing_component)

urkpssTest(manufacturing_data, type = c("tau"), lags = c("short"), use.lag = NULL, doplot = TRUE)

manufacturing_seasonal_adjusted <- manufacturing_data- manufacturing_component$seasonal
manufacturing_stationary_2 <- diff(manufacturing_seasonal_adjusted, differences = 1)

plot(manufacturing_stationary_2)

fit_manufacturing <- auto.arima(manufacturing_data, trace = TRUE)
fc_manufacturing <- forecast(fit_manufacturing, h = 12, level = seq(5, 99, 10))
plot(fc_manufacturing, 
     main = "Forecast of Manufacturing",
     showgap = F, 
     fcol =  "red",
     flty = 2)

fit_manufacturing_diff <- auto.arima(manufacturing_stationary_2, trace = TRUE)
fc_manufacturing_diff <- forecast(fit_manufacturing_diff, h = 12, level = seq(5, 99, 10))
plot(fc_manufacturing_diff, 
     main = "Forecast of Manufacturing Differences",
     showgap = F, 
     fcol =  "red",
     flty = 2)

######################################################################

###################### Electricity ARIMA #####################

plot(electricity_data, ylab = "IIP Electricity", main = "IIP Electricity Quarterly")

stat.desc(electricity_data)

acf(electricity_data)
adf.test(electricity_data)

electricity_component <- decompose(electricity_data)
plot(electricity_component)

urkpssTest(electricity_data, type = c("tau"), lags = c("short"), use.lag = NULL, doplot = TRUE)

electricity_seasonal_adjusted <- electricity_data- electricity_component$seasonal
electricity_stationary_2 <- diff(electricity_seasonal_adjusted, differences = 1)

plot(electricity_stationary_2)

fit_electricity <- auto.arima(electricity_data, trace = TRUE)
fc_electricity <- forecast(fit_electricity, h = 12, level = seq(5, 99, 10))
plot(fc_electricity, 
     main = "Forecast of Electricity",
     showgap = F, 
     fcol =  "red",
     flty = 2)

fit_electricity_diff <- auto.arima(electricity_stationary_2, trace = TRUE)
fc_electricity_diff <- forecast(fit_electricity_diff, h = 12, level = seq(5, 99, 10))
plot(fc_electricity_diff, 
     main = "Forecast of Electricity Differences",
     showgap = F, 
     fcol =  "red",
     flty = 2)
################################### Electricity ADL ##############################

electricity_adl <- dynlm(electricity_data ~ L(electricity_data) + 
                             L(gdp_per_capita_data)
                         + L(export_data)
                         +L(inflation_data),
                           start = c(1996, 1))
summary(electricity_adl)

plot(x = iip_data$gdp_per_capita, y = iip_data$electricity)

#########################################################
########################################
############# General ARIMA #############

plot(general_data, ylab = "IIP General", main = "IIP General Quarterly")

stat.desc(general_data)

acf(general_data)
adf.test(general_data)

general_component <- decompose(general_data)
plot(general_component)

urkpssTest(general_data, type = c("tau"), lags = c("short"), use.lag = NULL, doplot = TRUE)

general_seasonal_adjusted <- general_data- general_component$seasonal
general_stationary_2 <- diff(general_seasonal_adjusted, differences = 1)

plot(general_stationary_2)

fit_general <- auto.arima(general_data, trace = TRUE)
fc_general <- forecast(fit_general, h = 12, level = seq(5, 99, 10))
plot(fc_general, 
     main = "Forecast of General",
     showgap = F, 
     fcol =  "red",
     flty = 2)

plot(diff(mining_data), main = "Quarterly Difference in Mining IIP", 
     ylab = "Difference in Mining")
plot(diff(manufacturing_data), main = "Quarterly Difference in Manufacturing IIP", 
     ylab = "Difference in Manufacturing")
plot(diff(electricity_data), main = "Quarterly Difference in Electricity IIP", 
     ylab = "Difference in Electricity")
plot(diff(general_data), main = "Quarterly Difference in General IIP", 
     ylab = "Difference in General")

#################### Manufacturing ADL ####################

manufacturing_adl <- dynlm(log(manufacturing_data) ~ L(inflation_data)
                           + L(log(iip_data$gdp)) + L(log(iip_data$gdp), 2),
                    start = c(1996, 1))
bptest(manufacturing_adl)
summary(manufacturing_adl)

######################### General ADL ##################

general_adl <- dynlm(log(general_data) ~ L(log(general_data)) + L(inflation_data)
                     + L(log(gdp_per_capita_data)) + L(log(gdp_per_capita_data), 2),
                           start = c(1996, 1))
bptest(general_adl)
summary(general_adl)

plot(inflation_data, ylab = "Consumer Price Index", main = "Quarterly Consumer Price Index")
plot(gdp_per_capita_data, ylab = "Average Income", main = "Average Income in 2015 USD")
plot(export_data, ylab = "Export Ratio (%)", main = "Export to GDP Ratio")
plot(import_data, ylab = "Import Ratio (%)", main = "Import to GDP Ratio")
plot(gdp_data, ylab = "GDP", main = "GDP in 2015 USD")




