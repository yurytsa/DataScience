### create a time-series dataset based on the data

dts <- ts(AirPassengers, start=c(1949,1), end=c(1960,12), frequency=12)

dt## change the size of the graphs
options(repr.plot.width = 8, repr.plot.height = 8)
plot(dts, xlab="Years", ylab="Number of passengers")
grid(nx = NULL, ny = nx, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)


stationary_dts <- decompose(dts)

plot(stationary_dts)

acf(dts)

pacf(dts)

dts_arima <- arima(dts, order=c(2,1,1))
dts_arima

BIC(dts_arima)

library(forecast)
dts_fit <- forecast(dts_arima)
dts_fit

plot(dts_fit)

dts_autoarima <- auto.arima(dts)
dts_autoarima

dts_autoforecast <- forecast(dts_autoarima)
dts_autoforecast

plot(dts_autoforecast)
