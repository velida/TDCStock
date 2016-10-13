
library(forecast)

## Forecast with ARIMA model
forecastArima <- function(x, n.ahead=30,p,d,q){
  
  myTs <- ts(x$close, start=1, frequency=256)
  fit.arima <- arima(myTs, order=c(p,d,q))
  fore <- forecast(fit.arima, h=n.ahead)
  upper <- fore$upper[,'95%']
  lower <- fore$lower[,'95%']
  trend <- as.numeric(fore$fitted)
  pred <- as.numeric(fore$mean)
  output <- data.frame(actual = c(x$close, rep(NA, n.ahead)),
                       trend = c(trend, rep(NA, n.ahead)),
                       #pred = c(trend, pred),
                       pred = c(rep(NA, nrow(x)), pred),
                       lower = c(rep(NA, nrow(x)), lower),                       
                       upper = c(rep(NA, nrow(x)), upper),                       
                       date = c(x$date, max(x$date) + (1:n.ahead))  
                       )
  return(output)
}