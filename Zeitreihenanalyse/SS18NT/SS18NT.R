library(forecast)
library(tseries)

autoplot(ice)
#Die Zeitreihe weißt eine saisonale Komponente auf, welches an jährlich wiederkehrenden Strukturen sichtbar ist. Ein (deterministischer) Trend ist nicht zu beobachten.
#Die Zeitreihe kann also in eine saisonale Komponente s_t und eine zufällige Komponente zerlegt werden. y_t = s_t + X_t 
plot(ice)

#b)
season <- tslm(ice ~ -1 + season)
random <- season$residuals

ar1 <- Arima(random, order = c(1,0,0))
arma11 <- Arima(random, order = c(1,0,1))
arma21 <-  Arima(random, order = c(2,0,1))

summary(ar1)
summary(arma11)
summary(arma21)
#Ich würde mich für das ARMA(1,1) - Modell entscheiden, da es den kleinen AIC-Wert aufweist.

#c)

seasonal <- rep(season$coefficients, 2)
random <- forecast(arma11,  24)
forecast <- seasonal + random$mean
plot(forecast)

#d)
sqrt_mse <- predict(arma11,24)$se
lower <- forecast + sqrt_mse * qnorm(0.025)
upper <- forecast + sqrt_mse * qnorm(0.975)

plot(forecast, ylim= c(0,2000))
lines(lower, type = 'l', col = 'red')
lines(upper, type = 'l', col = 'blue')

  length(random)

























