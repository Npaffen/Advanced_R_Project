library(forecast)
library(tseries)

set.seed(123)

y <- arima.sim(model = list(ar = c(0.95)), n = 110)

ar_1 <- Arima(y[1:100], order = c(1,0,0), include.mean = F)
ar_10 <- Arima(y[1:100], order = c(10,0,0), include.mean = F)

#c)
#In sample MS nur mean(residuals^2)

mean(ar_1$residuals^2)
mean(ar_10$residuals^2)
#Hier würden wir das Model mit dem höheren Freiheitsgerade wählen, da mehr Koeffiezenten die Daten im in-sample Fit immer besser erklären.
#d)
ar_f10 <- forecast(ar_1, 10)$mean
ar10_f10 <- forecast(ar_10, 10)$mean

ar_mse <- mean((ar_f10 - y[101:110])^2)
ar10_mse <- mean((ar10_f10 - y[101:110])^2)

ar_mse
ar10_mse
#Wir entscheiden uns für den AR-Prozess, da er den kleinenen MSE hat. Nicht überraschend, da wir erwarten, dass ein AR(1)-Modell die Daten eines AR(1)-Prozesses besser
#erklärt als ein ARMA(1,1)-Prozess


#Wenn die Zeitreihe bereits White Noise ist erwarten wir, dass kein Modell etwas zum Erklärungsgehalt der Zeitreihe beitragen kann. Die Modellierung würde also keinen Sinn machen.
#Mit einem white-noise Test können wir außerdem die Residuen analysieren

#b Die Elemente eiens White Noise Prozesses sind voneinander unabhängig. Dies impliziert Unkorreliertheit. In beiden Tests wird untersucht, ob die Autokorrelation der Zeitreihe
#verschieden von 0 sind. Kommt der Test zu dem Ergebnis, dass Korrelation vorhanden ist, dann handelt es sich wahrscheinlich nicht um White Noise. 
T_ <- 1000
ljung <- numeric()
boxp <- numeric()

for (i in 1:1000){
  
  y <- rnorm(T_)
  
  ljung[i] <- Box.test(y, lag = sqrt(T_), type = 'Box-Pierce')$p.value
  
  boxp[i] <- Box.test(y, lag = sqrt(T_), type = 'Ljung-Box')$p.value
}

mean(ljung<0.05)
mean(boxp<0.05)


T_ <- 1000
ljung <- numeric()
boxp <- numeric()

for (i in 1:1000){
  
  y <- arima.sim(model = list(ar= c(0.15)), n = T_)
  
  ljung[i] <- Box.test(y, lag = sqrt(T_), type = 'Box-Pierce')$p.value
  
  boxp[i] <- Box.test(y, lag = sqrt(T_), type = 'Ljung-Box')$p.value
}

mean(ljung<0.05)
mean(boxp<0.05)

#Im Fall , dass die H_0 flasch ist liegt der Ljung-Box Test häufig richtiger als der Box-Pierce Test (d). Im Fall, dass die H_0 warh ist, verwirft der Box-Pierce Test seltener 
#und liegt damit häufiger richtig (c). Allerdings testen wird zum Nivea alpha = 0.05 und gehen damit davon aus, dass der Test unter der H_0 in 5% der Fälle daneben liegt. Aus 
#statistischer Sicht entspricht das Ergebnis des Ljung-Box. Tests daher eher unserer Erwartung.