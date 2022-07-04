library(forecast)
library(tseries)

autoplot(passenger)
#Die Zeitreihe scheint neben einer zufälligen Komponente auch eine positven (deterministischen) Trend sowie ein saisonales Muster aufzuweisen.
#Dasm Classical Decomposition Model  mit Trend und Saisonkomponente würde hier passen also y_t = S_t + T_t + X_t wobei S_t die Saison- und T_t die Trendkomponente beschreibt.
#Für eine stationäre Zeitreihe müsste hier eine vollständige Saison- und Trendbereinigung durchgeführt werden.

#b)
seaonal <- decompose(passenger)
deseason <- passenger - seaonal$seasonal
trend <- tslm(deseason ~ trend)
random <- trend$residuals

plot(random)
#Die zufällige Komponente scheint noch nicht vollständig nach white noise auszusehen, Es gibt also noch Möglichkeiten den Erklärungsgehalt der Zeitreihe durch ein Modell zu verbessern-

#c)


best_model <- auto.arima(random, max.p = 3, max.q = 3)
best_model

#Ich würde mich für das ARMA(1,1)Modell entscheiden

ggAcf(random)
ggPacf(random)
#Da weder die ACF noch die PACF einen cut nach einem bestimmten Lag aufweisen, scheinen AR- und MA-Modelle hier ungeeignet

#d)
#Um die zufällige Komponente zu modellieren möchten wir, dass diese nach white Noise aussieht bzw. von der Null nicht zu unterscheiden ist. 
#Graphisch könenn wir dies anhand der ACF/PACF beurteilen und erwarten keine signifikante Lags ein geeigeter Test ist hier der Ljung-Box-Test

ggAcf(best_model$residuals)
ggAcf(best_model$residuals^2)

#Die ACF der residuals weisen keine signifikanten Lags auf der quadrierten Residuen nur einen was für white noise spricht.


Box.test(best_model$residuals, type = 'Ljung-Box')

#Zum Niveau von alpha = 0.05 verwirft der Ljung-Box Test die Nullhypothese nicht , dass die Reisuden unabhängig sind.

#d)
stat_1 <- numeric()
stat_2 <- numeric()
for (i in 1:10000){
y <- arima.sim(model = list(ar = c(0.4,0.3)), n = 10000)
phi_hat <- ar.ols(y)

if (is.na(phi_hat$ar[1]) == T){stat_1[i] <- 0}
else {stat_1[i] <- phi_hat$ar[1]} 
if (is.na(phi_hat$ar[2]) == T){stat_2[i] <- 0}
else {stat_2[i] <- phi_hat$ar[2]} 
}
xmean(stat_1)
mean(stat_2)
#Für Konsistenz müsste phi_hat strebt in Wahrscheinlichkeit gegen das wahre phi gelte, dass nach dem Gesetz der großen Zahlen sich das phi anpassen müsste. Dies scheint hier entweder nicht 
# der Fall oder passiert erst bei einer sehr großen Stichprobengröße


 phi_hat <- matrix(nrow = 100, ncol = 6)
T_ <- c(20, 50, 100, 500, 1000, 5000)
for(j in seq_along(T_)){
  for(i in 1:100){
    y <- arima.sim(model = list(ar = c(0.4, 0.3)), n = T_[j] )
    ar1_fit <- ar(y, aic = FALSE, order.max = 1, method = "ols")
    phi_hat[i,j] <- ar1_fit$ar
  }
}
par(mfrow = c(3,2))
for(i in 1:6){
  hist(phi_hat[ ,i], xlim = c(-0.5, 1))
}