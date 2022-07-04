#Aufgabe 1
autoplot(passenger)
#Die Zeitreihe scheint dem Classical Decomposition Model aus der Vorlesung mit Trend und saisonaler Kompnente zu entsprechen.
#Um Stationarität zu erreichen wären folgnde Schritte notwenig. Vorläufige Trendschätzung und Saisonschätzug. Bereinung der saisonalen Komponente sowie anschließende erneute Trendbereinigung.

#b
decomp_pass <- decompose(passenger)
desason_pass <- passenger - decomp_pass$seasonal
detrend_pass <- tslm(desason_pass~trend)
random_pass <- detrend_pass$residuals
autoplot(random_pass)
#Da in den geplotteten Residuen keine deterministischen Strukturen mehr erkennbar sind, kann man auf Stationarität schließen.

#c

acf(random_pass)
pacf(random_pass)
#Acf und pacf sprechen gegen ein AR oder MA Modell, da keine klaren Cutoffs zu erkennen sind.

pq  <- expand.grid(ar = 1:3, ma = 1:3)

model <- list()
aic <- c()

for (i in 1:nrow(pq)){
 model[[i]] <- Arima(random_pass, order = c(pq[i,1],0,pq[i,2]))
 
 aic[i] <- model[[i]]$aic
  }

model

best_model <- which.min(aic)
best_model

#Das Arma(1,1) Model hat den niedrigsten AIC und wird deswegen gewählt

#d)
acf(model[[1]]$residuals)
acf(model[[1]]$residuals^2)

# Die quadrierten Residueen des Modells sehen nach white noise aus, da wir nur 1-2 signifkante Lags beobachten Das Modell scheint also zu passen

#Mit einem Ljung-Box Test können wir die Nullhypothese ob die Reiduen von Null verschieden sind überprüfen
Box.test(model[[1]]$residuals, lag = round(sqrt(length(passenger))), type = 'Ljung-Box')


#Aufgabe 2d
phi_hat <- matrix(nrow = 100, ncol = 6)
T_ <- c(20,50,100,200,1000, 5000)

for(j in 1:length(T_)){
  for (i in 1:100){
  y <- arima.sim(model = list(ar = c(0.4, 0.3)), n = T_[j])
model <- ar(y, order.max = 1, aic = FALSE, method = 'ols') 
phi_hat[i,j] <- model$ar
}
}
par(mfrow = c(3,2))
for (i in 1:6){
  hist(phi_hat[,i], xlim = c(-0.5, 1))
}


#NT17
set.seed(123)
y <- arima.sim(model=list(ar = c(0.95)), n = 110)

ar1 <- arima(window(y,end = 100 ),order = c(1,0,0), include.mean = F )
ar10 <- arima(window(y,end = 100 ),order = c(10,0,0), include.mean = F)

mse_ar1 <- mean( ar1$residuals^2)
mse_ar10 <- mean( ar10$residuals^2)

mse_ar1
mse_ar10

#Der in-sample-fit des AR(10)-Modells ist besser. Jedoch keine Überraschung, mehr Parameter für die Schätzung genutzt wurden (Problem des Overfittings). Deswegen ist der MSE hier 
#nicht als Modelwahlkriterium geeignet.

ar1_pred_10 <- predict(ar1, 10)

oos_mse_ar1 <- mean((ar1_pred_10$pred - window(y,start = 100 ))^2)

ar10_pred_10 <- predict(ar10, 10)

oos_mse_ar10 <- mean((ar10_pred_10$pred - window(y,start = 100 ))^2)
oos_mse_ar1
oos_mse_ar10

#Im OOS- MSE schneidet das AR(1) - Modell besser ab. Hier würden wir uns für das richtige Modell entscheiden.

#Afg 4
#a
#Wenn die Zeireihe white noise ist, gibt es keine Abhängigkeiten die mit einem (ARMA)- Modell erklärt werden könnten.
#Eine weitere Anwendung für einen white-noise Test ist die Analyse von Residuen.

#b)
#Die Grundidee beider Test besteht darin die Nullhypothese zu testen, dass die vorliegenden Residuen unterschiedlich von Null sind. Wenn die Nullhypothese nicht abgelehnt wird,
#kann man davon ausgehen, dass die Reisduen keien Korrelation mehr beinhalten

#c)
ljung <- numeric()
boxpierce <- numeric()

for(i in 1:1000){
  y <- arima.sim(model = list(order(0,0,1)), n = 1000)
ljung[i] <- Box.test(y, lag = sqrt(length(y)), type = 'Ljung-Box')$p.value
boxpierce[i] <- Box.test(y, lag = sqrt(length(y)), type = 'Box-Pierce')$p.value
  }

mean(ljung  <0.05)
mean(boxpierce<0.05)

#d
ljung <- numeric()
boxpierce <- numeric()

for(i in 1:1000){
  y <- arima.sim(model = list(ar = c(0.15)), n = 1000)
  ljung[i] <- Box.test(y, lag = sqrt(length(y)), type = 'Ljung-Box')$p.value
  boxpierce[i] <- Box.test(y, lag = sqrt(length(y)), type = 'Box-Pierce')$p.value
}

mean(ljung  <0.05)
mean(boxpierce<0.05)

library(dplyr)
  'In Ihrer R-Arbeitsumgebung ist die Zeitreihe einer Vogelpopulation für die Jahre 1900 bis 2010 (birds)
geladen. Führen Sie für ein AR(1)- und ein ARMA(1,1)-Modell einen Rolling Forecast für den Zeitraum
von 2001 bis 2010 durch. D.h., passen Sie für jeden Prognoseschritt das jeweilige Modell an die 100 direkt
vorangegangenen Beobachtungen an (d.h. nutzen Sie eine Fensterbreite von 100). Welches Modell liefert die
besseren Prognosen?
'
f_ar1 <- numeric()
f_arma11 <- numeric()

for (i in 1:10){
  f_ar1[i] <- window(birds, start = 1900+i , end = 1999+i) %>% Arima(order = c(1,0,0)) %>% forecast(1) %>% .$mean
  
  f_arma1[i] <- window(birds, start = 1900+i , end = 1999+i) %>% Arima(order = c(1,0,1)) %>% forecast(1) %>% .$mean
}
(msfe_ar <- mean((f_ar1 - window(birds, start = 2001))^2))

(msfe_arma <- mean((f_arma1 - window(birds, start = 2001))^2))


Erzeugen Sie 100 Realisationen des folgenden Prozesses
Yt = 0.8Yt−1 + t
, t
iid∼ N(0, 1) t = 1, ... , 10.
Passen Sie jeweils ein AR(1)-Modell ohne Konstante an die erzeugten Daten an und speichern Sie alle
so geschätzten Koezienten. Berechnen Sie anschlieÿend das arithmetische Mittel über alle geschätzten
Koezienten. Erläutern Sie kurz, wieso mit diesem Vorgehen E[φ^] approximiert werden kann. Interpretieren
Sie Ihr Ergebnis
set.seed(123)
ar <- numeric()
for (i in 1:100){
yt <- arima.sim(model = list(ar = c(0.8)), n = 10)
ar[i] <- ar.ols(yt , order.max = 1, aic = F)$ar[1]
}
mean(unlist(ar))

#Durch das wiederholte Erzeugen des Prozesses kann man durch das arithemthische Mittel der Koeffizenten des angepassten Models eine Erwartungswert für das geschätzte Phi bilden.
#In unserem Fall erhalten wir ein erwarten wir für unser geschätztes Phi einen Wert von -0.4629583  

gridExtra::grid.arrange(ggAcf(y1),ggPacf(y1),
                        ggAcf(y2),ggPacf(y2),
                        ggAcf(y3),ggPacf(y3),
                        ggAcf(y4),ggPacf(y4))
  
(ar1 <- Arima(y1, order = c(1,0,0)))
(arma1 <- Arima(y1, order = c(1,0,1)))
(arma21 <- Arima(y1, order = c(2,0,1)))



#Fole 135 :  Die Entscheidungsregel ist: Wähle das Modell j mit dem niedrigsten AIC(j).
#Das ARMA(2,1) Modell weißt den niedireren AIC-Wert gegenüber des ARMA(1,1) Modell auf
#Wir wählen also das ARMA(2,1)-Modell

#Zunächst betrachten wir uns die Residuen und die quadrierten Residuen plotten lassen. Diese sollten nach white-noise aussehen

ggAcf(arma21$residuals^2) #Wir erwarten 1-2 signifikante Lags. Dies bestätigt sich

Die Residuen sollten außerdem standardnormalverteilt sein. Dies können wir über den ljung-box Test überprüfen

arma21
Box.test(arma21$residuals, lag = 100, type = 'Ljung-Box')
#Zum Niveau von alpha = 0.05 verwirft der Test nicht. Wir können also davon ausgehen, dass unsere Residuen white-noise sind.