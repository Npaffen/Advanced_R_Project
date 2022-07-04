library(forecast)
library(tseries)

ar_1 <- Arima(consumption, order = c(1,0,1))
ma_1 <- Arima(consumption, order = c(0,0,1))

ar_1_toma <- ARMAtoMA(ar = ar_1$coef[2], lag.max = 50)
plot(ar_1_toma, type = 'l')


ma_1_toma <- ARMAtoMA(ma = ma_1$coef[1], lag.max = 50)
plot(ma_1_toma, type = 'l')

#Die Koeffizienten der MA(inf)-Darstellung Y_tt = mu + sum(psi_j*eta_{t-j}) als Funktion von j zusammengefasst bilden die IA. Da für einen MA(1) Prozess für j > 1 alle psi_j = 0,
#muss die IA einen Cutoff nach j = 1 haben. Fü reinen AR(1)  Prozes gilt hingegen psi_j = phi^j. Dementsprechend klingt die IA geometrisch ab, was auch an der Grafik zu sehen ist.


#Afg 2 d)
bird_ar <- numeric()
bird_arma <- numeric()

for (i in 1:10){

bird_ar[i] <- window(birds, start = 1900 + i , end = 1999 + i) %>% Arima(order = c(1,0,0)) %>% forecast( h = 1) %>% .$mean



bird_arma[i] <- window(birds, start = 1900 + i , end = 1999 + i) %>% Arima(order = c(1,0,1)) %>% forecast(  h = 1) %>% .$mean



}

(msfe1 <- mean((bird_ar - window(birds, start = 2001))^2))
(msfe2 <- mean((bird_arma - window(birds, start = 2001))^2))


# Afg 3

ar_1 <- numeric()
for (i in 1:100){
ar_1[i] <- arima.sim(n = 10, model = list(ar = c(0.8))) %>% ar.ols(order.max = 1, aic = F) %>% .$ar
}

mean(ar_1)

#Afg 4g
gridExtra::grid.arrange(
ggAcf(y1),ggPacf(y1),
ggAcf(y2),ggPacf(y2),
ggAcf(y3),ggPacf(y3),
ggAcf(y4),ggPacf(y4),
nrow = 4)

#y1 Da die Acf abnimmt und die PACF Funktion ebenfalls einen abnhemnen Verlauf zeigt, scheint hier ein ARMA(p,q) Modell geeignet zu sein
#y2 Da die Acf abnimmt und die Pacf direkt nach dem ersten Lag abschneidet weist dies auf ein AR(1)-Modell hin
#y3 Das abschneiden der Acf nach dem ersten Lag weißt eindeutig auf ein MA(1) - Modell hin
#y4 white noise, da die Daten keine Autokorrelation aufweisen

#Ar(1) und MA(1) danach Impuls-antwort

ar_1 <- Arima(consumption, order  = c(1,0,0))
ma_1 <- Arima(consumption, order = c(0,0,1))

plot(ARMAtoMA(ar = ar_1$coef[1], lag.max = 20), type = 'l')
plot(ARMAtoMA(ma = ma_1$coef[1], lag.max = 20), type = 'l')

#MA(infinty) - Darstellung ist gegeben durch Yt = mu + \sum psi_j * eta_t-j. Da für einen MA(1)-Prozess für j>1 psi_j = 0 gilt muss die Funktion einen Cutoff nach j = 1 haben.
#Für einen AR(1)-Prozess gilt jedoch psi_j = phi^j. Dementsprechend klingt die IA geometrisch ab.

sum(colSums(Rendite)>0)
#Es war 60 Managern möglich eine positive log-Rendite zu erwirtschaften. Da Renditen zufällig verteilt sind, kann man aus den Ergebnissen nicht schließen,
#dass die Manager auch in Zukunft eine positive Log-Rendite erzielen.
pval <- numeric()
for( i in 1:ncol(Rendite)){
  pval[i] <- t.test(Rendite[,i], mu = 0 , alternative = 'greater')$p.value  
}
sum(pval<0.05)
# 7 Manager haben signifikant positive log-Überschussrenditen erwirtschaftet

#c) Die Anleger sollten dem Manager ihr Geld nicht anvertrauen, da die Ergebnisse wie beriets in a) erklärt zufällig sind und nur für diese Monate signifikant sind.
# Würde man die Zeitreihe erweitern, würde sehr wahrscheinlich langfristig keine Signifikanz nachweisbar sein. Das Problem des multiplen Testens welches hier
#angesprochen wird besteht stochastisch daran, dass es wahrscheinlicher wird umso häufiger man testet irgendwann die Nullhypothese fälschlicherweise abgelehnt wird.  

library(fGarch)

#2a Die log-Differenzens scheinen nicht stationär, da ein deterministischer Trend zu erkennen ist. Die log-Differenzen scheinen stationär weisen jedoch Volatitlitätcluster auf.
y <- diff(log(Dow))
plot(y)

arch_1 <- garchFit(formula = ~garch(1,0), y, trace = F )
garch_11 <- garchFit(formula = ~garch(1,1), y, trace = F )

summary(arch_1)
summary(garch_11)
#Wir würden uns für das Modell mit dem kleineren AIC entscheiden also Das Garch(1,1)-Modell

#Um zu überprüfen wie gut unser Fit ist, schauen wir uns die standardisierten Risiduen an 
resid <- garch_11@residuals/garch_11@sigma.t
acf(resid^2)
#Für ein gutes Modell erwarten wir Reisuden die nach white Noise aussehen. Hierbei erwarten wir 1-2 signifkante Lags der quadrierten Residuen im ACF-Plot. Dies bestätigt sich.
#Außerdem lehnt der Ljung-Box-Test alle Nullhypothesen zum Niveau alpha = 0.05 ab.


