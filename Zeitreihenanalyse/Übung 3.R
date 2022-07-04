#R-Übung 2
#Kausalität
a <- polyroot(c(1,0.2,-0.48))
b <-polyroot(c(1,1.99,0.88))
c <- polyroot(c(1,0.6))


abs(a) #Kausal da Betrag aller Nullstellen größer 1
abs(b) #Nicht kausal, da Betrag von 1. Nullstelle kleiner 1
abs(c) #Kausal da Betrag der Nullstelle größer 1

#Invertierbarkeit
b <-polyroot(c(1,0.2,0.7))
c <- polyroot(c(1,1.2))

#a  ist invertierbar per Definition, da 
abs(b) #Alle Nullstelen sind größer im Betrag deswegen invertierbar
abs(c) #Nicht invertierbar da Betrag einer Nullstelle kleiner 1

#2)
library(forecast)
Air57 <- window(AirPassengers, end = c(1957,12))
Air_58_60 <- window(AirPassengers, start = c(1958))

#a)
plot(Air57)

log_AP <- log(Air57)
plot(log_AP)

decomp <- decompose(log_AP)
autoplot(decomp)
log_AP_deseason <- log_AP - decomp$seasonal
autoplot(log_AP_deseason)

#WICHTIG FÜR TRENDBEREINIGUNG : Residuenmethode
log_AP_trend <- tslm(log_AP_deseason ~ trend)
log_AP_detrend <- residuals(log_AP_trend)
ggAcf(log_AP_detrend)
Pacf(log_AP_detrend)

(best_model <- auto.arima(log_AP_detrend, ic = 'aic', stationary = T, seasonal = F))

best_model %>% checkresiduals()

#Aufgabe 3
n <- c(200, 1000)

small_order <- list()
large_order <- list()

for (i in 1:100){
  
  small <- arima.sim(list(ar = 0.5, ma = 0.4), n = n[1] )
  large <- arima.sim(list(ar = 0.5, ma = 0.4), n = n[2] )
  
  m_small <- auto.arima(small, ic = 'bic', stationary = T, seasonal = F)
  m_large <- auto.arima(large, ic = 'bic', stationary = T, seasonal = F)

  p_small <- sum(m_small$model$phi != 0)
  q_small <- sum(m_small$model$theta!= 0)

  p_large <- sum(m_large$model$phi!= 0)
  q_large <- sum(m_large$model$theta!= 0)
  
  small_order[[i]] <- paste0('ARMA(', p_small, ',', q_small, ")" )
  
  large_order[[i]] <- paste0('ARMA (', p_large, ',', q_large, ")" )
}
table(unlist(small_order))

table(unlist(large_order))

m_small$model$phi == 0      
