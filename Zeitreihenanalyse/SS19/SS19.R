#Afg 1
#Überschussrendite
log_mean_pos <- sum(colMeans(Rendite)>0)
# Es ist 60 von 100 Managern gelungen durchschnittliche pos. log- Renditen zu erzielen
#Ich würde von diesem Ergebnis nicht darauf schließen, dass sie auch in Zukunft, pos. log- Renditen zu erzielen, da zukünftige Renditen unabhängig von vorherigen Beobachtungen sind.

#b
pval <- numeric()
for (i in 1 : ncol(Rendite)){
pval[i] <- t.test(Rendite[,i], mu = 0 , alternative = 'greater')$p.value
}
sum(pval<0.05)

#c
#Ich rate nicht dazu, da ein Problem durch multiples Teten besteht. Je mehr Test man durchführt umso wahrscheinlicher wird es, dass eine Hypothese fälschlicherweise abgelehnt wird. 
#Beim durchführen vieler Tests. steigt die Gefahr einen Fehler 1.Art zu machen (Alphafehler-Kumulierung)

#d irrelevant für die Klausur

#Aufgabe 2
#Die Zeitreihe scheint nicht stationär. Da es sich um einen Aktienindex handelt kann man auf einen stochaistischen Trend schließen. Ein (positiv) deterministischer Trend 
#wäre jedoch auch denkbar. Eindeutige Signale für Saisonalität sind jedoch nicht gegeben. 

y <- diff(log(Dow))
plot(y, type = 'l')
#Die log-Differenzen scheinen stationär. Die starken Ausreiser lassen jedoch Volatilitätscluster vermuten.

#b
#Aufgrund der Volatilitäscluster eigenen sich ARCH und GARCH Modelle
library(fGarch)
arch1 <- garchFit(y, formula =  ~ garch(1,0))
garch11 <- garchFit(y, formula = ~ garch(1,1))

summary(arch1)
summary(garch11)

#Da der AIC Wert von garch11 kleiner sind als von arch1 scheint das garch11 Modell besser geeignet zu sein.

#c)
r <- garch11@residuals /garch11@sigma.t
ggAcf(r^2)
ggPacf(r)

# Da wir nur 1-2 siginifikante Autokorrelationen zum 5%-Niveau erwarten können wir aus dem acf der quad. Residuen White Noise schließen. Der Ljung-Box Test lehnt ebenfalls immer ab

#d
#Der CVaR ist der Verlust, der basierend auf den vorhandnen Informationen nur mit einer Wahrscheinlichkeit von alpha * 100% überschritten wird. Mit einer Vorhersage für sigma_(T+1) 
#kann CVaR durch hat(CVaR_alpha) = hat(sigma_T+1 * phi^-1(alpha)) geschätzt werden. In R nutzen wir hierfür predict()

sigma <- predict(garch11,1)$standardDeviation
CVar <- sigma *qnorm(0.05)

