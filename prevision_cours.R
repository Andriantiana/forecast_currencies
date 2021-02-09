#Définition du répertoire de travail
setwd("D:/sipromad/cours_change")
library(tseries)
library(forecast)
library(rugarch)
set.seed(2021)

#Importation des données de la BFM
euro <- read.csv("euro.csv", sep = ";", header = TRUE) 
dollar <- read.csv("dollar.csv", sep = ";", header = TRUE)
#vérification des données
str(euro)
str(dollar)

#Remplacement des virgules par des points deans les données
euro$COURS <- as.numeric(gsub(",", ".", gsub(" ", "", euro$COURS)))
dollar$COURS <- as.numeric(gsub(",", ".", gsub(" ", "", dollar$COURS)))
#Certaines dates sont dans dollar mais pas dans euro
setdiff(dollar$DATE, euro$DATE)
setdiff(euro$DATE, dollar$DATE)

#Fusionner les deux tables
cours <- dollar[ , c("DATE", "COURS")]
colnames(cours)[2] <- "dollar"
cours <- merge(cours, euro[ , c("DATE", "COURS")], by = "DATE", all = TRUE)
colnames(cours)[3] <- "euro"

#Transformer les dates en POSIX
cours$DATE <- strptime(cours$DATE, format = "%d/%m/%Y", tz = "")
cours <- cours[order(cours$DATE), ]

#regarder un premier graphique
plot(cours$DATE, cours$dollar, type = "l", col = "red", ylab = "cours en ariary", xlab = "date", main = "Variation des cours euro et dollar", ylim = c(1500, 5000))
lines(cours$DATE, cours$euro, col = "blue")
legend("topleft", legend = colnames(cours)[c(2,3)], col = c("red", "blue"), lty = 1)

#Transformation en séries temporelles
cours_dollar <- ts(cours$dollar, name = cours$DATE, frequency = 5)
cours_dollar <- tsclean(cours_dollar, replace.missing = TRUE)
cours_euro <- ts(cours$euro, name = cours$DATE, frequency = 5)
cours_euro <- tsclean(cours_euro, replace.missing = TRUE)

####Visuellement les deux séries présentent une tendance donc sont non stationnaires
#Test augmenté de Dickey-Fuller pour la stationarité
print("DOLLAR ADF.TEST")
adf.test(cours_dollar, alternative = "stationary")
print("EURO ADF.TEST")
adf.test(cours_euro, alternative = "stationary")


#Différentiation des séries
d1_dollar <- diff(cours_dollar, differences = 1)
print("DOLLAR ADF.TEST DIFF1")
adf.test(d1_dollar, alternative = "stationary")

d1_euro <- diff(cours_euro, differences = 1)
print("EURO ADF.TEST DIFF 1")
adf.test(d1_euro, alternative = "stationary")

#ACF et PACF
acf(d1_dollar, main = "ACF 1 differenced dollar")
pacf(d1_dollar, main = "PACF 1 differenced dollar")

acf(d1_euro, main = "ACF 1 differenced euro")
pacf(d1_euro, main = "PACF 1 differenced euro")

#Modelisation ARIMA
arima_fit_dollar <- auto.arima(cours_dollar, seasonal = FALSE)
tsdisplay(residuals(arima_fit_dollar), lag.max = 45, main = "(2,1,1) model dollar")

arima_fit_euro <- auto.arima(cours_euro, seasonal = FALSE)
tsdisplay(residuals(arima_fit_euro), lag.max = 45, main = "(3,1,1) model euro")

#Les résultats suggèrent une modélisation GARCH
ar_comp_dollar <- arimaorder(arima_fit_dollar)[1]
ma_comp_dollar <- arimaorder(arima_fit_dollar)[3]
ar_comp_euro <- arimaorder(arima_fit_euro)[1]
ma_comp_euro <- arimaorder(arima_fit_euro)[3]

#Préparation des prévisions de cours en ajoutant des dates dans le tableau "cours"
next_days <- seq.Date(max(as.Date(cours$DATE)), length = 36, by = "days")
next_days <- data.frame(next_days)
colnames(next_days)[1] <- "DATE"
next_days$jour <- weekdays(next_days$DATE)
next_days <- subset(next_days, jour != "samedi" & jour != "dimanche" & DATE > max(as.Date(cours$DATE)),  c("DATE"))
next_days$dollar <- NA
next_days$euro <- NA
next_days$DATE <- strptime(next_days$DATE, format = "%Y-%m-%d", tz = "")
cours <- rbind(cours, next_days)

rm(next_days)

#Dollar
garch_dollar <- ugarchspec(mean.model = list(armaOrder = c(ar_comp_dollar, ma_comp_dollar), arfima = TRUE), fixed.pars = list(arfima = 1), variance.model = list(c(1,1)), distribution.model = "std")
garch_dollar_fit <- ugarchfit(data = cours_dollar, spec = garch_dollar, out.sample = 5, solver = "hybrid")
model_dollar <- ugarchforecast(garch_dollar_fit, data = NULL, n.ahead = 5, n.roll = 5, out.sample = 5)

sup_forecast_dollar <- vector()
inf_forecast_dollar <- vector()
forecast_dollar <- vector()
i <- 1
while(i < 6){
sup_forecast_dollar <- c(sup_forecast_dollar, model_dollar@forecast$seriesFor[i, ] + model_dollar@forecast$sigmaFor[i, ])
inf_forecast_dollar <- c(inf_forecast_dollar, model_dollar@forecast$seriesFor[i, ] - model_dollar@forecast$sigmaFor[i, ])
forecast_dollar <- c(forecast_dollar, model_dollar@forecast$seriesFor[i, ])
i <- i + 1
}
cours$forecast_dollar <- NA
index_inf <- length(cours$forecast_dollar)-29
index_sup <- length(cours$forecast_dollar)
cours$forecast_dollar[index_inf:index_sup] <- forecast_dollar 
cours$sup_forecast_dollar <- NA
cours$sup_forecast_dollar[index_inf:index_sup] <- sup_forecast_dollar
cours$inf_forecast_dollar <- NA
cours$inf_forecast_dollar[index_inf:index_sup] <- inf_forecast_dollar
plot(cours$DATE[cours$DATE >= "2021-01-01"], cours$dollar[cours$DATE >= "2021-01-01"], type = "l", col = "red", main = "Cours dollar et prévisions", xlab = "DATE",  ylab = "ARIARY", ylim = c(3000, 5000))
lines(cours$DATE, cours$forecast_dollar, col = "green")
lines(cours$DATE, cours$sup_forecast_dollar, col = "green", lty = "dotted")
lines(cours$DATE, cours$inf_forecast_dollar, col = "green", lty = "dotted")

#Euro
garch_euro <- ugarchspec(mean.model = list(armaOrder = c(ar_comp_euro, ma_comp_euro), arfima = TRUE), fixed.pars = list(arfima = 1), variance.model = list(c(1,1)), distribution.model = "std")
garch_euro_fit <- ugarchfit(data = cours_euro, spec = garch_euro, out.sample = 5, solver = "hybrid")
model_euro <- ugarchforecast(garch_euro_fit, data = NULL, n.ahead = 5, n.roll = 5, out.sample = 5)

sup_forecast_euro <- vector()
inf_forecast_euro <- vector()
forecast_euro <- vector()
i <- 1
while(i < 6){
  sup_forecast_euro <- c(sup_forecast_euro, model_euro@forecast$seriesFor[i, ] + model_euro@forecast$sigmaFor[i, ])
  inf_forecast_euro <- c(inf_forecast_euro, model_euro@forecast$seriesFor[i, ] - model_euro@forecast$sigmaFor[i, ])
  forecast_euro <- c(forecast_euro, model_euro@forecast$seriesFor[i, ])
  i <- i + 1
}
cours$forecast_euro <- NA
index_inf <- length(cours$forecast_euro)-29
index_sup <- length(cours$forecast_euro)
cours$forecast_euro[index_inf:index_sup] <- forecast_euro 
cours$sup_forecast_euro <- NA
cours$sup_forecast_euro[index_inf:index_sup] <- sup_forecast_euro
cours$inf_forecast_euro <- NA
cours$inf_forecast_euro[index_inf:index_sup] <- inf_forecast_euro
plot(cours$DATE[cours$DATE >= "2021-01-01"], cours$euro[cours$DATE >= "2021-01-01"], type = "l", col = "red", main = "Cours euro et prévisions", xlab = "DATE",  ylab = "ARIARY", ylim = c(3000, 5000))
lines(cours$DATE, cours$forecast_euro, col = "black")
lines(cours$DATE, cours$sup_forecast_euro, col = "black", lty = "dotted")
lines(cours$DATE, cours$inf_forecast_euro, col = "black", lty = "dotted")