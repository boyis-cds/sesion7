library(forecast)
library(lubridate)
library(fpp2)
library(dplyr)
library(tidyr)

setwd("/Users/david/Documents/Aprendizaje/BEDU_Data_scientist/Modulo_2/Proyecto")
venta<- read.csv("ventas2.csv", 
                 header = T, sep = ",")
View(venta)

attach(venta)
fecha <- venta[, 7] # La col fecha
venta['Fecha'] <- as.Date(fecha) # sobre escribir nueva 
### venta$Fecha<- ymd (venta$Fecha) usando libridate
class(venta)
str(venta)
venta_mes<- aggregate(Ventas ~ Fecha, data = venta, 
                         FUN = sum) # suma las ventas por fecha y por importe
View(venta_mes)
ts<- ts(venta_mes$Ventas, start= c(2009,11,1), end= c(2012,10,1), frequency = 12)
autoplot(ts) + ggtitle("Ventas por mes") + ylab("Ventas") + xlab("Fecha")
tsoutliers(ts) # reemplaza los datos atipicos
tsclean(ts) # interpola los datos atipicos

autoplot(tsclean(ts), series="clean", color='red', lwd=0.9) +
   autolayer(ts, series="original", color='gray', lwd=1) +
   geom_point(data = tsoutliers(ts) %>% as.data.frame(), 
              aes(x=index, y=replacements), col='blue') +
   labs(x = "Mes", y = "Venta papas")

componentes.ts = decompose(ts)
plot(componentes.ts)

mod1<- HoltWinters(ts, seasonal = "additive")
plot(mod1)
fcst1<- forecast(mod1,h=6)
accuracy(fcst1)
plot(fcst1)
data.frame(fcst1)

mod2<- HoltWinters(ts, seasonal = "multiplicative")
plot(mod2)
fcst2<- forecast(mod2,h=6)
accuracy(fcst2)
plot(fcst1)
data.frame(fcst1)

arima<- auto.arima(ts)
print(summary(arima))
checkresiduals(arima)
plot(forecast(arima,h=6))
data.frame(forecast(arima,h=6))

### ============================

plot(diff(ts),  xlab = "", ylab = "") #quita la tendencia
title(main = "Serie Diferenciada de Ventas de Papas",
      xlab = "Tiempo", ylab = "Dif Serie") 

ts_se<- diff(ts) # serie de tiempo sin estacionalidad

plot(ts_se, xlab = "", ylab = "") # quita la varianza y tendencia
title(main = "Serie sin estacionalidad",
      xlab = "Tiempo", ylab = "Dif log-Serie")
ventas.AR<- arima(ts_se)
print(summary(ventas.AR))
checkresiduals(ventas.AR)
plot(forecast(ventas.AR,h=6))
data.frame(forecast(ventas.AR,h=6))

pr<- predict(ventas.AR,6)$pred
exp(pr)


