library(dplyr) # Para usar el operador %>%
library(ggplot2)
library(plyr)

setwd("/Users/david/Documents/Aprendizaje/BEDU_Data_scientist/Modulo_2/Proyecto")
embarques<- read.csv("Embarques.csv", header = T)
# No acepta acentos en los encabezados
View(embarques)
attach(embarques)
fecha_ped<- embarques[,2] # Columna de fecha de pedido
embarques["FECHA_DE_PEDIDO"]<- as.Date(fecha_ped)
fecha_fac<- embarques[,3] # Columna de fecha de factura
embarques["FECHA_DE_EMBARQUE"]<- as.Date(fecha_fac)
mes<- embarques[,23] # fecha del mes del embarque
embarques["MES"] <- as.Date(mes)
str(embarques)
data.frame(table(embarques$A_TIEMPO))
hogar<- subset(embarques, NEGOCIO == "HOGAR", select = -(TIPO_FLETE)) # filtra por negocio y elimina la columna TIPO_FLETE
View(hogar)

summary(hogar$DIAS_EMBARQUE)
data.frame(table(hogar$A_TIEMPO)) # identifica los pedidos entregados a tiempo (0,1)
hist(hogar$DIAS_EMBARQUE)
hist(hogar$DIAS_EMBARQUE, 
     main = "Dias de embarque - Histograma de frecuencias",
     xlab = "Dias",
     ylab = "Frecuencia")

hogar%>%
  ggplot() + 
  aes(hogar$DIAS_EMBARQUE) +
  geom_histogram(binwidth = 1, col="black", fill = "blue") + 
  ggtitle("Histograma de dias de entrega de pedidos") +
  ylab("Frecuencia") +
  xlab("Dias") + 
  theme_light()

hogar2<- subset(hogar, STATUS == "ENTREGADO ", select = -(COSIGNADO))
View(hogar2)
ggplot(hogar2, aes(x = RUTA, y = DIAS_EMBARQUE, fill = STATUS)) + geom_boxplot() +
  ggtitle("Boxplots") +
  xlab("Ruta de entrega") +
  ylab("Dias de entrega")
