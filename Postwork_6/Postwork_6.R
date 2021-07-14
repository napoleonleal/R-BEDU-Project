#Postwork 6 

#Importa el conjunto de datos match.data.csv a R y realiza lo siguiente:
setwd("C:/Users/adria/OneDrive/Bedu - Data Science/Programación y estadística en R/postwork")
df <- read.csv("match.data.csv")

head(df)
summary(df)

#1. Agrega una nueva columna sumagoles que contenga la suma de goles por partido.
df["sumagoles"] <- df$home.score+df$away.score

#2. Obtén el promedio por mes de la suma de goles.
#Revisamos el tipo de dato de la fecha
class(df$date)
df$date <- as.Date(df$date)
class(df$date)

df$fecha <- format(df$date, format = "%Y-%m")
golesxmes <- aggregate( df$sumagoles ~ df$fecha, df , mean)
View(golesxmes)

#3. Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.
golesxmes.ts <- ts(golesxmes[ ,2], start = c(2010,08), end = c(2019,12), frequency = 12)
golesxmes.ts

#Extra
#Modelo aditivio
gxm.decom.A <- decompose(golesxmes.ts)
plot(gxm.decom.A, xlab = "Tiempo", 
     sub = "Descomposición de los datos de goles por mes")

Tendencia <- gxm.decom.A$trend
Estacionalidad <- gxm.decom.A$seasonal
Aleatorio <- gxm.decom.A$random

plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2,
     sub = "Tendencia con efectos estacionales aditivos sobrepuestos")
lines(Tendencia, lwd = 2, col = "blue")
lines(Tendencia + Estacionalidad, lwd = 2, col = "red", lty = 2)

ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
        ylab = "Promedio de goles por mes", lty = 1:2, 
        col = c("blue", "red"), lwd = 2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")

Tendencia[20] + Estacionalidad[20] + Aleatorio[20]
golesxmes.ts[20]


plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2)
lines(Tendencia, lwd = 2, col = "blue")
lines(Tendencia + Estacionalidad, lwd = 2, col = "red", lty = 2)


#Modelo multiplicativo
gxm.decom.M <- decompose(golesxmes.ts, type = "mult")

plot(gxm.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de goles por mes")

Trend <- gxm.decom.M$trend
Seasonal <- gxm.decom.M$seasonal
Random <- gxm.decom.M$random

plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2,
     sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")
lines(Trend, lwd = 2, col = "blue")
lines(Trend * Seasonal, lwd = 2, col = "red", lty = 2)

ts.plot(cbind(Trend, Trend * Seasonal), 
        xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
        ylab = "Promedio de goles por mes", lty = 1:2, 
        col = c("blue", "red"), lwd = 2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

Trend[100]*Seasonal[100]*Random[100]
golesxmes.ts[100]

#Ambos modelos arrojan el mismo resultado por lo que sería indistinto elegir 
#entre uno u otro
