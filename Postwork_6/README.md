Importamos el conjunto de datos match.data.csv a R:


```r
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-06/Postwork/match.data.csv")

head(df)
```

```
##         date home.team home.score   away.team away.score
## 1 2010-08-28  Hercules          0  Ath Bilbao          1
## 2 2010-08-28   Levante          1     Sevilla          4
## 3 2010-08-28    Malaga          1    Valencia          3
## 4 2010-08-29   Espanol          3      Getafe          1
## 5 2010-08-29 La Coruna          0    Zaragoza          0
## 6 2010-08-29  Mallorca          0 Real Madrid          0
```

```r
summary(df)
```

```
##      date            home.team           home.score      away.team        
##  Length:3800        Length:3800        Min.   : 0.000   Length:3800       
##  Class :character   Class :character   1st Qu.: 1.000   Class :character  
##  Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
##                                        Mean   : 1.589                     
##                                        3rd Qu.: 2.000                     
##                                        Max.   :10.000                     
##    away.score   
##  Min.   :0.000  
##  1st Qu.:0.000  
...
```

Agregamos una nueva columna "sumagoles" que contiene la suma de goles por partido.

## 📋 Columna "sumagoles"

```r
df["sumagoles"] <- df$home.score+df$away.score
```

## 📋 Promedio por mes 

Obtuvimos el promedio por mes de la columna suma de goles.
Revisamos el tipo de dato de la fecha


```r
class(df$date)
```

```
## [1] "character"
```

```r
df$date <- as.Date(df$date)
class(df$date)
```

```
## [1] "Date"
```

```r
df$fecha <- format(df$date, format = "%Y-%m")
golesxmes <- aggregate( df$sumagoles ~ df$fecha, df , mean)
View(golesxmes)
```

## 📋 Serie de Tiempo 

Creamos la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.


```r
golesxmes.ts <- ts(golesxmes[ ,2], start = c(2010,08), end = c(2019,12), frequency = 10)
golesxmes.ts
```

```
## Time Series:
## Start = c(2010, 8) 
## End = c(2020, 2) 
## Frequency = 10 
##  [1] 2.200000 2.425000 3.025641 2.902439 2.733333 3.000000 2.325000 2.400000
##  [9] 2.930233 2.957447 3.000000 2.525000 2.420000 2.833333 2.900000 2.550000
## [17] 3.050000 2.981818 2.854545 2.700000 3.000000 2.871795 2.838710 2.829268
## [25] 2.794872 3.025000 2.750000 2.657895 3.023810 2.725000 3.800000 2.920000
## [33] 2.711111 2.850000 3.166667 3.125000 2.902439 2.500000 2.474576 2.769231
## [41] 2.387097 2.400000 2.650000 2.903226 2.631579 2.400000 2.555556 2.780488
...
```

## 📊 Serie de Tiempo

```r
plot(golesxmes.ts, xlab = "Tiempo", ylab = "Promedio de goles", type = "o",
     pch = 12, col = "black", lwd = 2, cex = 2) + #Grafica de la serie de tiempo
title(main = "Serie de tiempo", sub = "Frecuencia = 10", cex.sub = 1,
      font.main =2) #modificaciones del titulo
```

<img src="https://github.com/omar17md/Equipo10/blob/main/GolesxMes.png?raw=true">


## 📋 Modelo Aditivo


```r
gxm.decom.A <- decompose(golesxmes.ts)
plot(gxm.decom.A, xlab = "Tiempo", 
     sub = "Descomposición de los datos de goles por mes")
```

<img src="https://raw.githubusercontent.com/omar17md/Equipo10/main/Modelo%20Aditivo.png">


```r
Tendencia <- gxm.decom.A$trend
Estacionalidad <- gxm.decom.A$seasonal
Aleatorio <- gxm.decom.A$random

plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2,
     sub = "Tendencia con efectos estacionales aditivos sobrepuestos")  +
     lines(Tendencia, lwd = 2, col = "blue") +
     lines(Tendencia + Estacionalidad, lwd = 2, col = "red", lty = 2)
```

<img src="https://raw.githubusercontent.com/omar17md/Equipo10/main/Tendencia%20estacionales.png">


```r
ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
        ylab = "Promedio de goles por mes", lty = 1:2, 
        col = c("blue", "red"), lwd = 2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")
```

<img src="https://raw.githubusercontent.com/omar17md/Equipo10/main/Tendencia%20estacionales%20sobrepuestos.png">


Comprobamos un punto de la suma de las componentes con la serie de tiempo

```r
(Tendencia[20] + Estacionalidad[20] + Aleatorio[20])
```

```
## [1] 2.7
```

```r
(golesxmes.ts[20])
```

```
## [1] 2.7
```

```r
plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2) +
     lines(Tendencia, lwd = 2, col = "blue") +
     lines(Tendencia + Estacionalidad, lwd = 2, col = "red", lty = 2)
```

<img src="https://github.com/omar17md/Equipo10/blob/main/Goles%20x%20mes%20sobrepuestos.png">


## 📋 Modelo Multiplicativo


```r
gxm.decom.M <- decompose(golesxmes.ts, type = "mult")

plot(gxm.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de goles por mes")
```

<img src="https://raw.githubusercontent.com/omar17md/Equipo10/main/modelo-multiplicativo.png">


```r
Trend <- gxm.decom.M$trend
Seasonal <- gxm.decom.M$seasonal
Random <- gxm.decom.M$random

plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2,
     sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos") +
     lines(Trend, lwd = 2, col = "blue") +
     lines(Trend * Seasonal, lwd = 2, col = "red", lty = 2)
```

<img src="https://raw.githubusercontent.com/omar17md/Equipo10/main/tendencia-estacionales.png">


```r
ts.plot(cbind(Trend, Trend * Seasonal), 
        xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
        ylab = "Promedio de goles por mes", lty = 1:2, 
        col = c("blue", "red"), lwd = 2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")
```

<img src="https://raw.githubusercontent.com/omar17md/Equipo10/main/tendencia-estacionales-2.png">


Comprobamos un punto de la multiplicación de las componentes con la serie de tiempo


```r
Trend[20] * Seasonal[20] * Random[20]
```

```
## [1] 2.7
```

```r
golesxmes.ts[20]
```

```
## [1] 2.7
```

Ambos modelos arrojan el mismo resultado por lo que sería indistinto elegir 
entre uno u otro

## 🏁 Conclusiones

Notemos que la frecuencia es 10 y no 12, ya que aunque la serie de tiempo se 
realiza tomando en cuenta los meses, las temporadas se juegan de agosto a mayo, 
por lo tanto como no se realizan juegos durante los meses de junio y julio
sólo se toman en cuenta los 10 meses donde se generan los goles
