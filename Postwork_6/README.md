# 📂 POSTWORK 6

Importa el conjunto de datos match.data.csv a R y realiza lo siguiente:

``` r
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-06/Postwork/match.data.csv")
```
Vemos la informacion que contiene match.data.csv

``` r
head(df)
summary(df)
```
     ##       date home.team home.score   away.team away.score
     ## 2010-08-28  Hercules          0  Ath Bilbao          1
     ## 2010-08-28   Levante          1     Sevilla          4
     ## 2010-08-28    Malaga          1    Valencia          3
     ## 2010-08-29   Espanol          3      Getafe          1
     ## 2010-08-29 La Coruna          0    Zaragoza          0
     ## 2010-08-29  Mallorca          0 Real Madrid          0
     ## ....


## 📋 Goles por mes

Agregamos una nueva columna sumagoles que contenga la suma de goles por partido.
```r
df["sumagoles"] <- df$home.score+df$away.score
```

Obtenems el promedio por mes de la suma de goles

Revisamos el tipo de dato de la fecha
```r
class(df$date)
```
Vemos que la columna date no es tipo de tipo Date
La convertimos a tipo Date

```r
df$date <- as.Date(df$date)
class(df$date)
```

Obtenemos el promedio de goles por mes

```r
df$fecha <- format(df$date, format = "%Y-%m")
golesxmes <- aggregate( df$sumagoles ~ df$fecha, df , mean)
```

```r
View(golesxmes)
```
     ##       date   home.team home.score   away.team away.score sumagoles   fecha
     ## 2010-08-28    Hercules          0  Ath Bilbao          1         1 2010-08
     ## 2010-08-28     Levante          1     Sevilla          4         5 2010-08
     ## 2010-08-28      Malaga          1    Valencia          3         4 2010-08
     ## 2010-08-29     Espanol          3      Getafe          1         4 2010-08
     ## 2010-08-29   La Coruna          0    Zaragoza          0         0 2010-08
     ## 2010-08-29    Mallorca          0 Real Madrid          0         0 2010-08
     ## 2010-08-29     Osasuna          0     Almeria          0         0 2010-08
     ## 2010-08-29   Santander          0   Barcelona          3         3 2010-08
     ## 2010-08-29    Sociedad          1  Villarreal          0         1 2010-08
     ## ...


## 📋 Serie de tiempo

Creamos la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.
```r
golesxmes.ts <- ts(golesxmes[ ,2], start = c(2010,08), end = c(2019,12), frequency = 12)
golesxmes.ts
```
     ##           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug      Sep      Oct      Nov      Dec
     ## 2010                                                                2.200000 2.425000 3.025641 2.902439 2.733333
     ## 2011 3.000000 2.325000 2.400000 2.930233 2.957447 3.000000 2.525000 2.420000 2.833333 2.900000 2.550000 3.050000
     ## 2012 2.981818 2.854545 2.700000 3.000000 2.871795 2.838710 2.829268 2.794872 3.025000 2.750000 2.657895 3.023810
     ## 2013 2.725000 3.800000 2.920000 2.711111 2.850000 3.166667 3.125000 2.902439 2.500000 2.474576 2.769231 2.387097
     ## 2014 2.400000 2.650000 2.903226 2.631579 2.400000 2.555556 2.780488 2.200000 2.633333 3.225000 1.750000 2.650000
     ## 2015 3.055556 2.441176 2.435897 3.204082 2.714286 3.025000 2.727273 2.880000 2.850000 2.878049 3.384615 2.600000
     ## 2016 2.862069 2.675000 2.880952 2.948718 2.985294 3.250000 2.300000 2.800000 2.914286 3.033333 2.128205 3.200000
     ## 2017 2.604167 2.513514 2.296296 3.312500 2.260870 2.644444 2.375000 2.709677 2.526316 2.756098 2.300000 2.725000
     ## 2018 2.550000 2.966667 2.269231 2.590909 2.564103 2.742857 2.777778 2.133333 2.590909 2.375000 2.415094 2.263158
     ## 2019 2.200000 2.425000 3.025641 2.902439 2.733333 3.000000 2.325000 2.400000 2.930233 2.957447 3.000000 2.525000


## 📊 Graficamos la serie de tiempo 
```r
plot(golesxmes.ts, xlab = "Tiempo", ylab = "Goles por mes", main = "Serie de Goles por Mes")
```
<img src="https://github.com/omar17md/Equipo10/blob/main/GolesxMes.png?raw=true">

## 📋 Modelo aditivio
Descomponemos la serie de tiempo
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
```

```r
plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2,
     sub = "Tendencia con efectos estacionales aditivos sobrepuestos")
lines(Tendencia, lwd = 2, col = "blue")
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

```r
Tendencia[20] + Estacionalidad[20] + Aleatorio[20]
golesxmes.ts[20]
```
Ambos dan el mismo resultado
     
     ## 2.7
     ## 2.7

```r
plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2)
lines(Tendencia, lwd = 2, col = "blue")
lines(Tendencia + Estacionalidad, lwd = 2, col = "red", lty = 2)
```
<img src="https://github.com/omar17md/Equipo10/blob/main/Goles%20x%20mes%20sobrepuestos.png">

Modelo multiplicativo

```r
gxm.decom.M <- decompose(golesxmes.ts, type = "mult")
```

```r
plot(gxm.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de goles por mes")
```
<img src="https://raw.githubusercontent.com/omar17md/Equipo10/main/modelo-multiplicativo.png">

```r
Trend <- gxm.decom.M$trend
Seasonal <- gxm.decom.M$seasonal
Random <- gxm.decom.M$random
```

```r
plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2,
     sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")
lines(Trend, lwd = 2, col = "blue")
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

```r
Trend[100]*Seasonal[100]*Random[100]
golesxmes.ts[100]
```
     ## 2.415094
     ## 2.415094

Ambos modelos arrojan el mismo resultado por lo que sería indistinto elegir entre uno u otro
