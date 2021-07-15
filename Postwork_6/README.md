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

Creamos la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.
```r
golesxmes.ts <- ts(golesxmes[ ,2], start = c(2010,08), end = c(2019,12), frequency = 12)
golesxmes.ts
```

Modelo aditivio
```r
gxm.decom.A <- decompose(golesxmes.ts)
plot(gxm.decom.A, xlab = "Tiempo", 
     sub = "Descomposición de los datos de goles por mes")
```
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
```r
ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
        ylab = "Promedio de goles por mes", lty = 1:2, 
        col = c("blue", "red"), lwd = 2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")
```

```r
Tendencia[20] + Estacionalidad[20] + Aleatorio[20]
golesxmes.ts[20]
```

```r
plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2)
lines(Tendencia, lwd = 2, col = "blue")
lines(Tendencia + Estacionalidad, lwd = 2, col = "red", lty = 2)
```


Modelo multiplicativo

```r
gxm.decom.M <- decompose(golesxmes.ts, type = "mult")
```

```r
plot(gxm.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de goles por mes")
```

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
```r

```r
ts.plot(cbind(Trend, Trend * Seasonal), 
        xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
        ylab = "Promedio de goles por mes", lty = 1:2, 
        col = c("blue", "red"), lwd = 2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")
```

```r
Trend[100]*Seasonal[100]*Random[100]
golesxmes.ts[100]
```

Ambos modelos arrojan el mismo resultado por lo que sería indistinto elegir 
entre uno u otro
