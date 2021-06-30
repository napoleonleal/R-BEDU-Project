
<h1><a href="https://itzamango.github.io/postwork-equipo-10/" target="_blank">PARA VER LA VERSIÓN EN HTML RESPONSIVA DEL SCRIPT DE R CON GRÁFICAS INTERACTIVAS HAZ CLICK EN ESTA ORACIÓN</a></h1>

<img src="https://github.com/itzamango/postwork-equipo-10/blob/eb0c755ba8810634f7e2cb7eb2298b66d3989ddb/img/bedulogo.png?raw=true">

# POSTWORK1-4 EQUIPO 10

## BEDU SANTANDER UNIVERSIDADES

## Integrantes

-   María Magdalena Castro Sam

-   Sergio Napoleón Leal

-   Jesús Omar Magaña Medina

-   Fernando Itzama Novales Campos

-   Adrián Ramírez Cortés

-   Efraín Soto Olmos

### Descripción

Éste código analiza algunos datos de la primera división de la liga
española, obtenidos de <https://www.football-data.co.uk/spainm.php>. Más
notas sobre los datos pueden encontrarse en
<https://www.football-data.co.uk/notes.txt>.





# POSTWORK 1

Importamos datos de la primera división 2019-2020:


```r
df <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
```

Extraemos la columna para los goles locales:


```r
goles.local <- table(df$FTHG)
```

La probabilidad marginal es la probabilidad de que ocurra un evento
simple, por lo tanto estará dada por el número de goles sobre el total
de los partidos.

Calcularemos la probabilidad de que el equipo que juegue en casa anote
'*x'* goles:

Con table generamos una tabla que nos indica la frecuencia de los goles:


```r
?table
```

## Probabilidad Marginal Equipo Local


```r
(total_partidos = length(df$FTHG)) #total de los partidos
```

```
## [1] 380
```

```r
prob.marginal = goles.local / total_partidos #probabilidad marginal 
(round(prob.marginal, 3))
```

```
## 
##     0     1     2     3     4     5     6 
## 0.232 0.347 0.261 0.100 0.037 0.021 0.003
```

```r
table.local = data.frame(goles.local, 
                          round(prob.marginal*100, 2), 
                          check.names = T)[-3]
colnames(table.local) = c("Goles", "Frecuencia", "P. Marginal")
table.local #tabla final con las probabilidades para el equipo local
```

```
##   Goles Frecuencia P. Marginal
## 1     0         88       23.16
## 2     1        132       34.74
## 3     2         99       26.05
## 4     3         38       10.00
## 5     4         14        3.68
## 6     5          8        2.11
## 7     6          1        0.26
```

## Probabilidad Marginal Equipo Visitante

En el caso del equipo vistante el procedimiento es análogo.


```r
goles.visitante <-table(df$FTAG) #total de los partidos
prob.marginal = goles.visitante / total_partidos #probabilidad marginal
(round(prob.marginal,3))
```

```
## 
##     0     1     2     3     4     5 
## 0.358 0.353 0.213 0.047 0.024 0.005
```

```r
table.visitante = data.frame(goles.visitante, 
                             round(prob.marginal*100, 2), 
                             check.names = T)[-3]
colnames(table.visitante) = c("Goles", "Frecuencia", "P. Marginal")
table.visitante #tabla final de probabilidades para el equipo visitante
```

```
##   Goles Frecuencia P. Marginal
## 1     0        136       35.79
## 2     1        134       35.26
## 3     2         81       21.32
## 4     3         18        4.74
## 5     4          9        2.37
## 6     5          2        0.53
```

## Probabilidad Conjunta

La probabilidad conjunta toma en cuenta la probabilidad de dos eventos
sobre el total de resultados posibles.

Calcularemos la probabilidad conjunta de que el equipo local anote '*x'*
goles y el visitante '*y'* goles:


```r
goles.partidos = table(df$FTHG, df$FTAG, dnn=c("x", "y"))
prob.conjunta = prop.table(goles.partidos)
(prob.conjunta <- round(prob.conjunta * 100,2))
```

```
##    y
## x       0     1     2     3     4     5
##   0  8.68  7.37  3.95  2.11  0.53  0.53
##   1 11.32 12.89  8.42  1.32  0.79  0.00
##   2 10.26  9.21  5.26  0.79  0.53  0.00
##   3  3.68  3.68  1.84  0.53  0.26  0.00
##   4  1.05  1.32  1.05  0.00  0.26  0.00
##   5  0.53  0.79  0.79  0.00  0.00  0.00
##   6  0.26  0.00  0.00  0.00  0.00  0.00
```

# POSTWORK 2


```r
library(dplyr)
```

Ahora agregamos aún más datos. Utilizaremos los datos de las temporadas
2017/2018, 2018/2019 y 2019/2020.

Descargamos los datos:


```r
setwd(getwd())
liga1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
liga1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
liga1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
download.file(url = liga1718, destfile = "liga1718.csv", mode = "wb")
download.file(url = liga1819, destfile = "liga1819.csv", mode = "wb")
download.file(url = liga1920, destfile = "liga1920.csv", mode = "wb")
temporadas <- lapply(dir(pattern="csv$"), read.csv)  #guardamos los archivos en una lista
```

Revisamos que tipo de objeto son:


```r
str(temporadas); head(temporadas); View(temporadas); summary(temporadas)
```

```
## List of 3
##  $ :'data.frame':	380 obs. of  64 variables:
##   ..$ Div       : chr [1:380] "SP1" "SP1" "SP1" "SP1" ...
##   ..$ Date      : chr [1:380] "18/08/17" "18/08/17" "19/08/17" "19/08/17" ...
##   ..$ HomeTeam  : chr [1:380] "Leganes" "Valencia" "Celta" "Girona" ...
##   ..$ AwayTeam  : chr [1:380] "Alaves" "Las Palmas" "Sociedad" "Ath Madrid" ...
##   ..$ FTHG      : int [1:380] 1 1 2 2 1 0 2 0 1 0 ...
##   ..$ FTAG      : int [1:380] 0 0 3 2 1 0 0 3 0 1 ...
##   ..$ FTR       : chr [1:380] "H" "H" "A" "D" ...
##   ..$ HTHG      : int [1:380] 1 1 1 2 1 0 2 0 0 0 ...
...
```

```
## [[1]]
##     Div     Date    HomeTeam    AwayTeam FTHG FTAG FTR HTHG HTAG HTR HS AS HST
## 1   SP1 18/08/17     Leganes      Alaves    1    0   H    1    0   H 16  6   9
## 2   SP1 18/08/17    Valencia  Las Palmas    1    0   H    1    0   H 22  5   6
## 3   SP1 19/08/17       Celta    Sociedad    2    3   A    1    1   D 16 13   5
## 4   SP1 19/08/17      Girona  Ath Madrid    2    2   D    2    0   H 13  9   6
## 5   SP1 19/08/17     Sevilla     Espanol    1    1   D    1    1   D  9  9   4
## 6   SP1 20/08/17  Ath Bilbao      Getafe    0    0   D    0    0   D 12  8   2
## 7   SP1 20/08/17   Barcelona       Betis    2    0   H    2    0   H 15  3   2
## 8   SP1 20/08/17   La Coruna Real Madrid    0    3   A    0    2   A 12 16   6
...
```

```
##      Length Class      Mode
## [1,]  64    data.frame list
## [2,]  61    data.frame list
## [3,] 105    data.frame list
```

Seleccionamos sólo algunas columnas de interés:


```r
temporadas <- lapply(temporadas, select, c("Date", "HomeTeam", "AwayTeam","FTHG","FTAG","FTR")) 
```

## Data Frame completo
Revisamos que las columnas sean del mismo tipo, corregimos el tipo de
dato de la columna `Date` y unimos en un solo data frame:


```r
data <- do.call(rbind, temporadas)

data <- mutate(data, Date = as.Date(Date, "%d/%m/%y"))

data #data frame final solo con los datos elegidos
```

```
##            Date    HomeTeam    AwayTeam FTHG FTAG FTR
## 1    2017-08-18     Leganes      Alaves    1    0   H
## 2    2017-08-18    Valencia  Las Palmas    1    0   H
## 3    2017-08-19       Celta    Sociedad    2    3   A
## 4    2017-08-19      Girona  Ath Madrid    2    2   D
## 5    2017-08-19     Sevilla     Espanol    1    1   D
## 6    2017-08-20  Ath Bilbao      Getafe    0    0   D
## 7    2017-08-20   Barcelona       Betis    2    0   H
## 8    2017-08-20   La Coruna Real Madrid    0    3   A
## 9    2017-08-21     Levante  Villarreal    1    0   H
...
```

```r
dim(data)
```

```
## [1] 1140    6
```


```r
write.csv(data, file = 'Postwork_02.csv') #guardamos el df en un archivo csv
```

# POSTWORK 3


```r
library(ggplot2)
library(plotly)
```

Con el data frame obtenido realizaremos algunas gráficas.

Calcularemos la probabilidad marginal de que el equipo local anote '*x'*
goles:


```r
goles.local <- table(data$FTHG) #goles locales
total_partidos <- length(data$FTHG) #total de partidos
pm.local <-(goles.local/total_partidos)#probalidad marginal 
goles.local <- data.frame(goles.local, 
                          round(pm.local*100, 2))[-3]
colnames(goles.local)=c("Goles","Frecuencia", "Prob.Marginal")
goles.local #probabilidad final
```

```
##   Goles Frecuencia Prob.Marginal
## 1     0        265         23.25
## 2     1        373         32.72
## 3     2        304         26.67
## 4     3        128         11.23
## 5     4         40          3.51
## 6     5         22          1.93
## 7     6          6          0.53
## 8     7          1          0.09
## 9     8          1          0.09
```

Realizamos una gráfica para vizualizar los datos:



## Figura 3.1 P(x) Marginal Equipo Local meta Gol


```r
ggplotly(e.local) #versión interactiva
```

<img src="https://github.com/itzamango/postwork-equipo-10/blob/main/img/1.png?raw=true">



Ahora calcularemos la probabilidad para el equipo visitante:


```r
goles.visitante <- table(data$FTAG)
pm.vis <- (goles.visitante/total_partidos)
goles.visitante <- data.frame(goles.visitante,
                              round(pm.vis*100, 2))[-3]
colnames(goles.visitante)=c("Goles","Freq", "Prob.Marginal")
goles.visitante #probabilidad final
```

```
##   Goles Freq Prob.Marginal
## 1     0  401         35.18
## 2     1  388         34.04
## 3     2  242         21.23
## 4     3   62          5.44
## 5     4   33          2.89
## 6     5   11          0.96
## 7     6    3          0.26
```

Realizamos una gráfica para vizualizar los datos



## Figura 3.2 P(y) Marginal Equipo Visitante meta Gol


```r
ggplotly(e.visitante) #versión interactiva
```


<img src="https://github.com/itzamango/postwork-equipo-10/blob/main/img/2.png?raw=true">


La probabilidad conjunta de que el equipo que juega en casa anote *'x'*
goles y el equipo que juega como visitante anote '*y'* goles:


```r
goles.partidos = table(data$FTHG, data$FTAG, dnn=c("x", "y"))
prob.conjunta = prop.table(goles.partidos)
(prob.conjunta <- round(prob.conjunta * 100,2))
```

```
##    y
## x       0     1     2     3     4     5     6
##   0  7.81  8.07  4.56  1.84  0.53  0.44  0.00
##   1 11.58 11.49  6.84  1.75  0.88  0.18  0.00
##   2  8.77  9.39  6.14  1.14  0.88  0.18  0.18
##   3  4.47  3.25  2.46  0.61  0.18  0.18  0.09
##   4  1.40  1.05  0.70  0.00  0.35  0.00  0.00
##   5  0.88  0.53  0.44  0.00  0.09  0.00  0.00
##   6  0.26  0.18  0.00  0.09  0.00  0.00  0.00
##   7  0.00  0.09  0.00  0.00  0.00  0.00  0.00
##   8  0.00  0.00  0.09  0.00  0.00  0.00  0.00
```

Realizaremos ahora un *heat map* para visualizar los datos:


```r
heat.df <- as.data.frame(t(prob.conjunta)) #convertimos la matriz a df
colnames(heat.df)=c("Visitante","Local", "Probabilidad")
heat.df
```

```
##    Visitante Local Probabilidad
## 1          0     0         7.81
## 2          1     0         8.07
## 3          2     0         4.56
## 4          3     0         1.84
## 5          4     0         0.53
## 6          5     0         0.44
## 7          6     0         0.00
## 8          0     1        11.58
## 9          1     1        11.49
...
```



## Figura 3.3 P(x∩y) conjunta


```r
ggplotly(p) #versión interactiva
```


<img src="https://github.com/itzamango/postwork-equipo-10/blob/main/img/3.png?raw=true">


# POSTWORK 4

Ahora obtendremos una tabla de cocientes al dividir las probabilidades
conjuntas por el producto de las probabilidades correspondientes:

$Cocientes=Pxy.conjunta/(Px.marginal*Py.marginal)$


```r
df <- read.csv("https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_02.csv")
total_partidos <- length(df$FTHG)
```

Para la probabilidad marginal de los goles metidos por locales:


```r
goles.local <-table(df$FTHG)
prob.marginal.local = prop.table(goles.local) #probabilidad marginal
table.local = data.frame(goles.local, prob.marginal.local, 
                         check.names = T)[-3]
colnames(table.local) = c("Goles", "Frecuencia", "P. Marginal") #Renombramos las columnas
table.local
```

```
##   Goles Frecuencia  P. Marginal
## 1     0        363 0.2388157895
## 2     1        503 0.3309210526
## 3     2        402 0.2644736842
## 4     3        157 0.1032894737
## 5     4         59 0.0388157895
## 6     5         27 0.0177631579
## 7     6          7 0.0046052632
## 8     7          1 0.0006578947
## 9     8          1 0.0006578947
```

Para los goles metidos por el equipo visitante:


```r
goles.visitante <-table(df$FTAG)
prob.marginal.visitantes <- prop.table(goles.visitante) #probabilidad marginal
table.visitante = data.frame(goles.visitante, prob.marginal.visitantes, 
                              check.names = T)[-3]
colnames(table.visitante) = c("Goles", "Frecuencia", "P. Marginal") #Renombramos las columnas
table.visitante
```

```
##   Goles Frecuencia P. Marginal
## 1     0        516 0.339473684
## 2     1        540 0.355263158
## 3     2        315 0.207236842
## 4     3         90 0.059210526
## 5     4         43 0.028289474
## 6     5         12 0.007894737
## 7     6          4 0.002631579
```

Hacemos 2 tablas con las probabilidades individuales repitiendolas en
las columnas:


```r
visitante <- rbind(table.visitante$`P. Marginal`) #probabilidad visitante
for (i in 1:8) {
  visitante <-rbind(visitante,table.visitante$`P. Marginal`)
}
(round(visitante, 3))
```

```
##        [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]
##  [1,] 0.339 0.355 0.207 0.059 0.028 0.008 0.003
##  [2,] 0.339 0.355 0.207 0.059 0.028 0.008 0.003
##  [3,] 0.339 0.355 0.207 0.059 0.028 0.008 0.003
##  [4,] 0.339 0.355 0.207 0.059 0.028 0.008 0.003
##  [5,] 0.339 0.355 0.207 0.059 0.028 0.008 0.003
##  [6,] 0.339 0.355 0.207 0.059 0.028 0.008 0.003
##  [7,] 0.339 0.355 0.207 0.059 0.028 0.008 0.003
##  [8,] 0.339 0.355 0.207 0.059 0.028 0.008 0.003
##  [9,] 0.339 0.355 0.207 0.059 0.028 0.008 0.003
```

```r
local <- cbind(table.local$`P. Marginal`) #probabilidad local
for (i in 1:6) {
  local <-cbind(local,table.local$`P. Marginal`)
}
(round(local, 3))
```

```
##        [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]
##  [1,] 0.239 0.239 0.239 0.239 0.239 0.239 0.239
##  [2,] 0.331 0.331 0.331 0.331 0.331 0.331 0.331
##  [3,] 0.264 0.264 0.264 0.264 0.264 0.264 0.264
##  [4,] 0.103 0.103 0.103 0.103 0.103 0.103 0.103
##  [5,] 0.039 0.039 0.039 0.039 0.039 0.039 0.039
##  [6,] 0.018 0.018 0.018 0.018 0.018 0.018 0.018
##  [7,] 0.005 0.005 0.005 0.005 0.005 0.005 0.005
##  [8,] 0.001 0.001 0.001 0.001 0.001 0.001 0.001
##  [9,] 0.001 0.001 0.001 0.001 0.001 0.001 0.001
```

```r
producto.probabilidad <- local*visitante #Realizamos el producto
producto.probabilidad <- producto.probabilidad
producto <- data.frame(producto.probabilidad)
(round(producto, 3))
```

```
##      X1    X2    X3    X4    X5    X6    X7
## 1 0.081 0.085 0.049 0.014 0.007 0.002 0.001
## 2 0.112 0.118 0.069 0.020 0.009 0.003 0.001
## 3 0.090 0.094 0.055 0.016 0.007 0.002 0.001
## 4 0.035 0.037 0.021 0.006 0.003 0.001 0.000
## 5 0.013 0.014 0.008 0.002 0.001 0.000 0.000
## 6 0.006 0.006 0.004 0.001 0.001 0.000 0.000
## 7 0.002 0.002 0.001 0.000 0.000 0.000 0.000
## 8 0.000 0.000 0.000 0.000 0.000 0.000 0.000
## 9 0.000 0.000 0.000 0.000 0.000 0.000 0.000
```

La probabilidad conjunta:


```r
goles.partidos = table(df$FTHG, df$FTAG, dnn=c("x", "y"))
prob.conjunta = prop.table(goles.partidos)
prob.conjunta <- prob.conjunta
(round(prob.conjunta,3))
```

```
##    y
## x       0     1     2     3     4     5     6
##   0 0.078 0.083 0.051 0.018 0.006 0.003 0.000
##   1 0.112 0.123 0.062 0.022 0.009 0.002 0.001
##   2 0.086 0.097 0.060 0.012 0.008 0.001 0.001
##   3 0.039 0.032 0.022 0.006 0.003 0.001 0.001
##   4 0.015 0.013 0.007 0.001 0.003 0.000 0.000
##   5 0.007 0.005 0.005 0.000 0.001 0.000 0.000
##   6 0.002 0.002 0.000 0.001 0.000 0.000 0.000
##   7 0.000 0.001 0.000 0.000 0.000 0.000 0.000
##   8 0.000 0.000 0.001 0.000 0.000 0.000 0.000
```

## Cociente de las probabilidades

Realizamos el cociente:


```r
cociente <- prob.conjunta/producto
#cociente
colnames(cociente) = c(0, 1, 2, 3, 4, 5,6) #Cambiamos los nombres
rownames(cociente) = c(0, 1, 2, 3, 4, 5, 6,7,8)
(round(cociente,2)) #df final del cociente
```

```
##      0    1    2    3    4    5    6
## 0 0.97 0.98 1.02 1.26 0.88 1.74 0.00
## 1 1.00 1.05 0.91 1.14 0.91 0.76 0.76
## 2 0.95 1.03 1.09 0.76 1.06 0.63 1.89
## 3 1.13 0.86 1.01 0.97 0.90 1.61 2.42
## 4 1.15 0.95 0.90 0.29 2.40 0.00 0.00
## 5 1.20 0.83 1.25 0.00 1.31 0.00 0.00
## 6 1.26 1.21 0.00 2.41 0.00 0.00 0.00
## 7 0.00 2.81 0.00 0.00 0.00 0.00 0.00
## 8 0.00 0.00 4.83 0.00 0.00 0.00 0.00
```

Para determinar si el número de goles del equipo local o el de el equipo
visitante son dependientes o independientes, realizaremos un
procedimiento de bootstrap para obtener más cocientes similares y
analizar la distribución.

Transformamos el data frame a columna para facilitar el bootstrap.


```r
data_origin <- as.data.frame(as.vector(unlist(cociente)))

colnames(data_origin) <- "values"
```

Utilizamos la biblioteca `"rsample"` para poder hacer las muestras
bootstrap:


```r
library(rsample)
```

Fijamos la semilla para poder reproducir los datos:


```r
set.seed(83928782)
```

Aplicamos la función bootstraps, para generar 1000 muestras,
guardándolas en boot:


```r
boot <- bootstraps(data_origin, times = 1000)
```

Cargamos las siguientes bibliotecas:


```r
library(purrr)
library(modeldata)
library(viridis)
library(tidyverse)
library(hrbrthemes)
library(forcats)
```

Realizamos una función para hacer una columna de las medias muestrales
obtenidas por bootstrap y aplicamos la función:




```r
boot$means <- map_dbl(boot$splits, obtener_media)
length(boot$means); summary(boot$means)
```

```
## [1] 1000
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.4996  0.7523  0.8256  0.8288  0.9048  1.1796
```

Observamos el valor de la media de las medias muestrales.

Realizamos una función para un histograma:



Comprobamos la hipótesis de que la media se encuentra en 1 con las
medidas muestrales bootstrap y obtenemos el intervalo de confianza de
95% con una prueba t:


```r
t_boot <- t.test(boot$means, alternative = "two.sided", mu = 1)
t_boot_ic <- round(t_boot$conf.int,3)
t_boot_ic
```

```
## [1] 0.822 0.835
## attr(,"conf.level")
## [1] 0.95
```

Realizamos el histograma de las muestras obtenidas por bootstrap.







## Figura 4.1 Histograma bootstrap


```r
ggplotly(hist_boot)
```

<img src="https://github.com/itzamango/postwork-equipo-10/blob/7d498adad44bc306fff4238f65491e09f4578516/img/4.png?raw=true">


> La línea sólida indica la posición de la media y las punteadas, la
> posición de los límites del intervalo de confianza.

De igual modo lo hacemos para la muestra original:


```r
t_origin <- t.test(data_origin$values, alternative = "two.sided", mu = 1)
t_origin_ic <- round(t_origin$conf.int, 3)
t_origin_ic
```

```
## [1] 0.613 1.053
## attr(,"conf.level")
## [1] 0.95
```



## Figura 4.2 Histograma original


```r
ggplotly(hist_origin)
```


<img src="https://github.com/itzamango/postwork-equipo-10/blob/7d498adad44bc306fff4238f65491e09f4578516/img/5.png?raw=true">


> La línea sólida indica la posición de la media y las punteadas, la
> posición de los límites del intervalo de confianza.

Vemos los datos de los estadísticos de las pruebas t para ambos conjuntos de datos.

Remuestreo bootstrap:



```r
t_boot
```

```
## 
## 	One Sample t-test
## 
## data:  boot$means
## t = -50.464, df = 999, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 1
## 95 percent confidence interval:
##  0.8221057 0.8354231
## sample estimates:
## mean of x 
## 0.8287644
```

Muestras originales:


```r
t_origin
```

```
## 
## 	One Sample t-test
## 
## data:  data_origin$values
## t = -1.5194, df = 62, p-value = 0.1338
## alternative hypothesis: true mean is not equal to 1
## 95 percent confidence interval:
##  0.6130959 1.0527411
## sample estimates:
## mean of x 
## 0.8329185
```

## Conclusiones

En la teoría para que haya independencia de variables es necesario que
todos los cocientes de las probabilidades resulten en uno. En la
práctica a simple vista por la cercanía entre los valores se podría
considerar como independientes.

Es mejor realizar un método de remuestreo para estimar la media
poblacional acorde al Teorema del Limite Central. Usamos el método de
bootstrap, con el cual obtuvimos las medias muestrales del 'resampling'
de 1000 muestras a partir de la original.

Apreciamos que en el rango del intervalo de confianza de 95% de las
medias bootstrap no se encuentra el 1, el valor p es mucho menor a 0.025
y la media estimada no está en 1.

A partir de ello podemos decir que la hipótesis nula de que sean las
variables son independentes al ser la media 1 de ambas muestras es rechazada. 
Por tanto, aceptamos las variables de goles
anotados por el equipo local y el equipo visitante en un partido como
variables dependientes.

Este resultado tiene sentido, al saber que el evento de que un equipo
meta gol depende de su interacción con el otro equipo. Por ejemplo, que
un jugador lesione intencionalmente al jugador de otro equipo, incluso
se puede considerar si el equipo entra en estrés al ver que falta poco
tiempo y no superan el marcador.

En el área de análisis de datos será una parte importante identificar
las variables dependientes, puesto que a partir de la relación entre
ellas se encontrarán patrones y tendencias. Identificarlas para hacer
una propuesta a partir de ello va a dar valor a los datos.

Por ejemplo, la tendencia de compras de ciertos muebles; saber en cuál
temporada se venden más hará que una tienda gaste menos en
almacenamiento en bodega por ellos y pueda ocupar esos recursos en otras
áreas. O encontrar que un producto piloto es preferido por una población
de cierto grupo etario u otras características para enfocar los recursos
en diseñar una campaña de marketing dirigido especialmente a ellos.

Y para ello, es necesario utilizar un método para estimar los
estádisticos de la población. Hay una diferencia notoria entre los
estádisticos de la muestra original y los del remuestreo. Aunque la
media de ambos no difiere mucho, el rango del intervalo de confianza sí.

Con los datos originales, el intervalo de confianza cubre al 1 
mientras que con los estadísticos de las muestras bootstrap la
probabilidad que la media poblacional sea 1 es super baja y podemos
aceptar las variables como dependientes.

