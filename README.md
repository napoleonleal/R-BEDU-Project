
<a href="https://itzamango.github.io/postwork-equipo-10/" target="_blank"><h1>PARA VER LA VERSIÓN EN HTML RESPONSIVA DEL SCRIPT DE R CON GRÁFICAS INTERACTIVAS HAZ CLICK EN ESTA ORACIÓN</h1></a>

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


```r
(total_partidos = length(df$FTHG)) #total de los partidos
```

```
## [1] 380
```

```r
(prob.marginal = goles.local / total_partidos) #probabilidad marginal 
```

```
## 
##           0           1           2           3           4           5 
## 0.231578947 0.347368421 0.260526316 0.100000000 0.036842105 0.021052632 
##           6 
## 0.002631579
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

En el caso del equipo vistante el procedimiento es análogo.


```r
goles.visitante <-table(df$FTAG) #total de los partidos
(prob.marginal = goles.visitante / total_partidos) #probabilidad marginal
```

```
## 
##           0           1           2           3           4           5 
## 0.357894737 0.352631579 0.213157895 0.047368421 0.023684211 0.005263158
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

La probabilidad conjunta toma en cuenta la probabilidad de dos eventos
sobre el total de resultados posibles.

Calcularemos la probabilidad conjunta de que el equipo local anote '*x'*
goles y el visitante '*y'* goles:


```r
goles.partidos = table(df$FTHG, df$FTAG, dnn=c("x", "y"))
(prob.conjunta = prop.table(goles.partidos))
```

```
##    y
## x             0           1           2           3           4           5
##   0 0.086842105 0.073684211 0.039473684 0.021052632 0.005263158 0.005263158
##   1 0.113157895 0.128947368 0.084210526 0.013157895 0.007894737 0.000000000
##   2 0.102631579 0.092105263 0.052631579 0.007894737 0.005263158 0.000000000
##   3 0.036842105 0.036842105 0.018421053 0.005263158 0.002631579 0.000000000
##   4 0.010526316 0.013157895 0.010526316 0.000000000 0.002631579 0.000000000
##   5 0.005263158 0.007894737 0.007894737 0.000000000 0.000000000 0.000000000
##   6 0.002631579 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000
```

```r
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


```r
library(dplyr)
library(ggplot2)
library(viridis)
as.data.frame(goles.local) %>% 
  ggplot() + geom_bar(aes(Goles, Prob.Marginal, fill = Prob.Marginal), stat = 'identity') + 
  ggtitle('Probabilidad de que el equipo local anote goles') +
  ylab('Probabilidad de ocurrencia [%]') +
  scale_fill_viridis(name="Pmarginal", direction = 1) + 
  theme_minimal() -> e.local
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


```r
as.data.frame(goles.visitante) %>% 
  ggplot() + geom_bar(aes(Goles, pm.vis, fill = pm.vis), stat = 'identity') + 
  ggtitle('Probabilidad de que el equipo visitante anote goles') +
  ylab('Probabilidad de ocurrencia [0,1]') +
  scale_fill_viridis(name="Pmarginal", direction = 1) +
  theme_minimal() -> e.visitante
ggplotly(e.visitante) #versión interactiva
```


<img src="https://github.com/itzamango/postwork-equipo-10/blob/main/img/2.png?raw=true">


La probabilidad conjunta de que el equipo que juega en casa anote *'x'*
goles y el equipo que juega como visitante anote '*y'* goles:


```r
goles.partidos = table(data$FTHG, data$FTAG, dnn=c("x", "y"))
(prob.conjunta = prop.table(goles.partidos))
```

```
##    y
## x             0           1           2           3           4           5
##   0 0.078070175 0.080701754 0.045614035 0.018421053 0.005263158 0.004385965
##   1 0.115789474 0.114912281 0.068421053 0.017543860 0.008771930 0.001754386
##   2 0.087719298 0.093859649 0.061403509 0.011403509 0.008771930 0.001754386
##   3 0.044736842 0.032456140 0.024561404 0.006140351 0.001754386 0.001754386
##   4 0.014035088 0.010526316 0.007017544 0.000000000 0.003508772 0.000000000
##   5 0.008771930 0.005263158 0.004385965 0.000000000 0.000877193 0.000000000
##   6 0.002631579 0.001754386 0.000000000 0.000877193 0.000000000 0.000000000
##   7 0.000000000 0.000877193 0.000000000 0.000000000 0.000000000 0.000000000
##   8 0.000000000 0.000000000 0.000877193 0.000000000 0.000000000 0.000000000
##    y
## x             6
##   0 0.000000000
##   1 0.000000000
##   2 0.001754386
##   3 0.000877193
##   4 0.000000000
##   5 0.000000000
##   6 0.000000000
##   7 0.000000000
##   8 0.000000000
```

```r
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


```r
p <- ggplot(heat.df, aes(Local, Visitante, fill= Probabilidad)) + #gráficamos
  geom_raster() +
  scale_fill_viridis_c(name="Probabilidad [%]") +
  ggtitle("Probabilidad conjunta de anotación") +
  ylab('Visitante [goles]') +
  xlab('Local [goles]')
#p
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
visitante
```

```
##            [,1]      [,2]      [,3]       [,4]       [,5]        [,6]
##  [1,] 0.3394737 0.3552632 0.2072368 0.05921053 0.02828947 0.007894737
##  [2,] 0.3394737 0.3552632 0.2072368 0.05921053 0.02828947 0.007894737
##  [3,] 0.3394737 0.3552632 0.2072368 0.05921053 0.02828947 0.007894737
##  [4,] 0.3394737 0.3552632 0.2072368 0.05921053 0.02828947 0.007894737
##  [5,] 0.3394737 0.3552632 0.2072368 0.05921053 0.02828947 0.007894737
##  [6,] 0.3394737 0.3552632 0.2072368 0.05921053 0.02828947 0.007894737
##  [7,] 0.3394737 0.3552632 0.2072368 0.05921053 0.02828947 0.007894737
##  [8,] 0.3394737 0.3552632 0.2072368 0.05921053 0.02828947 0.007894737
##  [9,] 0.3394737 0.3552632 0.2072368 0.05921053 0.02828947 0.007894737
##              [,7]
##  [1,] 0.002631579
##  [2,] 0.002631579
##  [3,] 0.002631579
##  [4,] 0.002631579
##  [5,] 0.002631579
##  [6,] 0.002631579
##  [7,] 0.002631579
##  [8,] 0.002631579
##  [9,] 0.002631579
```

```r
local <- cbind(table.local$`P. Marginal`) #probabilidad local
for (i in 1:6) {
  local <-cbind(local,table.local$`P. Marginal`)
}
local
```

```
##               [,1]         [,2]         [,3]         [,4]         [,5]
##  [1,] 0.2388157895 0.2388157895 0.2388157895 0.2388157895 0.2388157895
##  [2,] 0.3309210526 0.3309210526 0.3309210526 0.3309210526 0.3309210526
##  [3,] 0.2644736842 0.2644736842 0.2644736842 0.2644736842 0.2644736842
##  [4,] 0.1032894737 0.1032894737 0.1032894737 0.1032894737 0.1032894737
##  [5,] 0.0388157895 0.0388157895 0.0388157895 0.0388157895 0.0388157895
##  [6,] 0.0177631579 0.0177631579 0.0177631579 0.0177631579 0.0177631579
##  [7,] 0.0046052632 0.0046052632 0.0046052632 0.0046052632 0.0046052632
##  [8,] 0.0006578947 0.0006578947 0.0006578947 0.0006578947 0.0006578947
##  [9,] 0.0006578947 0.0006578947 0.0006578947 0.0006578947 0.0006578947
##               [,6]         [,7]
##  [1,] 0.2388157895 0.2388157895
##  [2,] 0.3309210526 0.3309210526
##  [3,] 0.2644736842 0.2644736842
##  [4,] 0.1032894737 0.1032894737
##  [5,] 0.0388157895 0.0388157895
##  [6,] 0.0177631579 0.0177631579
##  [7,] 0.0046052632 0.0046052632
##  [8,] 0.0006578947 0.0006578947
##  [9,] 0.0006578947 0.0006578947
```

```r
producto.probabilidad <- local*visitante #Realizamos el producto
producto.probabilidad <- producto.probabilidad
(producto <- data.frame(producto.probabilidad))
```

```
##            X1           X2           X3           X4           X5           X6
## 1 0.081071676 0.0848424515 0.0494914301 1.414041e-02 0.0067559730 1.885388e-03
## 2 0.112338989 0.1175640582 0.0685790339 1.959401e-02 0.0093615824 2.612535e-03
## 3 0.089781856 0.0939577562 0.0548086911 1.565963e-02 0.0074818213 2.087950e-03
## 4 0.035064058 0.0366949446 0.0214053843 6.115824e-03 0.0029220048 8.154432e-04
## 5 0.013176939 0.0137898199 0.0080440616 2.298303e-03 0.0010980783 3.064404e-04
## 6 0.006030125 0.0063105956 0.0036811807 1.051766e-03 0.0005025104 1.402355e-04
## 7 0.001563366 0.0016360803 0.0009543802 2.726801e-04 0.0001302805 3.635734e-05
## 8 0.000223338 0.0002337258 0.0001363400 3.895429e-05 0.0000186115 5.193906e-06
## 9 0.000223338 0.0002337258 0.0001363400 3.895429e-05 0.0000186115 5.193906e-06
##             X7
## 1 6.284626e-04
## 2 8.708449e-04
## 3 6.959834e-04
## 4 2.718144e-04
## 5 1.021468e-04
## 6 4.674515e-05
## 7 1.211911e-05
## 8 1.731302e-06
## 9 1.731302e-06
```

La probabilidad conjunta:


```r
goles.partidos = table(df$FTHG, df$FTAG, dnn=c("x", "y"))
prob.conjunta = prop.table(goles.partidos)
(prob.conjunta <- prob.conjunta)
```

```
##    y
## x              0            1            2            3            4
##   0 0.0782894737 0.0828947368 0.0506578947 0.0177631579 0.0059210526
##   1 0.1118421053 0.1230263158 0.0625000000 0.0223684211 0.0085526316
##   2 0.0855263158 0.0967105263 0.0598684211 0.0118421053 0.0078947368
##   3 0.0394736842 0.0315789474 0.0217105263 0.0059210526 0.0026315789
##   4 0.0151315789 0.0131578947 0.0072368421 0.0006578947 0.0026315789
##   5 0.0072368421 0.0052631579 0.0046052632 0.0000000000 0.0006578947
##   6 0.0019736842 0.0019736842 0.0000000000 0.0006578947 0.0000000000
##   7 0.0000000000 0.0006578947 0.0000000000 0.0000000000 0.0000000000
##   8 0.0000000000 0.0000000000 0.0006578947 0.0000000000 0.0000000000
##    y
## x              5            6
##   0 0.0032894737 0.0000000000
##   1 0.0019736842 0.0006578947
##   2 0.0013157895 0.0013157895
##   3 0.0013157895 0.0006578947
##   4 0.0000000000 0.0000000000
##   5 0.0000000000 0.0000000000
##   6 0.0000000000 0.0000000000
##   7 0.0000000000 0.0000000000
##   8 0.0000000000 0.0000000000
```

Realizamos el cociente:


```r
cociente <- prob.conjunta/producto
#cociente
colnames(cociente) = c(0, 1, 2, 3, 4, 5,6) #Cambiamos los nombres
rownames(cociente) = c(0, 1, 2, 3, 4, 5, 6,7,8)
cociente #df final del cociente
```

```
##           0         1         2         3         4         5         6
## 0 0.9656822 0.9770432 1.0235690 1.2561983 0.8764175 1.7447199 0.0000000
## 1 0.9955769 1.0464620 0.9113573 1.1415949 0.9135882 0.7554672 0.7554672
## 2 0.9526013 1.0292980 1.0923162 0.7562189 1.0551892 0.6301824 1.8905473
## 3 1.1257591 0.8605803 1.0142554 0.9681529 0.9006073 1.6135881 2.4203822
## 4 1.1483379 0.9541745 0.8996503 0.2862524 2.3965313 0.0000000 0.0000000
## 5 1.2001148 0.8340192 1.2510288 0.0000000 1.3092162 0.0000000 0.0000000
## 6 1.2624585 1.2063492 0.0000000 2.4126984 0.0000000 0.0000000 0.0000000
## 7 0.0000000 2.8148148 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
## 8 0.0000000 0.0000000 4.8253968 0.0000000 0.0000000 0.0000000 0.0000000
```

Para determinar si el número de goles del equipo local o el de el equipo
visitante son dependientes o independientes, realizaremos un
procedimiento de bootstrap para obtener más cocientes similares y
analizar la distribución.

Transformamos el data frame a columna para facilitar el bootstrap.


```r
aux = matrix(, nrow=9*7, 1)
indice = 1
for(i in 1:7){
  for(k in 1:9){
    aux[indice,1] = cociente[k,i]
    indice= indice + 1
  }
}
```

Este es nuestro dataframe, con la información de los cocientes.


```r
dataframe = as.data.frame(aux)
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
boot <- bootstraps(dataframe, times = 1000)
```

Confirmamos la dimensión de nuestras muestras:


```r
dim(as.data.frame(boot$splits[[5]]))
```

```
## [1] 63  1
```

Guardamos nuestro dataframe para poder añadir las muestras para
facilitar su análisis:


```r
data_f = dataframe
```

Juntamos las columnas de nuestras muestras, para poder aplicar `apply`
directo:


```r
for(i in 1:length(boot$splits)){
  data_f = cbind(data_f, as.data.frame(boot$splits[[i]]))
}
```

Calculamos la media por cociente (por renglón):


```r
mean_conj = apply(data_f, 1, mean)
```

De forma analoga calculamos la varianza (por renglón)


```r
var_conj = apply(data_f, 1, var)
```

Juntamos en un solo dataframe las columnas, media, varianza para su
análisis:


```r
analisis <- cbind(dataframe, mean_conj, var_conj)
```

Utilizamos el teorema de límite central, para poder calcular la
probabilidad de la distribución normal, ya que nuestra muestra contiene
1000 datos


```r
analisis <- mutate(analisis, 
                   probabilidad = pnorm(q = V1, mean = mean_conj, 
                                        sd = sqrt(var_conj/1000)))
```

Hacemos los cambios en los nombres, y añadimos el número de goles para
su presentación:


```r
analisis <- cbind(c(rep(0,9), rep(1,9), rep(2,9), 
                    rep(3,9), rep(4,9), rep(5,9), 
                    rep(6,9)),analisis)
analisis <- cbind(rep(seq(0,8,1),7),analisis)
colnames(analisis) = c("goles local", "goles visitante",
                       "Cociente", "Media", "Varianza", 
                       "Probabilidad")
```

Utilizamos la biblioteca `"dplyr"` para el análisis:


```r
library(dplyr)
```

Filtramos los valores en donde el cociente es proporcional a 1. Ya que
esto significa que la probabilidad grupal es igual a la probabilidad
independiente.


```r
(dependiente <- filter(analisis, Cociente >= 0.97 & Cociente <= 1.04))
```

```
##      goles local goles visitante  Cociente     Media  Varianza Probabilidad
## 49             1               0 0.9955769 0.7929626 0.6455403            1
## 7              0               1 0.9770432 0.8250909 0.7949466            1
## 2              2               1 1.0292980 0.8397165 0.7896762            1
## 48.1           0               2 1.0235690 0.8290051 0.7101425            1
## 2.1            3               2 1.0142554 0.7911646 0.7151448            1
```

### Conclusiones

Podemos apreciar que la probabilidad no es en todos los casos de 1.  Para la independencia de variables es necesario que todos los cocientes sean uno, pero tomando en cuenta que es en teoría ese argumento; por la cercania de los valores podríamos considerarlas como independientes, pero tomando en cuenta la media, acorde al Teorema del Limite Central, vimos que el valor es alejado de 1 mediante el procedimiento bootstrap, del cual encontraremos las medias muestrales del 'resampling' de 1000 muestras.

Por lo que la hipótesis de que las variables sean independientes es descartada al no estar la media de las medias muestrales (la media poblacional) en el rango de 1 ± el intervalo de confianza utilizando el promedio de las medias muestrales generadas por bootstrap.

Este resultado tiene sentido, al saber que el evento de que un equipo meta gol depende de su interacción con el otro equipo. Por ejemplo, que un jugador lesione intencionalmente al jugador de otro equipo, incluso se puede considerar si el equipo entra en estrés al ver que falta poco tiempo y no superan el marcador.

En el área de ciencia de datos será una parte importante identificar las variables dependientes, puesto que a partir de la relación entre ellas se encontrarán patrones y tendencias. Identificarlas para hacer una propuesta a partir de ello va a dar valor a los datos. 

Por ejemplo, la tendencia de compras de ciertos muebles; saber en cuál temporada se venden más hará que una tienda gaste menos en almanecamiento en bodega por ellos y pueda ocupar esos recursos en otras áreas. O encontrar que un producto piloto es preferido por una población de cierto grupo etario u otras características para enfocar los recursos en diseñar una campaña de marketing dirigido especialmente a ellos.
