# 📂 POSTWORK 1

Importamos las bibliotecas con las que trabajaremos:

``` r
library(dplyr)
```

Importamos datos de la primera división 2019-2020:

``` r
data <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
```

Vemos la ayuda del comando table:

``` r
?table
```

## 🔢 get.freq()

``` r
get.freq <- function(data, team, name.freq){
  team <- enquo(team)
  data %>%
    pull(!!team) %>%
    table(., dnn = name.freq) 
}
```

## 📋 Tablas de Frecuencias

Extraemos las columnas, obtenemos la frecuencia marginal para el equipo
de casa y visitante en una función. con table generamos una tabla que
nos indica la frecuencia de los goles:

### Equipo Local

``` r
freq.m.local <- 
  get.freq(data, FTHG, "Freq.Home")

(freq.m.local)
```

    ## Freq.Home
    ##   0   1   2   3   4   5   6 
    ##  88 132  99  38  14   8   1

### Equipo Visitante

``` r
freq.m.visit <- 
  get.freq(data, FTAG, "Freq.Away")

(freq.m.visit)
```

    ## Freq.Away
    ##   0   1   2   3   4   5 
    ## 136 134  81  18   9   2

## 🔢 get.prob.m.tbl()

``` r
get.prob.m.tbl <- function(data, team, name.prob){
  team <- enquo(team)
  data %>%
    pull(!!team) %>%
    table %>%
    prop.table -> prob.tbl
  
  names(attributes(prob.tbl)$dimnames) <- name.prob
  return(prob.tbl)
}
```

## 📋 Probabilidad Marginal

La probabilidad marginal es la probabilidad de que ocurra un evento
simple, por lo tanto estará dada por el número de goles sobre el total
de los partidos.

Calcularemos la probabilidad de que el equipo que juegue en casa anote
’*x’* goles en una función:

### Equipo Local

La probabilidad marginal de que el equipo que juega local anote x goles
(x = 0, 1, 2, …)

``` r
prob.m.local <- 
  get.prob.m.tbl(data, FTHG, "Prob.Home")

(prob.m.local %>% round(3))
```

    ## Prob.Home
    ##     0     1     2     3     4     5     6 
    ## 0.232 0.347 0.261 0.100 0.037 0.021 0.003

### Equipo Visitante

La probabilidad marginal de que el equipo que juega como visitante anote
y goles (y = 0, 1, 2, …)

En el caso del equipo vistante el procedimiento es análogo.

``` r
prob.m.visit <- 
  get.prob.m.tbl(data, FTAG, "Prob.Away")

(prob.m.visit %>% round(3))
```

    ## Prob.Away
    ##     0     1     2     3     4     5 
    ## 0.358 0.353 0.213 0.047 0.024 0.005

## 🔢 get.prob.df() y get.round()

``` r
get.prob.df<- function(data, team, name.gol, name.prob){ 
  team      <- enquo(team)
  data %>% 
    pull(!!team) %>% 
    table(., dnn = (name.gol)) %>% 
    as.data.frame %>% 
    mutate(!!name.prob := Freq/sum(Freq)) 
}
```

``` r
get.round <- function(data, digits){
  data %>% mutate_if(is.numeric, round, digits=digits)
}
```

## 📋 Frecuencias y Probabilidad Marginal

Obtenemos un dataframe con las frecuencias y probabilidades con una
función y redondeamos:

### Equipo Local

``` r
data.local <- get.prob.df(data, FTHG, "Gol.Home", "Prob.Marginal")

(data.local %>% get.round(3))
```

    ##   Gol.Home Freq Prob.Marginal
    ## 1        0   88         0.232
    ## 2        1  132         0.347
    ## 3        2   99         0.261
    ## 4        3   38         0.100
    ## 5        4   14         0.037
    ## 6        5    8         0.021
    ## 7        6    1         0.003

### Equipo Visitante

``` r
data.visit <- get.prob.df(data, FTAG, "Gol.Away", "Prob.Marginal" )

(data.visit %>% get.round(3))
```

    ##   Gol.Away Freq Prob.Marginal
    ## 1        0  136         0.358
    ## 2        1  134         0.353
    ## 3        2   81         0.213
    ## 4        3   18         0.047
    ## 5        4    9         0.024
    ## 6        5    2         0.005

## 🔢 get.prob.joint.tbl() y get.prob.joint.df

``` r
get.prob.joint.tbl <- function(data, team.h, team.a, name.h, name.a){
  data %>%
  {table( team.h,  team.a, dnn = c(name.h, name.a)) } %>% 
  prop.table
}
 
get.prob.joint.df <- function(data, team.h, team.a, name.h, name.a){
  data %>%
  {table( team.h,  team.a, dnn = c(name.h, name.a)) } %>% 
  prop.table %>% 
  unclass %>% 
  as.data.frame
}
```

## 📋 Probabilidad Conjunta P(x∩y)

La probabilidad conjunta toma en cuenta la probabilidad de dos eventos
sobre el total de resultados posibles.

Calcularemos la probabilidad conjunta de que el equipo local anote ‘*x’*
goles y el visitante’*y’* goles (x = 0, 1, 2, …, y = 0, 1, 2, …) con una
función:

``` r
table.prob.joint <-
  get.prob.joint.tbl(data, data$FTHG, data$FTAG, "x (Home)", "y (Away)")

(table.prob.joint %>% round(3))
```

    ##         y (Away)
    ## x (Home)     0     1     2     3     4     5
    ##        0 0.087 0.074 0.039 0.021 0.005 0.005
    ##        1 0.113 0.129 0.084 0.013 0.008 0.000
    ##        2 0.103 0.092 0.053 0.008 0.005 0.000
    ##        3 0.037 0.037 0.018 0.005 0.003 0.000
    ##        4 0.011 0.013 0.011 0.000 0.003 0.000
    ##        5 0.005 0.008 0.008 0.000 0.000 0.000
    ##        6 0.003 0.000 0.000 0.000 0.000 0.000
