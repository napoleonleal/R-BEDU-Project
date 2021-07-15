# 📂 POSTWORK 5

Con los datos de la liga de primera división española, de las temporadas
2017/2018, 2018/2019 y 2019/2020 haremos una predicción de los
resultados de los partidos de la fecha 07/16/2020.

Utilizamos la librería dplyr para manipulaciín de datos:

``` r
library(dplyr)
```

Y la librería fbRanks para las predicciones en base al modelo de Dixon y
Coles:

``` r
library(fbRanks)
```

## 📋 Escribir soccer.csv

Guardamos los datos de las 3 temporadas en una lista. Seleccionamos
unicamente los elementos “Date”, “HomeTeam”, “FTHG”, “AwayTeam” ,“FTAG”,
los cuales son esenciales para el modelo y cambiamos los nombres de las
columnas por requerimiento de la biblioteca. Unimos las 3 temporadas en
una sola y Cambiamos la forma del año, para poder aplicar un único
formato date:

``` r
columns <- c(  date       = "Date"
             , home.team  = "HomeTeam" 
             , away.team  = "AwayTeam"
             , home.score = "FTHG"
             , away.score = "FTAG"
             )

SmallData <-
    c( "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
     , "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
     , "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
     ) %>% 
        lapply (read.csv)                     %>%
        lapply (select, all_of(columns))      %>% 
        do.call(rbind, .)                     %>% 
        mutate (date = gsub("/(1[78])$", 
                            "/20\\1", date))  %>%
        mutate (date = as.Date(date, "%d/%m/%Y"))
```

Guardamos los cambios en un archivo csv:

``` r
w.dir   <- getwd()
sub.dir <- "equipo10"
path    <- file.path(w.dir, sub.dir)
dir.create(path, showWarnings = F, recursive = T)
setwd(path)

write.csv(SmallData, file = './soccer.csv', row.names = F)
setwd(w.dir)
```

## 📋 fbRanks: anotaciones y equipos

Aplicamos la primera función de la biblioteca,
“create.fbranks.dataframes” con la finalidad de poder hacer una limpieza
de datos, excluyendo los datos nulos para las puntuaciones, así como
nombres repetidos, los cambios incluyen la transformación del formato de
columna “date”:

``` r
listasoccer <- create.fbRanks.dataframes(scores.file = "soccer.csv")
```

    ## Alert: teams info file was not passed in.
    ## Will construct one from the scores data frame but teams in the scores file must use a unique name.
    ## Alert: teams resolver was not passed in.
    ## Will construct one from the team info data frame.

``` r
(listasoccer)
```

    ## $scores
    ##            date   home.team   away.team home.score away.score
    ## 1    2017-08-18     Leganes      Alaves          1          0
    ## 2    2017-08-18    Valencia  Las Palmas          1          0
    ## 3    2017-08-19       Celta    Sociedad          2          3
    ## 4    2017-08-19      Girona  Ath Madrid          2          2
    ## 5    2017-08-19     Sevilla     Espanol          1          1
    ## 6    2017-08-20  Ath Bilbao      Getafe          0          0
    ## 7    2017-08-20   Barcelona       Betis          2          0
    ## 8    2017-08-20   La Coruna Real Madrid          0          3
    ...

Como residuo, la función nos devuelve una lista con un data.frame scores
con los datos de nuestro csv limpios y las fechas en orden ascendente,
como también el data frame teams, en este caso generado del csv, con los
nombres de los equipos sin repetir, el data.frame teams.resolver y por
último el data.frame raw.scores, con algunas configuraciones para el uso
de otras funciones.

``` r
View(listasoccer)
```

Guardamos el data.frame scores generados, con las puntuaciones en las
temporadas

``` r
anotaciones = listasoccer$scores
```

Guardamos la lista de equipos

``` r
equipos = listasoccer$teams
```

## 📋 Ranking de Equipos

Guardamos las fechas sin repetir

``` r
fecha = unique(listasoccer$scores$date)
```

Y la cantidad de las fechas para un mejor control

``` r
n = length(fecha)
```

Aplicamos la función “rank.teams”, el cual aplica una regresión lineal
usando como modelo la distribución de Poisson, tomando como rango de
tiempo la duración de un partido.

La función requiere los datos scores, la lista de equipos y las fechas
entre las que generamos nuestro ranking

``` r
ranking = rank.teams(  anotaciones
                     , equipos
                     , max.date = fecha[n-1]
                     , min.date = fecha[1]
                     , date.format = "%Y-%m-%d")
```

    ## 
    ## Team Rankings based on matches 2017-08-18 to 2020-07-16
    ##    team        total attack defense n.games.Var1 n.games.Freq
    ## 1  Barcelona    1.51 2.23   1.28    Barcelona    113         
    ## 2  Ath Madrid   1.24 1.33   1.78    Ath Madrid   113         
    ## 3  Real Madrid  1.15 1.86   1.19    Real Madrid  113         
    ## 4  Valencia     0.56 1.34   1.10    Valencia     113         
    ## 5  Getafe       0.55 1.10   1.33    Getafe       113         
    ## 6  Sevilla      0.43 1.37   0.98    Sevilla      113         
    ## 7  Granada      0.37 1.26   1.03    Granada       37         
    ...

Como resultado nos da una lista con las especificaciones que le dimos a
nuestra función Y nos presenta una tabla ranking, con los coeficientes
de la regresión, tanto de ataque y defensa, modificados para su mejor
comprensión, y uniéndolos en un total, posicionando a los equipos con
base en el total.

``` r
(ranking)
```

    ## 
    ## Team Rankings based on matches 2017-08-18 to 2020-07-16
    ##    team        total attack defense n.games.Var1 n.games.Freq
    ## 1  Barcelona    1.51 2.23   1.28    Barcelona    113         
    ## 2  Ath Madrid   1.24 1.33   1.78    Ath Madrid   113         
    ## 3  Real Madrid  1.15 1.86   1.19    Real Madrid  113         
    ## 4  Valencia     0.56 1.34   1.10    Valencia     113         
    ## 5  Getafe       0.55 1.10   1.33    Getafe       113         
    ## 6  Sevilla      0.43 1.37   0.98    Sevilla      113         
    ## 7  Granada      0.37 1.26   1.03    Granada       37         
    ...

La función da como resultado una clase única de la librería “fbRanks”
necesaria para el uso de otras funciones

``` r
class(ranking)
```

    ## [1] "fbRanks"

En la lista “ranking” es posible encontrar los coeficientes de la
regresión en crudo, como sus especificaciones

``` r
ranking[1]
```

    ## $fit
    ## $fit$cluster.1
    ## 
    ## Call:  glm(formula = as.formula(my.formula), family = family, weights = exp(-1 * 
    ##     time.weight.eta * time.diff), na.action = "na.exclude")
    ## 
    ## Coefficients:
    ##      attackAlaves   attackAth Bilbao   attackAth Madrid    attackBarcelona  
    ##         0.0494074          0.1262803          0.3912525          0.9066502  
    ##       attackBetis        attackCelta        attackEibar      attackEspanol  
    ...

``` r
View(ranking[1])
```

Podemos extraer los datos de la función rank

``` r
columns = c(  team    = "ranks.team"
            , total   = "ranks.total"
            , attack  = "ranks.attack"
            , defense = "ranks.defense"
            , n.games.Var1 = "ranks.n.games.Var1"
            , n.games.Freq = "ranks.n.games.Freq"
            )
Ranking.datos <-
    ranking %>% 
    print   %>% 
    as.data.frame %>% 
    select(all_of(columns))
```

    ## 
    ## Team Rankings based on matches 2017-08-18 to 2020-07-16
    ##    team        total attack defense n.games.Var1 n.games.Freq
    ## 1  Barcelona    1.51 2.23   1.28    Barcelona    113         
    ## 2  Ath Madrid   1.24 1.33   1.78    Ath Madrid   113         
    ## 3  Real Madrid  1.15 1.86   1.19    Real Madrid  113         
    ## 4  Valencia     0.56 1.34   1.10    Valencia     113         
    ## 5  Getafe       0.55 1.10   1.33    Getafe       113         
    ## 6  Sevilla      0.43 1.37   0.98    Sevilla      113         
    ## 7  Granada      0.37 1.26   1.03    Granada       37         
    ...

y aprovecharlos para hacer diversos análisis teniéndolos en formato csv
ya exportado podemos tomarlo del link de nuestro repositorio:

``` r
Ranking.datos <- read.csv("https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/ranking20.csv")
```

Basándonos en nuestro modelo, podemos ver el máximo atacante, y el
máximo defensor

``` r
(Max.atacante <- Ranking.datos %>% filter(attack  == max(attack)))
```

    ##        team total attack defense n.games.Var1 n.games.Freq
    ## 1 Barcelona  1.51   2.23    1.28    Barcelona          113

``` r
(Max.defensor <- Ranking.datos %>% filter(defense == max(defense)))
```

    ##         team total attack defense n.games.Var1 n.games.Freq
    ## 1 Ath Madrid  1.24   1.33    1.78   Ath Madrid          113

Como también el resumen

``` r
summary(Ranking.datos)
```

    ##      team               total              attack          defense      
    ##  Length:26          Min.   :-1.43000   Min.   :0.5800   Min.   :0.6000  
    ##  Class :character   1st Qu.:-0.32500   1st Qu.:0.9675   1st Qu.:0.7875  
    ##  Mode  :character   Median : 0.00000   Median :1.0950   Median :0.9050  
    ##                     Mean   : 0.02885   Mean   :1.1600   Mean   :0.9442  
    ##                     3rd Qu.: 0.36000   3rd Qu.:1.3175   3rd Qu.:1.0225  
    ##                     Max.   : 1.51000   Max.   :2.2300   Max.   :1.7800  
    ##                                                                         
    ##       n.games.Var1  n.games.Freq   
    ##  Barcelona  : 1    Min.   : 37.00  
    ...

## 📋 Predicción Última Fecha

Para las predicciones referentes a los últimos partidos de la liga,
utilizaremos la función “predict”, que se basa en el modelo de Dixon and
Coles, el cual es una modificación del modelo de predicciones de
distribuciones de Poisson, con una modificación en cuanto a la
sobre-estimación de los datos fuera de la media.

La función requiere un objeto clase “fbRank”, ya que hace uso de los
coeficientes de la regresión lineal.

``` r
prediccion = predict.fbRanks(ranking, date = fecha[n])
```

    ## Predicted Match Results for 1900-05-01 to 2100-06-01
    ## Model based on data from 2017-08-18 to 2020-07-16
    ## ---------------------------------------------
    ## 2020-07-19 Alaves vs Barcelona, HW 9%, AW 76%, T 16%, pred score 0.7-2.5  actual: AW (0-5)
    ## 2020-07-19 Valladolid vs Betis, HW 29%, AW 43%, T 28%, pred score 1-1.3  actual: HW (2-0)
    ## 2020-07-19 Villarreal vs Eibar, HW 45%, AW 30%, T 25%, pred score 1.5-1.2  actual: HW (4-0)
    ## 2020-07-19 Ath Madrid vs Sociedad, HW 54%, AW 20%, T 26%, pred score 1.5-0.8  actual: T (1-1)
    ## 2020-07-19 Espanol vs Celta, HW 32%, AW 41%, T 27%, pred score 1.2-1.4  actual: T (0-0)
    ## 2020-07-19 Granada vs Ath Bilbao, HW 40%, AW 31%, T 29%, pred score 1.2-1  actual: HW (4-0)
    ## 2020-07-19 Leganes vs Real Madrid, HW 13%, AW 66%, T 21%, pred score 0.7-1.9  actual: T (2-2)
    ...

La función regresa una lista, con data.frames y vectores, nos
enfocaremos principalmente en su primer objeto “scores”, a diferencia de
nuestro csv, solo contiene los datos sobre los últimos partidos, dando
como información las probabilidades de victoria, derrota, empate
victoria-derrota con 0 goles del rival, ademas de incluir los
coeficientes de la regresión

``` r
View(prediccion[1])
```

Centrándonos en las probabilidades de victoria, derrota, empate para el
equipo de local como también la predicción del número de goles
redondeada:

``` r
columns <-
c(  Home.team      = "home.team"
  , Away.team      = "away.team"
  , Prob.win.home  = "home.win"
  , Prob.win.away  = "away.win"
  , Prob.tie       = "tie"
  , Pred.home.score = "pred.home.score"
  , Pred.away.score = "pred.away.score"
  )

predict.prob <-
  prediccion$scores %>% as.data.frame %>% select(all_of(columns)) %>% 
      mutate(  Pred.home.score = Pred.home.score %>% round(0)
             , Pred.away.score = Pred.away.score %>% round(0))

(predict.prob)
```

    ##       Home.team   Away.team Prob.win.home Prob.win.away Prob.tie
    ## 1131     Alaves   Barcelona         8.814        75.668   15.518
    ## 1132 Valladolid       Betis        28.672        43.066   28.262
    ## 1133 Villarreal       Eibar        44.932        29.754   25.314
    ## 1134 Ath Madrid    Sociedad        53.669        19.958   26.373
    ## 1135    Espanol       Celta        31.949        41.429   26.622
    ## 1136    Granada  Ath Bilbao        39.629        31.135   29.236
    ## 1137    Leganes Real Madrid        12.720        66.207   21.073
    ## 1138    Levante      Getafe        25.172        47.890   26.938
    ## 1139    Osasuna    Mallorca        47.992        27.228   24.780
    ...

Hacemos una comparación con los datos reales:

``` r
columns <-
c(  Home.team           = "home.team"
  , Home.score          = "home.score"
  , Pred.home.score     = "pred.home.score"
  , Away.team           = "away.team"
  , Away.score          = "away.score"
  , Pred.away.score     = "pred.away.score"
  )

   
comparacion <-
    prediccion$scores %>% as.data.frame %>% select(all_of(columns)) %>% 
      mutate(  Pred.home.score = Pred.home.score %>% round(0)
             , Pred.away.score = Pred.away.score %>% round(0))
```

Los partidos en los cuales acertó el número de goles del equipo de local

``` r
(comparacion %>% filter(Home.score == Pred.home.score))
```

    ##    Home.team Home.score Pred.home.score Away.team Away.score Pred.away.score
    ## 1 Ath Madrid          1               1  Sociedad          1               1
    ## 2    Levante          1               1    Getafe          0               1
    ## 3    Osasuna          2               2  Mallorca          2               1
    ## 4    Sevilla          1               1  Valencia          0               1

Y en donde acertó los goles del equipo visitante

``` r
(comparacion %>% filter(Away.score == Pred.away.score))
```

    ##    Home.team Home.score Pred.home.score   Away.team Away.score Pred.away.score
    ## 1 Ath Madrid          1               1    Sociedad          1               1
    ## 2    Leganes          2               1 Real Madrid          2               2

## 📋 Matriz de Confusión

Hacemos la predicción de todas las fechas de los partidos usando el
objeto ranking y obtenemos la matriz de confusión donde las clases son
el número de goles

``` r
prediccion.total.partidos = predict.fbRanks(ranking, date = fecha)
```

    ## Predicted Match Results for 1900-05-01 to 2100-06-01
    ## Model based on data from 2017-08-18 to 2020-07-16
    ## ---------------------------------------------
    ## 2017-08-18 Leganes vs Alaves, HW 33%, AW 35%, T 32%, pred score 0.9-1  actual: HW (1-0)
    ## 2017-08-18 Valencia vs Las Palmas, HW 75%, AW 8%, T 17%, pred score 2.1-0.5  actual: HW (1-0)
    ## 2017-08-19 Celta vs Sociedad, HW 32%, AW 45%, T 23%, pred score 1.4-1.7  actual: AW (2-3)
    ## 2017-08-19 Girona vs Ath Madrid, HW 14%, AW 62%, T 25%, pred score 0.6-1.6  actual: T (2-2)
    ## 2017-08-19 Sevilla vs Espanol, HW 49%, AW 24%, T 26%, pred score 1.5-0.9  actual: T (1-1)
    ## 2017-08-20 Ath Bilbao vs Getafe, HW 27%, AW 40%, T 33%, pred score 0.8-1  actual: T (0-0)
    ## 2017-08-20 Barcelona vs Betis, HW 74%, AW 11%, T 15%, pred score 2.8-1  actual: HW (2-0)
    ...

``` r
columns <-
c(  Home.team           = "home.team"
  , Home.score          = "home.score"
  , Pred.home.score     = "pred.home.score"
  , Away.team           = "away.team"
  , Away.score          = "away.score"
  , Pred.away.score     = "pred.away.score"
  )

comparacion.total.partidos <-
    prediccion.total.partidos$scores %>% as.data.frame %>% select(all_of(columns)) %>% 
      mutate(  Pred.home.score = Pred.home.score %>% round(0)
             , Pred.away.score = Pred.away.score %>% round(0))
```

``` r
library(caret)
library(lattice)
```

### Matriz de confusión de la predicción de los goles locales

``` r
confusion.m.local <-
    confusionMatrix(  factor(  comparacion.total.partidos$Pred.home.score
                             , levels = 0:max(comparacion.total.partidos$Home.score))
                    , factor(  comparacion.total.partidos$Home.score
                             , levels = 0:max(comparacion.total.partidos$Home.score))
                    , dnn = c("Prediccion", "Valores Reales"))

(confusion.m.local)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Valores Reales
    ## Prediccion   0   1   2   3   4   5   6   7   8
    ##          0   8   4   1   0   0   0   0   0   0
    ##          1 226 305 210  78  19   4   1   0   0
    ##          2  31  61  86  44  19  16   4   0   0
    ##          3   0   3   7   5   1   2   1   1   1
    ##          4   0   0   0   1   1   0   0   0   0
    ##          5   0   0   0   0   0   0   0   0   0
    ##          6   0   0   0   0   0   0   0   0   0
    ##          7   0   0   0   0   0   0   0   0   0
    ##          8   0   0   0   0   0   0   0   0   0
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.3553          
    ##                  95% CI : (0.3274, 0.3838)
    ##     No Information Rate : 0.3272          
    ##     P-Value [Acc > NIR] : 0.02395         
    ##                                           
    ##                   Kappa : 0.0686          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0 Class: 1 Class: 2 Class: 3  Class: 4 Class: 5
    ## Sensitivity          0.030189   0.8177  0.28289 0.039062 0.0250000   0.0000
    ## Specificity          0.994286   0.2986  0.79067 0.984190 0.9990909   1.0000
    ## Pos Pred Value       0.615385   0.3618  0.32950 0.238095 0.5000000      NaN
    ## Neg Pred Value       0.771961   0.7710  0.75199 0.890080 0.9657293   0.9807
    ## Prevalence           0.232456   0.3272  0.26667 0.112281 0.0350877   0.0193
    ## Detection Rate       0.007018   0.2675  0.07544 0.004386 0.0008772   0.0000
    ## Detection Prevalence 0.011404   0.7395  0.22895 0.018421 0.0017544   0.0000
    ## Balanced Accuracy    0.512237   0.5581  0.53678 0.511626 0.5120455   0.5000
    ##                      Class: 6  Class: 7  Class: 8
    ## Sensitivity          0.000000 0.0000000 0.0000000
    ## Specificity          1.000000 1.0000000 1.0000000
    ## Pos Pred Value            NaN       NaN       NaN
    ## Neg Pred Value       0.994737 0.9991228 0.9991228
    ## Prevalence           0.005263 0.0008772 0.0008772
    ## Detection Rate       0.000000 0.0000000 0.0000000
    ## Detection Prevalence 0.000000 0.0000000 0.0000000
    ## Balanced Accuracy    0.500000 0.5000000 0.5000000

Vemos que la exactitud (accuracy), que es la cantidad de predicciones
positivas que fueron correctas y que esta dada por la suma de la
diagonal entre la suma total, es de 35.53%. El modelo acertó en el
35.53% de su predicción.

Además se aprecia la relación entre los valores predecidos y los reales.

En la sensibilidad (sensitivity), que es la proporción de casos
positivos que fueron correctamente identificados, vemos que el valor más
alto es cuando predice que el equipo local anota 1 gol. El modelo acertó
en el 81.77% de las anotaciones reales que fueron de 1 gol.

Al ver la distribución de la matriz notamos que el modelo no acertó la
predicción en ningún valor de 5 a 8 goles. Ni siquiera hubo esos valores
en su predicción.

Y el “Pos Pred Value” es la proporción de predicciones correctamente
identificadas del total de predicciones para cada clase. De todos los
marcadores con gol 0 que predijo, el 61.53% fue acertado. Los demas
están por abajo del 50%.

### Matriz de confusión de la predicción de los goles del visitante

``` r
confusion.m.visit <-
    confusionMatrix(  factor(  comparacion.total.partidos$Pred.away.score
                             , levels = 0:max(comparacion.total.partidos$Away.score)
                             )
                    , factor(  comparacion.total.partidos$Away.score
                             , levels = 0:max(comparacion.total.partidos$Away.score))
                    , dnn = c("Prediccion", "Valores Reales"))

(confusion.m.visit)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Valores Reales
    ## Prediccion   0   1   2   3   4   5   6
    ##          0  12   0   1   0   0   0   0
    ##          1 336 299 155  39  10   4   0
    ##          2  49  86  81  18  19   5   3
    ##          3   4   2   5   5   3   2   0
    ##          4   0   1   0   0   1   0   0
    ##          5   0   0   0   0   0   0   0
    ##          6   0   0   0   0   0   0   0
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.3491          
    ##                  95% CI : (0.3214, 0.3776)
    ##     No Information Rate : 0.3518          
    ##     P-Value [Acc > NIR] : 0.5848          
    ##                                           
    ##                   Kappa : 0.063           
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0 Class: 1 Class: 2 Class: 3  Class: 4 Class: 5
    ## Sensitivity           0.02993   0.7706  0.33471 0.080645 0.0303030 0.000000
    ## Specificity           0.99865   0.2766  0.79955 0.985158 0.9990967 1.000000
    ## Pos Pred Value        0.92308   0.3547  0.31034 0.238095 0.5000000      NaN
    ## Neg Pred Value        0.65484   0.7003  0.81684 0.949062 0.9718805 0.990351
    ## Prevalence            0.35175   0.3404  0.21228 0.054386 0.0289474 0.009649
    ## Detection Rate        0.01053   0.2623  0.07105 0.004386 0.0008772 0.000000
    ## Detection Prevalence  0.01140   0.7395  0.22895 0.018421 0.0017544 0.000000
    ## Balanced Accuracy     0.51429   0.5236  0.56713 0.532901 0.5146998 0.500000
    ##                      Class: 6
    ## Sensitivity          0.000000
    ## Specificity          1.000000
    ## Pos Pred Value            NaN
    ## Neg Pred Value       0.997368
    ## Prevalence           0.002632
    ## Detection Rate       0.000000
    ## Detection Prevalence 0.000000
    ## Balanced Accuracy    0.500000

Vemos que la exactitud (accuracy), que es la cantidad de predicciones positivas 
que fueron correctas y que esta dada por la suma de la diagonal entre la suma
total, es de 35.53%. El modelo acertó en el 35.53% de su predicción.

Además se aprecia la relación entre los valores predecidos y los reales. 

En la sensibilidad (sensitivity), que es la proporción de casos positivos que 
fueron correctamente identificados, vemos que el valor más alto es cuando 
predice que el equipo local anota 1 gol. El modelo acertó en el 81.77% de las 
anotaciones reales que fueron de 1 gol.

Al ver la distribución de la matriz notamos  que el modelo no acertó la 
predicción en ningún valor de 5 a 8 goles. Ni siquiera hubo esos valores en su 
predicción.

Y el "Pos Pred Value" es la proporción de predicciones correctamente identificadas
del total de predicciones para cada clase. De todos los marcadores con gol cero
que predijo, el 61.53% fue acertado. Los demás están por abajo del 50%.


## 🏁 Conclusiones

En las últimas comparaciones de las predicciones hechas por la librería
fbRanks podemos notar que la predicción en cuanto el número de goles
debe tomarse con mucho cuidado, esto ya que por ejemplo en el partido
jugado por Barcelona, en el partido éste anotó 5 goles, estos casos son
muy poco probables, más si es usado el modelo de Dixon y Coles, ya que
reduce la posible sobre-estimación de estos casos, debido a su rareza,
de 10 partidos acertó totalmente en 1 partido, por lo que estos análisis
pueden servir como referencia, pero la predicción de resultados de
juegos puede ser muy compleja, por el número de factores involucrados.

En las métricas obtenidas por la matriz de confusión de la predicción
para todas las fechas con las que trabajamos obtuvimos un acierto aproximado
de 35% tanto para las predicciones de los goles de casa como visitante.
