### Postwork 5
# Con los datos de la liga de primera división española, de las temporadas 
# 2017/2018, 2018/2019 y 2019/2020 haremos una predicción de los resultados 
# de los partidos de la fecha 07/16/2020.

# Utilizamos la librería dplyr para manipulaciín de datos:
library(dplyr)

# Y la librería fbRanks para las predicciones en base al modelo de Dixon y Coles:
library(fbRanks)

### Escribir soccer.csv
# Guardamos los datos de las 3 temporadas en una lista. Seleccionamos unicamente 
# los elementos “Date”, “HomeTeam”, “FTHG”, “AwayTeam” ,“FTAG”, los cuales son esenciales 
# para el modelo y cambiamos los nombres de las columnas por requerimiento de la biblioteca. 
# Unimos las 3 temporadas en una sola y Cambiamos la forma del año, para poder aplicar un único formato date:
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

# Guardamos los cambios en un archivo csv:
w.dir   <- getwd()
sub.dir <- "equipo10"
path    <- file.path(w.dir, sub.dir)
dir.create(path, showWarnings = F, recursive = T)
setwd(path)

write.csv(SmallData, file = './soccer.csv', row.names = F)
setwd(w.dir)

### fbRanks: anotaciones y equipos
# Aplicamos la primera función de la biblioteca, “create.fbranks.dataframes” con la 
# finalidad de poder hacer una limpieza de datos, excluyendo los datos nulos para las 
# puntuaciones, así como nombres repetidos, los cambios incluyen la transformación del formato de columna “date”:
listasoccer <- create.fbRanks.dataframes(scores.file = "soccer.csv")

(listasoccer)

# Como residuo, la función nos devuelve una lista con un data.frame scores con los datos de nuestro csv 
# limpios y las fechas en orden ascendente, como también el data frame teams, en este caso generado del csv, 
# con los nombres de los equipos sin repetir, el data.frame teams.resolver y por último el data.frame raw.scores, 
# con algunas configuraciones para el uso de otras funciones.
View(listasoccer)

# Guardamos el data.frame scores generados, con las puntuaciones en las temporadas
anotaciones = listasoccer$scores

# Guardamos la lista de equipos
equipos = listasoccer$teams

### Ranking de Equipos
# Guardamos las fechas sin repetir
fecha = unique(listasoccer$scores$date)

# Y la cantidad de las fechas para un mejor control
n = length(fecha)

# Aplicamos la función “rank.teams”, el cual aplica una regresión lineal usando como 
# modelo la distribución de Poisson, tomando como rango de tiempo la duración de un partido.

# La función requiere los datos scores, la lista de equipos y las fechas entre las que generamos nuestro ranking
ranking = rank.teams(  anotaciones
                     , equipos
                     , max.date = fecha[n-1]
                     , min.date = fecha[1]
                     , date.format = "%Y-%m-%d")

# Como resultado nos da una lista con las especificaciones que le dimos a nuestra función Y nos presenta una tabla ranking, 
# con los coeficientes de la regresión, tanto de ataque y defensa, modificados para su mejor comprensión, y uniéndolos en un total, 
# posicionando a los equipos con base en el total.
(ranking)

# La función da como resultado una clase única de la librería “fbRanks” necesaria para el uso de otras funciones
class(ranking)

# En la lista “ranking” es posible encontrar los coeficientes de la regresión en crudo, como sus especificaciones
ranking[1]
View(ranking[1])

# Podemos extraer los datos de la función rank
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

# y aprovecharlos para hacer diversos análisis teniéndolos en formato csv ya exportado podemos tomarlo del link de nuestro repositorio
Ranking.datos <- read.csv("https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/ranking20.csv")

# Basándonos en nuestro modelo, podemos ver el máximo atacante, y el máximo defensor
(Max.atacante <- Ranking.datos %>% filter(attack  == max(attack)))
(Max.defensor <- Ranking.datos %>% filter(defense == max(defense)))

# Como también el resumen
summary(Ranking.datos)

### Predicción Última Fecha
# Para las predicciones referentes a los últimos partidos de la liga, utilizaremos la función “predict”, 
# que se basa en el modelo de Dixon and Coles, el cual es una modificación del modelo de predicciones de distribuciones de Poisson, 
# con una modificación en cuanto a la sobre-estimación de los datos fuera de la media.

# La función requiere un objeto clase “fbRank”, ya que hace uso de los coeficientes de la regresión lineal.
prediccion = predict.fbRanks(ranking, date = fecha[n])

# La función regresa una lista, con data.frames y vectores, nos enfocaremos principalmente en su primer objeto “scores”, 
# a diferencia de nuestro csv, solo contiene los datos sobre los últimos partidos, dando como información las probabilidades de victoria, 
# derrota, empate victoria-derrota con 0 goles del rival, ademas de incluir los coeficientes de la regresión
View(prediccion[1])

# Centrándonos en las probabilidades de victoria, derrota, empate para el equipo de local 
# como también la predicción del número de goles redondeada:
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

# Hacemos una comparación con los datos reales:
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

# Los partidos en los cuales acertó el número de goles del equipo de local
(comparacion %>% filter(Home.score == Pred.home.score))

# Y en donde acertó los goles del equipo visitante
(comparacion %>% filter(Away.score == Pred.away.score))

### Matriz de Confusión
# Hacemos la predicción de todas las fechas de los partidos usando el objeto ranking 
# y obtenemos la matriz de confusión donde las clases son el número de goles
prediccion.total.partidos = predict.fbRanks(ranking, date = fecha)

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

library(caret)
library(lattice)

### Matriz de confusión de la predicción de los goles locales
confusion.m.local <-
    confusionMatrix(  factor(  comparacion.total.partidos$Pred.home.score
                             , levels = 0:max(comparacion.total.partidos$Home.score))
                    , factor(  comparacion.total.partidos$Home.score
                             , levels = 0:max(comparacion.total.partidos$Home.score))
                    , dnn = c("Prediccion", "Valores Reales"))

(confusion.m.local)

# Vemos que la exactitud (accuracy), que es la cantidad de predicciones positivas que fueron correctas 
# y que esta dada por la suma de la diagonal entre la suma total, es de 35.53%. El modelo acertó en el 35.53% de su predicción.
# 
# Además se aprecia la relación entre los valores predecidos y los reales.
# 
# En la sensibilidad (sensitivity), que es la proporción de casos positivos que fueron correctamente identificados, 
# vemos que el valor más alto es cuando predice que el equipo local anota 1 gol. El modelo acertó en el 81.77% de las anotaciones 
# reales que fueron de 1 gol.
# 
# Al ver la distribución de la matriz notamos que el modelo no acertó la predicción en ningún valor de 5 a 8 goles. Ni siquiera 
# hubo esos valores en su predicción.
# 
# Y el “Pos Pred Value” es la proporción de predicciones correctamente identificadas del total de predicciones para cada clase. 
# De todos los marcadores con gol 0 que predijo, el 61.53% fue acertado. Los demas están por abajo del 50%.

### Matriz de confusión de la predicción de los goles del visitante
confusion.m.visit <-
    confusionMatrix(  factor(  comparacion.total.partidos$Pred.away.score
                             , levels = 0:max(comparacion.total.partidos$Away.score)
                             )
                    , factor(  comparacion.total.partidos$Away.score
                             , levels = 0:max(comparacion.total.partidos$Away.score))
                    , dnn = c("Prediccion", "Valores Reales"))

(confusion.m.visit)


# Vemos que la exactitud (accuracy) es de 34.91%. El modelo acertó en el 34.91% de su predicción.
# 
# En la sensibilidad (sensitivity) vemos que el valor más alto es cuando predice que el equipo visitante anota 1 gol. 
# El modelo acertó en el 77.06% de las anotaciones reales que fueron de 1 gol.
# 
# Al ver la distribución de la matriz notamos que el modelo no acertó la predicción en ningún valor de 5 y 6 goles. 
# Ni siquiera hubo esos valores en su predicción.
# 
# Y en el “Pos Pred Value” vemos que de todos los marcadores con gol 0 que predijo, el 92.30% fue acertado. 
# Los demas están por en el 50% o debajo.

### Conclusiones
# En las últimas comparaciones de las predicciones hechas por la librería fbRanks podemos notar que la predicción en cuanto 
# el número de goles debe tomarse con mucho cuidado, esto ya que por ejemplo en el partido jugado por Barcelona, en el partido 
# éste anotó 5 goles, estos casos son muy poco probables, más si es usado el modelo de Dixon y Coles, ya que reduce la posible 
# sobre-estimación de estos casos, debido a su rareza, de 10 partidos acertó totalmente en 1 partido, por lo que estos análisis 
pueden servir como referencia, pero la predicción de resultados de juegos puede ser muy compleja, por el número de factores involucrados.
