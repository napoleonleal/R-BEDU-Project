# 📂 POSTWORK 7

``` r
library(mongolite)
library(dplyr)
```

Importamos el csv

``` r
match.db <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-07/Postwork/match.data.csv")
```

## 📋 Alojar match.data.csv a match\_games

Conectamos el cluster con la base de datos “match\_games” y la colección
“match”

``` r
connection <- mongo(  collection = "match"
                    , db  = "match_games"
                    , url = "mongodb+srv://equipo10:bedu@postwork7.8voq3.mongodb.net/test")
```

Verificamos que si no hay documentos agregue los datos del csv

``` r
if (connection$count() == 0) {
  connection$insert(match.db)
}
```

## 📋 Número de Registros

Consultamos el número de registros

``` r
connection$count()
```

    ## [1] 3800

## 📋 Consulta Sintaxis Mongodb

Armamos el cuerpo de la consulta con sintaxis de mongodb

``` r
query = c('{ "$or": [ 
                    {"home_team": "Real Madrid"}
                  , {"away_team": "Real Madrid"}
                  ],
             "date": "2015-12-20"
        }')
```

Realizamos la consulta y ‘find’ convierte el resultado de la colección a
dataframe

``` r
q.response <- connection$find(query)
```

Notamos que el Real Madrid solo jugó como local; contamos los goles y
vemos quien fue el equipo contrincante

``` r
n.goles  <- 
    q.response %>% 
    filter(home_team == "Real Madrid") %>% 
    pull(home_score) %>% 
    sum()

vs.team  <-
    q.response %>% 
    filter(home_team == "Real Madrid") %>%
    pull(away_team)
```

Vemos los resultados

``` r
cat(paste( "Cantidad de goles metidos el 20-12-2015 por el Real Madrid: "
          , n.goles , "\n"
          , "Contra el equipo: "
          , vs.team
          , sep = ""))
```

    ## Cantidad de goles metidos el 20-12-2015 por el Real Madrid: 10
    ## Contra el equipo: Vallecano

``` r
if (n.goles > 4){cat(" ¡¡Fue una goleada!! ")}
```

    ##  ¡¡Fue una goleada!!

## 📋 Cerrar Conexión

Desconectamos la conexión

``` r
connection$disconnect()
```

Referencias:

<https://cran.r-project.org/web/packages/mongolite/mongolite.pdf>

<https://jeroen.github.io/mongolite/query-data.html>

<https://jeroen.github.io/mongolite/manipulate-data.html>
