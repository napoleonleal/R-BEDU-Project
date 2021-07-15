# 📂 POSTWORK 2

Importamos bibliotecas:

``` r
library(dplyr)
library(magrittr)
```

Ahora agregamos aún más datos. Utilizaremos los datos de las temporadas
2017/2018, 2018/2019 y 2019/2020.

## 📋 Importar datos

Importamos los datos a una lista:

``` r
temporadas <- c( SP1.1718 = "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
               , SP1.1819 = "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
               , SP1.1920 = "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
               ) %>% lapply(read.csv)
```

## 📋 Revisión de estructura de los datos

Revisamos su estructura:

> Solo se muestran las primeras 6 líneas de la consola

``` r
get.info <- function(data){
  data %>% str
  data %>% head
  data %>% summary
  data %>% View
}
```

``` r
temporadas["SP1.1718"] %>% get.info
```

    ## List of 1
    ##  $ SP1.1718:'data.frame':    380 obs. of  64 variables:
    ##   ..$ Div       : chr [1:380] "SP1" "SP1" "SP1" "SP1" ...
    ##   ..$ Date      : chr [1:380] "18/08/17" "18/08/17" "19/08/17" "19/08/17" ...
    ##   ..$ HomeTeam  : chr [1:380] "Leganes" "Valencia" "Celta" "Girona" ...
    ##   ..$ AwayTeam  : chr [1:380] "Alaves" "Las Palmas" "Sociedad" "Ath Madrid" ...
    ...

``` r
temporadas["SP1.1819"] %>% get.info
```

    ## List of 1
    ##  $ SP1.1819:'data.frame':    380 obs. of  61 variables:
    ##   ..$ Div       : chr [1:380] "SP1" "SP1" "SP1" "SP1" ...
    ##   ..$ Date      : chr [1:380] "17/08/2018" "17/08/2018" "18/08/2018" "18/08/2018" ...
    ##   ..$ HomeTeam  : chr [1:380] "Betis" "Girona" "Barcelona" "Celta" ...
    ##   ..$ AwayTeam  : chr [1:380] "Levante" "Valladolid" "Alaves" "Espanol" ...
    ...

``` r
temporadas["SP1.1920"] %>% get.info
```

    ## List of 1
    ##  $ SP1.1920:'data.frame':    380 obs. of  105 variables:
    ##   ..$ Div        : chr [1:380] "SP1" "SP1" "SP1" "SP1" ...
    ##   ..$ Date       : chr [1:380] "16/08/2019" "17/08/2019" "17/08/2019" "17/08/2019" ...
    ##   ..$ Time       : chr [1:380] "20:00" "16:00" "18:00" "19:00" ...
    ##   ..$ HomeTeam   : chr [1:380] "Ath Bilbao" "Celta" "Valencia" "Mallorca" ...
    ...

Vemos que hay un diferente formato de fechas en la temporada 17/18.

## 📋 Selección de columnas

Seleccionamos sólo las columnas de interés:

``` r
columns <- c(  "Date"
             , "HomeTeam" 
             , "AwayTeam"
             , "FTHG"
             , "FTAG"
             , "FTR"
             )

temporadas %<>% lapply(select, all_of(columns)) 
```

## 📋 Corrección y Unión de datos

Revisamos que las columnas sean del mismo tipo, corregimos el error de
formato y tipo de dato de la columna `Date` y unimos en un solo data
frame:

``` r
data <- temporadas %>% unname %>% do.call(rbind, .)
# Corrección del formato de fecha usando una expresión regular
data %<>% mutate(Date = gsub("/(1[78])$", "/20\\1", Date))  
# Correccioón del tipo de dato
data %<>% mutate(Date = as.Date(Date, "%d/%m/%Y"))

head(data$Date)
```

    ## [1] "2017-08-18" "2017-08-18" "2017-08-19" "2017-08-19" "2017-08-19"
    ## [6] "2017-08-20"

``` r
#data frame final solo con los datos elegidos
(dim(data))
```

    ## [1] 1140    6

## 📋 Escritura de archivo corregido

Guardamos el data frame obtenido en formato csv en una carpeta llamada
“equipo10”:

``` r
w.dir   <- getwd()
sub.dir <- "equipo10"
path    <- file.path(w.dir, sub.dir)
dir.create(path, showWarnings = F, recursive = T)
setwd(path)

#guardamos el df en un archivo csv
write.csv(data, file = 'Postwork_02.csv', row.names = FALSE)
setwd(w.dir)
```
