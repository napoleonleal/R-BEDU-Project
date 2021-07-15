# Postwork 1
# Éstos códigos analizan los datos de la temporada 2019/2020 de 
# la primera división de la liga española
#  los datos fueron obtenidos de https://www.football-data.co.uk/spainm.php
# Informacion sobre los datos puede encontrarse en https://www.football-data.co.uk/notes.txt

# Importamos las bibliotecas con las que trabajaremos:
library(dplyr)

# Importamos datos de la primera división 2019-2020:
data <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

# Vemos la ayuda del comando table:
?table

# Declaramos una funcion que nos ayude a calcular la frecuencia de los goles
get.freq <- function(data, team, name.freq){
  team <- enquo(team)
  data %>%
    pull(!!team) %>%
    table(., dnn = name.freq) 
}

## Tablas de Frecuencias

# Extraemos las columnas, obtenemos la frecuencia marginal para el equipo
# de casa y visitante en una función. con table generamos una tabla que
# nos indica la frecuencia de los goles:

### Equipo Local
freq.m.local <- 
  get.freq(data, FTHG, "Freq.Home")

(freq.m.local)

### Equipo visitante
freq.m.visit <- 
  get.freq(data, FTAG, "Freq.Away")

(freq.m.visit)

## Probabilidad Marginal

# Declaramos una funcion que nos calcula la probabilidad marginal
get.prob.m.tbl <- function(data, team, name.prob){
  team <- enquo(team)
  data %>%
    pull(!!team) %>%
    table %>%
    prop.table -> prob.tbl
  
  names(attributes(prob.tbl)$dimnames) <- name.prob
  return(prob.tbl)
}

# La probabilidad marginal es la probabilidad de que ocurra un evento
# simple, por lo tanto estará dada por el número de goles sobre el total
# de los partidos.
# 
# Calcularemos la probabilidad de que el equipo que juegue en casa anote
# ’*x’* goles en una función:
# 
# ### Equipo Local
# 
# La probabilidad marginal de que el equipo que juega local anote x goles
# (x = 0, 1, 2, …)

prob.m.local <- 
  get.prob.m.tbl(data, FTHG, "Prob.Home")

(prob.m.local %>% round(3))

### Equipo Visitante
# La probabilidad marginal de que el equipo que juega como visitante anote
# y goles (y = 0, 1, 2, …)
# 
# En el caso del equipo vistante el procedimiento es análogo.
prob.m.visit <- 
  get.prob.m.tbl(data, FTAG, "Prob.Away")

(prob.m.visit %>% round(3))

## Frecuencias y Probabilidad Marginal

# Declaramos una funcion para obtener las probabilidades
get.prob.df<- function(data, team, name.gol, name.prob){ 
  team      <- enquo(team)
  data %>% 
    pull(!!team) %>% 
    table(., dnn = (name.gol)) %>% 
    as.data.frame %>% 
    mutate(!!name.prob := Freq/sum(Freq)) 
}

# Declaramos una funcion para redondear el resultado
get.round <- function(data, digits){
  data %>% mutate_if(is.numeric, round, digits=digits)
}

# Obtenemos un dataframe con las frecuencias y probabilidades con una
# función y redondeamos:

### Equipo Local
data.local <- get.prob.df(data, FTHG, "Gol.Home", "Prob.Marginal")

(data.local %>% get.round(3))

### Equipo Visitante
data.visit <- get.prob.df(data, FTAG, "Gol.Away", "Prob.Marginal" )

(data.visit %>% get.round(3))


## Probabilidad Conjunta P(x∩y)

# Declaramos funciones para calcular la probabilidad conjunta
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

# La probabilidad conjunta toma en cuenta la probabilidad de dos eventos sobre el total de resultados posibles.
# 
# Calcularemos la probabilidad conjunta de que el equipo local anote ‘x’ goles y el visitante’y’ 
# goles (x = 0, 1, 2, …, y = 0, 1, 2, …) con una función:
table.prob.joint <-
  get.prob.joint.tbl(data, data$FTHG, data$FTAG, "x (Home)", "y (Away)")

(table.prob.joint %>% round(3))

