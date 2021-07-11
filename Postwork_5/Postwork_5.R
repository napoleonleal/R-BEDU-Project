#Con los datos de la liga de primera división española, de las temporadas 2017/2018, 2018/2019 y 2019/2020
# haremos una predicción de los resultados de los partidos de la fecha 07/16/2020

#Utilizamos la librería dplyr para manipulaciín de datos
library(dplyr)
#Y la librería fbRanks para las predicciones en base al modelo de Dixon y Coles
library(fbRanks)

#Guardamos los datos de las 3 temporadas en una lista
lista <- lapply(dir(), read.csv)

#Seleccionamos unicamente los elementos "Date", "HomeTeam", "FTHG", "AwayTeam" ,"FTAG",
#los cuales son esenciales para el modelo
SmallData = lapply(lista ,select, c("Date", "HomeTeam", "FTHG", "AwayTeam" ,"FTAG"))
#Unimos las 3 temporadas en una sola
SmallData = do.call(rbind, SmallData)
#Cambiamos los nombres de las columnas por requerimiento de la libreria
colnames(SmallData) = c("date", "home.team", "home.score", "away.team", "away.score")

#Cambiamos la forma del año, para poder aplicar un único formato date
SmallData$date =gsub("/17", "/2017", SmallData$date)
SmallData$date =gsub("/18", "/2018", SmallData$date)
#Guardamos los cambios en un archivo csv
write.csv(SmallData, file="soccer.csv", row.names = F)

#Aplicamos la primera función de la biblioteca, "create.fbranks.dataframes" con la finalidad
#de poder hacer una limpieza de datos, excluyendo los datos nulos para las puntuaciones,
#así como nombres repetidos, los cambios incluyen la transformación del formato de columna
#"date"
listasoccer <- create.fbRanks.dataframes(scores.file = 'soccer.csv', date.format = "%d/%m/%Y")
#Como residuo, la función nos devuelve una lista con un data.frame scores con los datos de
#nuestro csv limpios y las fechas en orden ascendente, como también el data frame teams,
#en este caso generado del csv, con los nombres de los equipos sin repetir, el data.frame
#teams.resolver y por último el data.frame raw.scores, con algunas configuraciones para el
#uso de otras funciones.
View(listasoccer)


#Guardamos el data.frame scores generados, con las puntuaciones en las temporadas
anotaciones = listasoccer$scores
#Guardamos la lista de equipos
equipos = listasoccer$teams
#Guardamos las fechas sin repetir
fecha = unique(listasoccer$scores$date)
#Y la cantidad de las fechas para un mejor control
n = length(fecha)

#Aplicamos la función "rank.teams", el cual aplica una regresión lineal usando como modelo
#la distribución de Poisson, tomando como rango de tiempo la duración de un partido.
#La función requiere los datos scores, la lista de equipos y las fechas entre las que
#generamos nuestro ranking
ranking = rank.teams(anotaciones, equipos, max.date = fecha[n-1], min.date = fecha[1], date.format = "%Y-%m-%d")

#Como resultado nos da una lista con las especificaciones que le dimos a nuestra función
#Y nos presenta una tabla ranking, con los coeficientes de la regresión, tanto de ataque y
#defensa, modificados para su mejor comprensión, y uniéndolos en un total, posicionando
#a los equipos en base al total.
(ranking)
#La función da como resultado una clase única de la librería "fbRanks" necesaria para el
#uso de otras funciones
class(ranking)
#En la lista "ranking" es posible encontrar los coeficientes de la regresión en crudo,
#como sus especificaciones
ranking[1]
View(ranking[1])

#Podemos aprovechar los datos de la función rank para hacer diversos análisis, teniéndolos
#en formato csv
Ranking.datos <- read.csv("https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/ranking20.csv")

#Basándonos en nuestro modelo, podemos ver el máximo atacante, y el máximo defensor
(Max.atacante <- filter(Ranking.datos, Ranking.datos$attack == max(Ranking.datos$attack)))
(Max.defensor <- filter(Ranking.datos, Ranking.datos$defense == max(Ranking.datos$defense)))
#Como también el resumen
summary(Ranking.datos)


#Para las predicciones referentes a los últimos partidos de la liga, utilizaremos la función
#"predict", que se basa en el modelo de Dixon and Coles, el cual es una modificación del modelo
#de predicciones de distribuciones de Poisson, con una modificación en cuanto a la sobre-estimación
#de los datos fuera de la media.
#La función requiere un objeto clase "fbRank", ya que hace uso de los coeficientes de la regresión
#lineal
prediccion = predict.fbRanks(ranking, date = fecha[n])


#La función regresa una lista, con data.frames y vectores, nos enfocaremos principalmente
#en su primer objeto "scores", a diferencia de nuestro csv, solo contiene los datos sobre
#los últimos partidos, dando como información las probabilidades de victoria, derrota, empate
#victoria-derrota con 0 goles del rival, ademas de incluir los coeficientes de la regresión
View(prediccion[1])

var.prediccion <- as.data.frame(prediccion[1])

#Centrándonos en las probabilidades de victoria, derrota, empate para el equipo de local como
#también la predicción del número de goles
probabilidades.predicciones <- select(var.prediccion, c("scores.home.team",
        "scores.away.team", "scores.home.win", "scores.away.win", "scores.tie",
        "scores.pred.home.score", "scores.pred.away.score"))
colnames(probabilidades.predicciones) = c("Home.team", "Away.team", "Prob.win", 
    "Prob.lose", "Prob.tie", "Pred.hometeam.score", "Pred.awayteam.score")
(probabilidades.predicciones <- mutate(probabilidades.predicciones,
    "Pred.hometeam.score" = round(Pred.hometeam.score,0),
    "Pred.awayteam.score" = round(Pred.awayteam.score,0)))


#Hacemos una comparación con los datos reales
comparacion <- select(var.prediccion, "scores.home.team", "scores.home.score",
    "scores.pred.home.score", "scores.away.team", "scores.away.score",
    "scores.pred.away.score")
(comparacion <- mutate(comparacion, "scores.pred.home.score" =round(scores.pred.home.score,0),
    "scores.pred.away.score" = round(scores.pred.away.score,0)))
#Los partidos en los cuales acertó el número de goles del equipo de local
(filter(comparacion, scores.home.score == scores.pred.home.score))
#Y en donde acertó los goles del equipo visitante
(filter(comparacion, scores.away.score == scores.pred.away.score))

#En las últimas comparaciones de las predicciones hechas por la librería fbRanks podemos
#notar que la predicción en cuanto el número de goles debe tomarse con mucho cuidado,
#esto ya que por ejemplo en el partido jugado por Barcelona, en el partido este anoto 5
#goles, estos casos son muy poco probables, más si es usado el modelo de Dixon y Coles,
#ya que reduce la posible sobre-estimación de estos casos, debido a su rareza, de 10
#partidos acertó totalmente en 1 partido, por lo que estos análisis pueden servir como
#referencia, pero la predicción de resultados de juegos puede ser muy compleja, por el
#número de factores involucrados

