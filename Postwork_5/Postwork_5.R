# Ahora con ayuda de los datos de la temporada 2017/2018, 2018/2019 y 2019/2020 de la liga española
# y utilizando la biblioteca fbRanks predeciremos los resultados de la ultima fecha 
# jugada en la temporada 2019/2020


# Importamos la biblioteca dplyr para utilizar algunas funciones y
# la biblioteca fbRanks para calcular los resultados
library(dplyr)
library(fbRanks)

# Creamos una lista a partir los 3 dataframes de las 3 temporadas
lista <- lapply(dir(), read.csv)

# Seleccionamos las columnas que queremos de la lista y lo guardamos en SmallData,
# juntamos todo en un solo dataframe con ayuda de do.call
# y renombramos las columnas
SmallData = lapply(lista ,select, c("Date", "HomeTeam", "FTHG", "AwayTeam" ,"FTAG"))
SmallData = do.call(rbind, SmallData)
colnames(SmallData) = c("date", "home.team", "home.score", "away.team", "away.score")

# Cambiamos el formato de fecha de algunos registros, esto con la finalidad de manejar
# de manera más eficiente la información. Al final guardamos el data frame como soccer.csv
SmallData$date =gsub("/17", "/2017", SmallData$date)
SmallData$date =gsub("/18", "/2018", SmallData$date)
write.csv(SmallData, file="soccer.csv", row.names = F)

# Ya teniendo el soccer.csv, lo leemos con la funcion create.fbRanks.dataframes y le decimos
# por parametro que es el archivo de puntuaciones y en que formato estan las fechas
listasoccer <- create.fbRanks.dataframes(scores.file = 'soccer.csv', date.format = "%d/%m/%Y")

# Creamos 4 variables que son de utlidad para crear un ranking de los partidos

# Guarda las anotaciones de cada partidos
anotaciones = listasoccer$scores

# Guarda los equipos que jugaron
equipos = listasoccer$teams

# Guarda todas las fechas sin repetir que se jugaron en las 3 temporadas.
fecha = unique(listasoccer$scores$date)

# Guarda la cantidad de fechas que se jugaron.
n = length(fecha)

# Con ayuda de rank.team creamos rangos usando un marco de datos de registros de coincidencia, esto nos regrsara una tabla
# con los datos de cada equipo, como su puntaje de ataque, defensa, etc.
ranking = rank.teams(anotaciones, equipos, max.date = fecha[n-1], min.date = fecha[1], date.format = "%Y-%m-%d")

# Ya teniendo ranking hacemos una prediccion de los resultados para la ultima feche de la temporada, esto lo hacemos con
# predict.fbRanks, nos regresara una prediccion de los resultados para la ultima fecha.
prediccion = predict.fbRanks(ranking, date = fecha[n])

# Comparamos los resultados con el data frame original y lo comparamos con la prediccion
originales = SmallData[SmallData$date == format(fecha[n], "%d/%m/%Y"),]

resultados =  prediccion[[1]][c("home.score","away.score")]
colnames(resultados) = c("predic.home.score", "predic.away.score")

comparacion = cbind(originales, resultados)

subset(comparacion, select=c("date","home.team", "home.score", 
                                            "predic.home.score", "away.team", "away.score", "predic.away.score"))


# Como vimos si tenemos los datos sufiicientes de algun evento en este caso los resultados de partidos
# de la liga española, podemos hacer una prediccion de los resultados de alguna fecha o partido.
# Con ayuda de la biblioteca fbtanks vimos lo sencillo que puede ser utlizar R como herramienta para predecir
# los resultados de un partido.
