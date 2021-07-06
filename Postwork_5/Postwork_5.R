# Ahora con ayuda de los datos de la temporada 2017/2018, 2018/2019 y 2019/2020 de la liga española
# y utilizando la biblioteca fbRanks predeciremos los resultados de la ultima fecha 
# jugada en la temporada 2019/2020

#Establecemos la ruta del directorio
setwd("D:/Omar/Programacion/Codigos/R/Bedu/dataframe")

#Exportamos la biblioteca dplyr para utilizar algunas funciones
library(dplyr)

#Creamos una lista a partir los 3 dataframes de las 3 temporadas
lista <- lapply(dir(), read.csv)

#Seleccionamos las columnas que queremos de la lista y lo guardamos en SmallData
SmallData = lapply(lista ,select, c("Date", "HomeTeam", "FTHG", "AwayTeam" ,"FTAG"))

SmallData = do.call(rbind, SmallData)

colnames(SmallData) = c("date", "home.team", "home.score", "away.team", "away.score")

SmallData$date =gsub("/17", "/2017", SmallData$date)
SmallData$date =gsub("/18", "/2018", SmallData$date)


write.csv(SmallData, file="soccer.csv", row.names = F)


library(fbRanks)

listasoccer <- create.fbRanks.dataframes(scores.file = 'soccer.csv', date.format = "%d/%m/%Y")

anotaciones = listasoccer$scores
  
equipos = listasoccer$teams
  
fecha = unique(listasoccer$scores$date)

n = length(fecha)

ranking = rank.teams(anotaciones, equipos, max.date = fecha[n-1], min.date = fecha[1], date.format = "%Y-%m-%d")

prediccion = predict.fbRanks(ranking, date = fecha[n])

originales = SmallData[SmallData$date == format(fecha[n], "%d/%m/%Y"),]

resultados =  prediccion[[1]][c("home.score","away.score")]
colnames(resultados) = c("predic.home.score", "predic.away.score")

comparacion = cbind(originales, resultados)

comparacion <- subset(comparacion, select=c("date","home.team", "home.score", 
                                            "predic.home.score", "away.team", "away.score", "predic.away.score"))
