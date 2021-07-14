#POSTWORK EQUIPO 10 
#BEDU SANTANDER UNIVERSIDADES

#Integrantes--------------------------------------------------------------------
#María Magdalena Castro Sam
#Sergio Napoleón Leal
#Jesús Omar Magaña Medina
#Fernando Itzama Novales Campos
#Adrián Ramírez Cortés
#Efraín Soto Olmos
#-------------------------------------------------------------------------------

#Éste código analiza algunos datos de la primera división de la liga española, 
#obtenidos de https://www.football-data.co.uk/spainm.php
#Más notas sobre los datos pueden encontrarse en 
#https://www.football-data.co.uk/notes.txt

#POSTWORK 1---------------------------------------------------------------------
#Importamos datos de la primera división 2019-2020
df <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

#Extraemos la columna para los goles locales
goles.local <-table(df$FTHG)

#La probabilidad marginal es la probabilidad de que ocurra un evento simple, por 
#lo tanto estará dada por el número de goles sobre el total de los partidos

#Calcularemos la probabilidad de que el equipo que juegue en casa anote x goles:

#Con table generamos una tabla que nos indica la frecuencia de los goles 
?table

(total_partidos = length(df$FTHG)) #total de los partidos

(prob.marginal = goles.local / total_partidos) #probabilidad marginal 

(table.local = data.frame(goles.local, round(prob.marginal*100, 2), check.names = T)[-3])

colnames(table.local) = c("Goles", "Frecuencia", "P. Marginal")

table.local #tabla final con las probabilidades para el equipo local

#En el caso del equipo vistante el procedimiento es análogo

goles.visitante <-table(df$FTAG) #total de los partidos

(prob.marginal = goles.visitante / total_partidos) #probabilidad marginal

table.visitante = data.frame(goles.visitante, round(prob.marginal*100,2), check.names = T)[-3]

colnames(table.visitante) = c("Goles", "Frecuencia", "P. Marginal")

table.visitante #tabla final de probabilidades para el equipo visitante

#La probabilidad conjunta toma en cuenta la probabilidad de dos eventos sobre el
#total de resultados posibles

#Calcularemos la probabilidad conjunta de que el equipo local anote x goles y el 
#visitante y goles
goles.partidos = table(df$FTHG, df$FTAG, dnn=c("x", "y"))

prob.conjunta = prop.table(goles.partidos)

prob.conjunta <- round(prob.conjunta * 100,2)

#POSTWORK 2---------------------------------------------------------------------
library(dplyr)
#Ahora agregamos aún más datos
#Utilizaremos los datos de las temporadas 2017/2018, 2018/2019 y 2019/2020

#Descargamos los datos
setwd(getwd())

liga1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
liga1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
liga1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

download.file(url = liga1718, destfile = "liga1718.csv", mode = "wb")
download.file(url = liga1819, destfile = "liga1819.csv", mode = "wb")
download.file(url = liga1920, destfile = "liga1920.csv", mode = "wb")

temporadas <- lapply(dir(pattern="csv$"), read.csv)  #guardamos los archivos en una lista

#revisamos que tipo de objeto son
str(temporadas); head(temporadas); View(temporadas); summary(temporadas)

#Seleccionamos sólo algunas columnas
temporadas <- lapply(temporadas, select, c("Date", "HomeTeam", "AwayTeam","FTHG","FTAG","FTR")) 

#Revisamos que las columnas sean del mismo tipo
data <- do.call(rbind, temporadas)

data <- mutate(data, Date = as.Date(Date, "%d/%m/%y"))

data #data frame final solo con los datos elegidos

write.csv(data, file = 'Postwork_02.csv') #guardamos el df en un archivo csv

#POSTWORK 3---------------------------------------------------------------------
library(ggplot2)
library(plotly)

#Con el data frame obtenido realizaremos algunas gráficas

#Calcularemos la probabilidad marginal de que el equipo local anote x goles 

goles.local <- table(data$FTHG) #goles locales
total_partidos <- length(data$FTHG) #total de partidos
pm.local <-(goles.local/total_partidos)#probalidad marginal 
(goles.local <- data.frame(goles.local, round(pm.local*100, 2))[-3]) 
colnames(goles.local)=c("Goles","Frecuencia", "Prob.Marginal")

goles.local #probabilidad final

#Realizamos una gráfica para vizualizar los datos
e.local <- ggplot(goles.local, aes(x = Goles, y = Prob.Marginal)) + geom_bar(stat="identity", colour='black', fill='#99CCFF') +
  ggtitle('Probabilidad de que el equipo\n local anote goles') +
  ylab('Probabilidad de ocurrencia') +
  theme_light() +
  theme(axis.text.x = element_text(size = 15)) + #tamaño de numeros en x
  theme(axis.text.y = element_text(size = 15)) + #tamaño de numeros en y
  theme(axis.title.x = element_text(size = 15)) + #tamaño del letrero en x
  theme(axis.title.y = element_text(size = 15)) + #tamaño del letrero en y
  theme(plot.title = element_text(size = 20, hjust = 0.5))    #tamaño del titulo

#######Opcional, creación de animación del grafico, necesitamos la libreria "gganimate"
library(gganimate)

(e.local.animation <- e.local + transition_states(Goles, transition_length = 10) +
  enter_grow() +
  shadow_mark())
#Lo generamos como video, para eso es necesario tener ffmpeg instalado
animate(e.local.animation, duration = 4, renderer = ffmpeg_renderer())
########

ggplotly(e.local) #versión interactiva

#Ahora calcularemos la probabilidad para el equipo visitante
goles.visitante <- table(data$FTAG)
pm.vis <- (goles.visitante/total_partidos)
goles.visitante <- data.frame(goles.visitante, round(pm.vis*100, 2))[-3]
colnames(goles.visitante)=c("Goles","Freq", "Prob.Marginal")

goles.visitante #probabilidad final

#Realizamos una gráfica para vizualizar los datos
e.visitante <- ggplot(goles.visitante, aes(x = Goles, y = pm.vis)) + geom_bar(stat="identity", colour='black', fill='#FFCC99') +
  ggtitle('Probabilidad de que el\n equipo visitante anote goles') +
  ylab('Probabilidad de que el equipo\n visitante anote goles') +
  theme_light() +
  theme(axis.text.x = element_text(size = 15)) + #tamaño de numeros en x
  theme(axis.text.y = element_text(size = 15)) + #tamaño de numeros en y
  theme(axis.title.x = element_text(size = 15)) + #tamaño del letrero en x
  theme(axis.title.y = element_text(size = 15)) + #tamaño del letrero en y
  theme(plot.title = element_text(size = 20, hjust = 0.5))    #tamaño del titulo

#####Opcional, creación de animación del grafico
library(gganimate)

(e.visitante.animation <- e.visitante + transition_states(Goles, transition_length = 10) +
    enter_grow() +
    shadow_mark())
#Lo generamos como video, para eso es necesario tener ffmpeg instalado
animate(e.visitante.animation, duration = 4, renderer = ffmpeg_renderer())
#####

ggplotly(e.visitante) #versión interactiva

#La probabilidad conjunta de que el equipo que juega en casa anote x goles y 
#el equipo que juega como visitante anote y goles 
goles.partidos = table(data$FTHG, data$FTAG, dnn=c("x", "y"))
(prob.conjunta = prop.table(goles.partidos))
(prob.conjunta <- round(prob.conjunta * 100,2))

#Realizaremos ahora un heat map para vizualizar los datos

heat.df <- as.data.frame(t(prob.conjunta)) #convertimos la matriz a df
heat.df

colnames(heat.df)=c("Visitante","Local", "Probabilidad")
heat.df

p <- ggplot(heat.df, aes(Local, Visitante, fill= Probabilidad)) + #gráficamos
  geom_raster() +
  scale_fill_viridis_c() +
  ggtitle("Probabilidad conjunta de anotación") 

p

ggplotly(p) #versión interactiva

#POSTWORK 4---------------------------------------------------------------------
#Ahora obtendremos una tabla de cocientes al dividir las probabilidades conjuntas
#por el producto de las probabilidades correspondientes

df <- read.csv("https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_02.csv")
total_partidos <- length(df$FTHG)

#Para la probabilidad marginal de los goles metidos por locales 
goles.local <-table(df$FTHG)
(prob.marginal.local = prop.table(goles.local)) #probabilidad marginal
(table.local = data.frame(goles.local, prob.marginal.local, check.names = T)[-3])
(colnames(table.local) = c("Goles", "Frecuencia", "P. Marginal")) #Renombramos las columnas

table.local

#Para los goles metidos por el equipo visitante
goles.visitante <-table(df$FTAG)
(prob.marginal.visitantes <- prop.table(goles.visitante)) #probabilidad marginal
(table.visitante = data.frame(goles.visitante, prob.marginal.visitantes, check.names = T)[-3])
(colnames(table.visitante) = c("Goles", "Frecuencia", "P. Marginal")) #Renombramos las columnas
table.visitante

#hacemos 2 tablas con las probabilidades individuales repitiendolas en las columnas
visitante <- rbind(table.visitante$`P. Marginal`) #probabilidad visitante
for (i in 1:8) {
  visitante <-rbind(visitante,table.visitante$`P. Marginal`)
}
visitante

(local <- cbind(table.local$`P. Marginal`)) #probabilidad local
for (i in 1:6) {
  local <-cbind(local,table.local$`P. Marginal`)
}
local

producto.probabilidad <- local*visitante #Realizamos el producto

(producto.probabilidad <- producto.probabilidad)
(producto <- data.frame(producto.probabilidad))

#La probabilidad conjunta 
goles.partidos = table(df$FTHG, df$FTAG, dnn=c("x", "y"))
prob.conjunta = prop.table(goles.partidos)
(prob.conjunta <- prob.conjunta)

#Realizamos el cociente
cociente <- prob.conjunta/producto
cociente

(colnames(cociente) = c(0, 1, 2, 3, 4, 5,6)) #Cambiamos los nombres
(rownames(cociente) = c(0, 1, 2, 3, 4, 5, 6,7,8))

cociente #df final del cociente

#Para determinar si el número de goles del equipo local o el de el equipo
#visitante son dependientes o independientes, realizaremos un 
#procedimiento de bootstrap para obtener más cocientes similares
#y analizar la distribución

#' Transformamos el data frame a columna para facilitar el bootstrap.
data_origin <- as.data.frame(as.vector(unlist(cociente)))

colnames(data_origin) <- "values"

#' Utilizamos la biblioteca `"rsample"` para poder hacer las muestras
#' bootstrap:
library(rsample)

#' Fijamos la semilla para poder reproducir los datos:
set.seed(83928782)

#' Aplicamos la función bootstraps, para generar 1000 muestras,
#' guardándolas en boot:
boot <- bootstraps(data_origin, times = 1000)

#' Cargamos las siguientes bibliotecas:
library(purrr)
library(modeldata)
library(viridis)
library(tidyverse)
library(hrbrthemes)
library(forcats)
library(viridisLite)

#' Realizamos una función para hacer una columna de las medias muestrales
#' obtenidas por bootstrap y aplicamos la función:
obtener_media <- function(boot_splits) {
  data_mean <- analysis(boot_splits)
  medias_muestrales <- mean(data_mean[,1])
  return(medias_muestrales)
}

boot$means <- map_dbl(boot$splits, obtener_media)
length(boot$means); summary(boot$means)

#' Observamos el valor de la media de las medias muestrales.
#' Realizamos una función para un histograma:
histograma <- function(data
                       ,data_medias
                       ,ic_2
                       ,n_bins
                       ,titulo
                       ,x_label
                       ,y_label
                       ,fill_label){
  histograma <- ggplot(data, 
                       aes(data_medias)) + 
    geom_histogram(bins = n_bins, 
                   color=NA, 
                   aes(fill=..count..)) + 
    geom_vline(aes(xintercept = mean(data_medias))) +
    geom_vline(xintercept = c(ic_2), ####
               linetype = c(2,2)) +
    ggtitle(titulo) + 
    ylab(y_label) +
    xlab(x_label) +
    scale_fill_viridis(name = fill_label) +
    theme_ipsum() +
    theme(
      #legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 10)
    )
  return(histograma)
}

#' Comprobamos la hipótesis de que la media se encuentra en 1 con las
#' medidas muestrales bootstrap y obtenemos el intervalo de confianza de
#' 95% con una prueba t:
t_boot <- t.test(boot$means, alternative = "two.sided", mu = 1)
t_boot_ic <- round(t_boot$conf.int,3)
t_boot_ic

#' Realizamos el histograma de las muestras obtenidas por bootstrap.

histograma <- function(data
                       ,data_medias
                       ,ic_2
                       ,n_bins
                       ,titulo
                       ,x_label
                       ,y_label
                       ,fill_label){
  histograma <- ggplot(data, 
                       aes(data_medias)) + 
    geom_histogram(bins = n_bins, 
                   color=NA, 
                   aes(fill=..count..)) + 
    geom_vline(aes(xintercept = mean(data_medias))) +
    geom_vline(xintercept = c(ic_2), ####
               linetype = c(2,2)) +
    ggtitle(titulo) + 
    ylab(y_label) +
    xlab(x_label) +
    scale_fill_viridis(name = fill_label) +
    theme_ipsum() +
    theme(
      #legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 10)
    )
  return(histograma)
}

ic_mean_ic <- c(t_boot_ic[1], 
                mean(boot$means), 
                t_boot_ic[2])

hist_boot <- histograma(boot, boot$means, t_boot_ic, 10, "Histograma de las medias muestrales bootstrap", "Valor de la Media","Frecuencia", "Frec")

#' ## Figura 4.1 Histograma bootstrap
ggplotly(hist_boot)

#' > La línea sólida indica la posición de la media y las punteadas, la
#' > posición de los límites del intervalo de confianza.
#' De igual modo lo hacemos para la muestra original:
t_origin <- t.test(data_origin$values, alternative = "two.sided", mu = 1)
t_origin_ic <- round(t_origin$conf.int, 3)
t_origin_ic

ori_ic_mean_ic <- c(t_origin_ic[1], 
                    mean(data_origin$values), 
                    t_origin_ic[2])

hist_origin <-  histograma(data_origin, data_origin$values, t_origin_ic, 11, "Histograma de la muestra original", "Valor de la Muestra","Frecuencia", "Frec")

#' ## Figura 4.2 Histograma original
ggplotly(hist_origin)

#' > La línea sólida indica la posición de la media y las punteadas, la
#' > posición de los límites del intervalo de confianza.
#' Vemos los datos de los estadísticos de las pruebas t para ambos conjuntos de datos.
#' Remuestreo bootstrap:
t_boot

#' Muestras originales:
t_origin

#POSTWORK 5---------------------------------------------------------------------
#Con los datos de la liga de primera división española, de las temporadas 2017/2018, 2018/2019 y 2019/2020
# haremos una predicción de los resultados de los partidos de la fecha 07/16/2020

#Utilizamos la librería dplyr para manipulaciín de datos
library(dplyr)
#Y la librería fbRanks para las predicciones en base al modelo de Dixon y Coles
library(fbRanks)

#Guardamos los datos de las 3 temporadas en una lista
lista <- lapply(dir(pattern="liga"), read.csv)

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

#POSTWORK 6---------------------------------------------------------------------
library(dplyr)

#Importaremos el conjunto de datos match.data.csv 
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-06/Postwork/match.data.csv")

head(df)
summary(df)

#Agregamos una nueva columna que contenga la suma de goles por partido y obtenemos su promedio por mes
df["sumagoles"] <- df$home.score+df$away.score

class(df$date) #Revisamos el tipo de dato de la fecha
df$date <- as.Date(df$date) #Convertimos a fecha
class(df$date) 

df$fecha <- format(df$date, format = "%Y-%m")
golesxmes <- aggregate( df$sumagoles ~ df$fecha, df , mean) #Realizamos promedio
View(golesxmes)

#Creamos la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019
golesxmes.ts <- ts(golesxmes[ ,2], start = c(2010,08), end = c(2019,12), frequency = 10)
golesxmes.ts

plot(golesxmes.ts) #Grafica de la serie de tiempo

#Notemos que la frecuencia es 10 y no 12, ya que aunque la serie de tiempo se 
#realiza tomando en cuenta los meses, las temporadas se juegan de agosto a mayo, 
#por lo tanto como no se realizan juegos durante los meses de junio y julio
#sólo se toman en cuenta los 10 meses donde se generan los goles

#POSTWORK 7---------------------------------------------------------------------
library(mongolite)
library(dplyr)

# Obtenemos el csv
match.db <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-07/Postwork/match.data.csv")

# Conectamos con la base de datos match_games y la colección match
connection <- mongo(collection = "match"
                    , db ="match_games"
                    , url = "mongodb+srv://equipo10:bedu@postwork7.8voq3.mongodb.net/test")

# Verificamos que si no hay documentos los agregue del csv
if (connection$count() == 0) {
  connection$insert(match.db)
}

# Armamnos el cuerpo de la consulta con sintaxis de mongo
query = c('{ "$or": [ 
                    {"home_team": "Real Madrid"}
                  , {"away_team": "Real Madrid"}
                  ],
             "date": "2015-12-20"
        }')

# Realizamos la consulta y find convierte el resultado de la colección a dataframe
q.result <- connection$find(query)

# Notamos que el Real Madrid solo jugó como solo como home; 
# contamos los goles y vemos quien fue el equipo contrincante
n.goles  <- q.result %>% filter(home_team == "Real Madrid") %>% pull(home_score) %>% sum()
vs.team  <- q.result %>% filter(home_team == "Real Madrid") %>% pull(away_team)

# Vemos los resultados
print(paste(  c('Cantidad de goles metidos el 20-12-2015 por el Real Madrid:'), n.goles
             ,c('contra el equipo:'), vs.team))

print("¡¡Fue una goleada!!")

# Desconectamos la conexión
connection$disconnect()


# Referencias:
# https://cran.r-project.org/web/packages/mongolite/mongolite.pdf
# https://jeroen.github.io/mongolite/query-data.html
# https://jeroen.github.io/mongolite/manipulate-data.html
