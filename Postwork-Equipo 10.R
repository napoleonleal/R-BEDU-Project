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
  ggtitle('Probabilidad de que el equipo local anote goles') +
  ylab('Probabilidad de ocurrencia') +
  theme_light()

ggplotly(e.local) #versión interactiva

#Ahora calcularemos la probabilidad para el equipo visitante
goles.visitante <- table(data$FTAG)
pm.vis <- (goles.visitante/total_partidos)
goles.visitante <- data.frame(goles.visitante, round(pm.vis*100, 2))[-3]
colnames(goles.visitante)=c("Goles","Freq", "Prob.Marginal")

goles.visitante #probabilidad final

#Realizamos una gráfica para vizualizar los datos
e.visitante <- ggplot(goles.visitante, aes(x = Goles, y = pm.vis)) + geom_bar(stat="identity", colour='black', fill='#FFCC99') +
  ggtitle('Probabilidad de que el equipo visitante anote goles') +
  ylab('Probabilidad de que el equipo visitante anote goles') +
  theme_light()

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
golesxmes.ts <- ts(golesxmes[ ,2], start = c(2010,08), end = c(2019,12), frequency = 12)
golesxmes.ts

plot(golesxmes.ts) #Grafica de la serie de tiempo
