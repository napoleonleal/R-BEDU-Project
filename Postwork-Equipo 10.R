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
setwd("~/Documents/BEDU/R/Postwork")

liga1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
liga1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
liga1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

download.file(url = liga1718, destfile = "liga1718.csv", mode = "wb")
download.file(url = liga1819, destfile = "liga1819.csv", mode = "wb")
download.file(url = liga1920, destfile = "liga1920.csv", mode = "wb")

temporadas <- lapply(dir(), read.csv)  #guardamos los archivos en una lista

#revisamos que tipo de objeto son
str(temporadas); head(temporadas); View(temporadas); summary(temporadas)

#Seleccionamos sólo algunas columnas
temporadas <- lapply(temporadas, select, c("Date", "HomeTeam", "AwayTeam","FTHG","FTAG","FTR")) 

#Revisamos que las columnas sean del mismo tipo
data <- do.call(rbind, temporadas)

data <- mutate(data, Date = as.Date(Date, "%d/%m/%y"))

data #data frame final solo con los datos elegidos

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
ggplot(goles.local, aes(x = Goles, y = pm.local)) + geom_bar(stat="identity")

#Ahora calcularemos la probabilidad para el equipo visitante
goles.visitante <- table(data$FTAG)
pm.vis <- (goles.visitante/total_partidos)
goles.visitante <- data.frame(goles.visitante, round(pm.vis*100, 2))[-3]
colnames(goles.visitante)=c("Goles","Freq", "Prob.Marginal")

goles.visitante #probabilidad final

#Realizamos una gráfica para vizualizar los datos
ggplot(goles.visitante, aes(x = Goles, y = Pmg)) + geom_bar(stat="identity")

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

