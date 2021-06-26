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

#Transformamos el data frame a columna para facilitar el bootstrap
aux = matrix(, nrow=9*7, 1)
indice = 1
for(i in 1:7){
  for(k in 1:9){
    aux[indice,1] = cociente[k,i]
    indice= indice + 1
  }
}

#Este es nuestro dataframe, con la informacion de los cocientes
dataframe = as.data.frame(aux)

# Utilizamos la libreria "rsample" para poder hacer las muestras bootstrap
library(rsample)

# Fijamos la semilla para poder reproducir los datos
set.seed(83928782)

# Aplicamos la funcion bootstraps, para generar 1000 muestras, guardandolas en boot
boot <- bootstraps(dataframe, times = 1000)

# Confirmamos la dimensión de nuestras muestras
dim(as.data.frame(boot$splits[[5]]))

# Guardamos nuestro dataframe para poder añadir las muestras para facilitar su
#analisis
data_f = dataframe

# Juntamos las columnas de nuestras muestras, para poder aplicar apply directo
for(i in 1:length(boot$splits)){
  data_f = cbind(data_f, as.data.frame(boot$splits[[i]]))
}

# Calculamos la media por cociente (por renglón)
mean_conj = apply(data_f, 1, mean)

# De forma analoga calculamos la varianza (por renglón)
var_conj = apply(data_f, 1, var)

# Juntamos en un solo dataframe las columnas, media, varianza para su análisis
analisis <- cbind(dataframe, mean_conj, var_conj)

# Utilizamos el teorema de límite central, para poder calcular la probabilidad
# de la distribución normal, ya que nuestra muestra
# contiene 1000 datos
analisis <- mutate(analisis, probabilidad = pnorm(q = V1, mean = mean_conj, sd = sqrt(var_conj/1000)))

# Hacemos los cambios en los nombres, y añadimos el numero de goles para su presentación
analisis <- cbind(c(rep(0,9), rep(1,9), rep(2,9), rep(3,9), rep(4,9), rep(5,9), rep(6,9)),analisis)
analisis <- cbind(rep(seq(0,8,1),7),analisis)
(colnames(analisis) = c("goles local", "goles visitante","Cociente", "Media", "Varianza", "Probabilidad"))

# Utilizamos la librería dplyr para el análisis
library(dplyr)

# Filtramos los valores en donde el cociente es proporcional a 1
# Ya que esto significa que la probabilidad grupal es igual a la probabilidad
# independiente
(dependiente <- filter(analisis, Cociente >= 0.97 & Cociente <= 1.04))

# Podemos apreciar que la probabilidad en todos los casos es de 1, por lo que
# en un primer vistazo, podríamos aceptarlos como independientes, pero
# tomando en cuenta la media, vemos que el valor es alejado de 1
# esto debido al procedimiento bootstrap, por el cual encontraremos
# medias similares en todos los datos, ya que los posiciona aleatoriamente en 
# todas las muestras, por lo que podemos tomar como inconcluso e insuficiente
# el análisis estadístico

