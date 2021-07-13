# Postworks seccion 6
# Objetivo : - Aprender a crear una serie de tiempo en R


#Instalacion del paquete "dplyr" para la manipulacion de dataframes.
install.packages("dplyr")
#Instalacion del paquete "lubridate" para la manipulacion de dataframes.
install.packages("lubridate")

#Uso de las librerias descargados
library(dplyr)
library(lubridate)

# 1. Agrega una nueva columna sumagoles que contenga la suma de goles por partido

#Cargando documento .csv
#Observaciones:
#Se importo desde el archivo raw del repositorio de github
data <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-06/Postwork/match.data.csv")

#Analisis exploratorio del df
head(data)
names(data)
summary(data)

#Agregando una nueva columna al data frame con los goles por partido
#El resultado sera la suma de goles del equipo visitante más los del equipo local
data$sumagoles <- data$home.score + data$away.score

# 2. Obtén el promedio por mes de la suma de goles

##Agregando una nueva columna con el numero de mes y el anio en un string
#Se concateno los resultados con la funcion "paste"
data$fecha <- paste(as.character(year(data$date)),as.character(month(data$date)),sep="-")

#Agregando una nueva columna con el numero de mes
data$mes <- as.numeric(month(data$date))
#Agregando una nueva columna con el numero del año
data$anio <- as.numeric(year(data$date))

#Muestra de cambios en el DataFrame
head(data)
tail(data)
summary(data)

#Procedimiento elaborado para agrupar por (anio y mes) y agregarle la media
#Se uso "%>%" para realizar esta operacion
df <- data %>% 
  group_by(anio,mes) %>%
  summarize(promedio_goles=mean(sumagoles))

# 3. Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019

#Observacion de resultados
head(df)

#Ordenando de los campos anio y mes
df1 <- df[order(df$anio,df$mes),]

#Observacion de resultados
head(df1)

#Serie de tiempo
goles.ts <- ts(df1[,3],start = c(2010,8), frequency = 12,end = c(2019,12))

# 4. Grafica la seria de tiempo.

#Graficando la serie de tiempo 
plot(goles.ts, xlab = "", ylab = "")
title(main = "Serie de Goles",
      ylab = "Suma de goles por partido",
      xlab = "Mes")
