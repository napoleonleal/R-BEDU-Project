library(dplyr)

####################################################################################
# 1     Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 
#       de la primera división de la liga española a R, los datos los puedes encontrar 
#       en el siguiente enlace: https://www.football-data.co.uk/spainm.php

# Creamos una lista SP1 con la informacion de cada archivo CSV
SP1 <- list(SP1.1718 = read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")
          , SP1.1819 = read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
          , SP1.1920 = read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
       )

####################################################################################
# 2     Revisa la estructura de de los data frames al usar las 
#       funciones: str, head, View y summary

# Inspeccionamos los datos
# Encontramos un formato de fecha distinto en un CSV ("SP1.1718")
lapply(SP1, summary)
lapply(SP1, head)
lapply(SP1, View)
lapply(SP1, str)

####################################################################################
# 3     Con la función select del paquete dplyr selecciona únicamente las columnas 
#       Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de 
#       los data frames. (Hint: también puedes usar lapply).
  
# Hacemos un vector con los nombres de las columnas de interes
columns <- c(
  "Date"
  ,"HomeTeam" 
  ,"AwayTeam"
  ,"FTHG"
  ,"FTAG"
  ,"FTR"
)

# Extraemos las columnas de interes
SP1 = lapply(SP1, select, all_of(columns)) # Warning sin el all_of()

####################################################################################
# 4     Asegúrate de que los elementos de las columnas correspondientes de los 
#       nuevos data frames sean del mismo tipo (Hint 1: usa as.Date y mutate 
#       para arreglar las fechas). Con ayuda de la función rbind forma 
#       un único data frame que contenga las seis columnas mencionadas en el punto 3 
#       (Hint 2: la función do.call podría ser utilizada).

# Revisamos los tipos de datos
lapply(SP1, str)

# Cambiamos un formato de fecha distinto en un CSV usando regex
csv.name = "SP1.1718"
SP1[[csv.name]]$Date <- gsub("([0-3][0-9])/([0-1][0-9])/([0-3][0-9])$"
      , "\\1/\\2/20\\3"
      , SP1[[csv.name]]$Date)

# Convertimos las columnas de nombre "Date" en formato Date
SP1 <- lapply(SP1, function(x)
            mutate(x,
               Date = as.Date(Date, "%d/%m/%Y")))

# Unimos todo en un data frame
data <- do.call(rbind, unname(SP1))

# Obtenemos el dataset con el que trabajaremos
(data)

# Exportamos el dataframe a CSV
write.csv(data, "data.csv")
