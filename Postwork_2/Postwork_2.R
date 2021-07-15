# Importamos bibliotecas:
library(dplyr)
library(magrittr)

### Importar datos

# Ahora agregamos aún más datos. Utilizaremos los datos de las temporadas 2017/2018, 2018/2019 y 2019/2020.
temporadas <- c( SP1.1718 = "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
               , SP1.1819 = "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
               , SP1.1920 = "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
               ) %>% lapply(read.csv)

### Revisión de estructura de los datos
# Revisamos su estructura
get.info <- function(data){
  data %>% str
  data %>% head
  data %>% summary
  data %>% View
}

temporadas["SP1.1718"] %>% get.info
temporadas["SP1.1819"] %>% get.info
temporadas["SP1.1920"] %>% get.info

# Vemos que hay un diferente formato de fechas en la temporada 17/18.

#### Selección de columnas

# Seleccionamos sólo las columnas de interés:
columns <- c(  "Date"
             , "HomeTeam" 
             , "AwayTeam"
             , "FTHG"
             , "FTAG"
             , "FTR"
             )

temporadas %<>% lapply(select, all_of(columns)) 

###  Corrección y Unión de datos

# Revisamos que las columnas sean del mismo tipo, corregimos el error de formato 
# y tipo de dato de la columna Date y unimos en un solo data frame:
data <- temporadas %>% unname %>% do.call(rbind, .)
# Corrección del formato de fecha usando una expresión regular
data %<>% mutate(Date = gsub("/(1[78])$", "/20\\1", Date))  
# Correccioón del tipo de dato
data %<>% mutate(Date = as.Date(Date, "%d/%m/%Y"))

head(data$Date)

# data frame final solo con los datos elegidos
(dim(data))

### Escritura de archivo corregido

# Guardamos el data frame obtenido en formato csv en una carpeta llamada “equipo10”:
w.dir   <- getwd()
sub.dir <- "equipo10"
path    <- file.path(w.dir, sub.dir)
dir.create(path, showWarnings = F, recursive = T)
setwd(path)

# Guardamos el df en un archivo csv
write.csv(data, file = 'Postwork_02.csv', row.names = FALSE)
setwd(w.dir)
