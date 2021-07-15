#' # 🏅 EQUIPO 10 
#' 
#' ## BEDU SANTANDER UNIVERSIDADES 
#' 
#' ## 👩🏻🧑🏽 Integrantes 
#' 
#' -   María Magdalena Castro Sam
#' 
#' -   Sergio Napoleón Leal
#' 
#' -   Jesús Omar Magaña Medina
#' 
#' -   Fernando Itzama Novales Campos
#' 
#' -   Adrián Ramírez Cortés
#' 
#' -   Efraín Soto Olmos
#' 
#' ### Descripción 
#' 
#' Éste código analiza algunos datos de la primera división de la liga
#' española, obtenidos de <https://www.football-data.co.uk/spainm.php>. Más
#' notas sobre los datos pueden encontrarse en
#' <https://www.football-data.co.uk/notes.txt>.
#' 
#' 
#' Link para la aplicacion Shiny: https://omar-magaa.shinyapps.io/Postwork8/
#' 
#' Link para el video: https://www.youtube.com/watch?v=mOSzWZWkoHE
#' 
#' Link para el HTML interactivo: https://itzamango.github.io/postwork-equipo-10/
#' 
#' 

# obtener df con la probabilidad marginal y frecuencia
get.prob.df<- function(data, team, name.gol, name.prob){ 
  team <- enquo(team)
  
  data %>% 
    pull(!!team) %>% 
    table(., dnn = (name.gol)) %>% 
    as.data.frame() %>% 
    mutate(!!name.prob := Freq/sum(Freq)) 
}

# Redondear 3 digitos
get.round <- function(data, digits){
  data %>% mutate_if(is.numeric, round, digits=digits)
}  


# Obtener la probabilidad marginal en objeto tabla
get.prob.m.tbl <- function(data, team, name.prob){
  team <- enquo(team)
  data %>%
    pull(!!team) %>%
    table %>%
    prop.table -> prob.tbl
  
  names(attributes(prob.tbl)$dimnames) <- name.prob
  return(prob.tbl)
}

# Obtener la frecuencia y probabilidad
get.prob.df<- function(data, team, name.gol, name.prob){ 
  team <- enquo(team)
  data %>% 
    pull(!!team) %>% 
    table(., dnn = (name.gol)) %>% 
    as.data.frame %>% 
    mutate(!!name.prob := Freq/sum(Freq)) 
}

# Obtener la probabilidad conjunta en objeto tabla
get.prob.joint.tbl <- function(data, team.h, team.a, name.h, name.a){
  data %>%
  {table( team.h,  team.a, dnn = c(name.h, name.a)) } %>% 
  prop.table
}
 
# Obtener en dataframe la probabilidad conjunta
get.prob.joint.df <- function(data, team.h, team.a, name.h, name.a){
  data %>%
  {table( team.h,  team.a, dnn = c(name.h, name.a)) } %>% 
  prop.table %>% 
  unclass %>% 
  as.data.frame
}

#' 
#' Importamos las bibliotecas con las que trabajaremos:
#' 
library(dplyr)

#' 
#' Importamos datos de la primera división 2019-2020:
#' 
data <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

#' 
#' Vemos la ayuda del comando table:
#' 
## ?table

#' 
#' ## 🔢 get.freq()
#' 
get.freq <- function(data, team, name.freq){
  team <- enquo(team)
  data %>%
    pull(!!team) %>%
    table(., dnn = name.freq) 
}

#' 
#' ## 📋 Tablas de Frecuencias
#' 
#' Extraemos las columnas, obtenemos la frecuencia marginal para el equipo
#' de casa y visitante en una función. con table generamos una tabla que
#' nos indica la frecuencia de los goles:
#' 
#' ### Equipo Local
#' 
freq.m.local <- 
  get.freq(data, FTHG, "Freq.Home")

(freq.m.local)

#' 
#' ### Equipo Visitante
#' 
freq.m.visit <- 
  get.freq(data, FTAG, "Freq.Away")

(freq.m.visit)

#' 
#' ## 🔢 get.prob.m.tbl()
#' 
get.prob.m.tbl <- function(data, team, name.prob){
  team <- enquo(team)
  data %>%
    pull(!!team) %>%
    table %>%
    prop.table -> prob.tbl
  
  names(attributes(prob.tbl)$dimnames) <- name.prob
  return(prob.tbl)
}

#' 
#' ## 📋 Probabilidad Marginal
#' 
#' La probabilidad marginal es la probabilidad de que ocurra un evento
#' simple, por lo tanto estará dada por el número de goles sobre el total
#' de los partidos.
#' 
#' Calcularemos la probabilidad de que el equipo que juegue en casa anote
#' '*x'* goles en una función:
#' 
#' ### Equipo Local
#' 
#' La probabilidad marginal de que el equipo que juega local anote x goles
#' (x = 0, 1, 2, ...)
#' 
prob.m.local <- 
  get.prob.m.tbl(data, FTHG, "Prob.Home")

(prob.m.local %>% round(3))

#' 
#' ### Equipo Visitante
#' 
#' La probabilidad marginal de que el equipo que juega como visitante anote
#' y goles (y = 0, 1, 2, ...)
#' 
#' En el caso del equipo vistante el procedimiento es análogo.
#' 
prob.m.visit <- 
  get.prob.m.tbl(data, FTAG, "Prob.Away")

(prob.m.visit %>% round(3))


#' 
#' ## 🔢 get.prob.df() y get.round()
#' 
get.prob.df<- function(data, team, name.gol, name.prob){ 
  team      <- enquo(team)
  data %>% 
    pull(!!team) %>% 
    table(., dnn = (name.gol)) %>% 
    as.data.frame %>% 
    mutate(!!name.prob := Freq/sum(Freq)) 
}

#' 
get.round <- function(data, digits){
  data %>% mutate_if(is.numeric, round, digits=digits)
}

#' 
#' ## 📋 Frecuencias y Probabilidad Marginal
#' 
#' Obtenemos un dataframe con las frecuencias y probabilidades con una
#' función y redondeamos:
#' 
#' ### Equipo Local
#' 
data.local <- get.prob.df(data, FTHG, "Gol.Home", "Prob.Marginal")

(data.local %>% get.round(3))

#' 
#' ### Equipo Visitante
#' 
data.visit <- get.prob.df(data, FTAG, "Gol.Away", "Prob.Marginal" )

(data.visit %>% get.round(3))

#' 
#' ## 🔢 get.prob.joint.tbl() y get.prob.joint.df
#' 
get.prob.joint.tbl <- function(data, team.h, team.a, name.h, name.a){
  data %>%
  {table( team.h,  team.a, dnn = c(name.h, name.a)) } %>% 
  prop.table
}
 
get.prob.joint.df <- function(data, team.h, team.a, name.h, name.a){
  data %>%
  {table( team.h,  team.a, dnn = c(name.h, name.a)) } %>% 
  prop.table %>% 
  unclass %>% 
  as.data.frame
}

#' 
#' ## 📋 Probabilidad Conjunta P(x∩y)
#' 
#' La probabilidad conjunta toma en cuenta la probabilidad de dos eventos
#' sobre el total de resultados posibles.
#' 
#' Calcularemos la probabilidad conjunta de que el equipo local anote '*x'*
#' goles y el visitante '*y'* goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
#' con una función:
#' 
table.prob.joint <-
  get.prob.joint.tbl(data, data$FTHG, data$FTAG, "x (Home)", "y (Away)")

(table.prob.joint %>% round(3))

#' 
#' # 📂 POSTWORK 2 {#p200}
#' 
#' Importamos bibliotecas:
#' 
library(dplyr)
library(magrittr)

#' 
#' Ahora agregamos aún más datos. Utilizaremos los datos de las temporadas
#' 2017/2018, 2018/2019 y 2019/2020.
#' 
#' ## 📋 Importar datos
#' 
#' Importamos los datos a una lista:
#' 
temporadas <- c( SP1.1718 = "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
               , SP1.1819 = "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
               , SP1.1920 = "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
               ) %>% lapply(read.csv)

#' 
#' ## 📋 Revisión de estructura de los datos
#' 
#' Revisamos su estructura:
#' 
#' > Solo se muestran las primeras 6 líneas de la consola
#' 
get.info <- function(data){
  data %>% str
  data %>% head
  data %>% summary
  data %>% View
}

#' 
temporadas["SP1.1718"] %>% get.info

temporadas["SP1.1819"] %>% get.info

temporadas["SP1.1920"] %>% get.info

#' 
#' Vemos que hay un diferente formato de fechas en la temporada 17/18.
#' 
#' ## 📋 Selección de columnas
#' 
#' Seleccionamos sólo las columnas de interés:
#' 
columns <- c(  "Date"
             , "HomeTeam" 
             , "AwayTeam"
             , "FTHG"
             , "FTAG"
             , "FTR"
             )

temporadas %<>% lapply(select, all_of(columns)) 

#' 
#' ## 📋 Corrección y Unión de datos
#' 
#' Revisamos que las columnas sean del mismo tipo, corregimos el error de
#' formato y tipo de dato de la columna `Date` y unimos en un solo data
#' frame:
#' 
data <- temporadas %>% unname %>% do.call(rbind, .)
# Corrección del formato de fecha usando una expresión regular
data %<>% mutate(Date = gsub("/(1[78])$", "/20\\1", Date))  
# Correccioón del tipo de dato
data %<>% mutate(Date = as.Date(Date, "%d/%m/%Y"))

head(data$Date)

#data frame final solo con los datos elegidos
(dim(data))

#' 
#' ## 📋 Escritura de archivo corregido
#' 
#' Guardamos el data frame obtenido en formato csv en una carpeta llamada
#' "equipo10":
#' 
w.dir   <- getwd()
sub.dir <- "equipo10"
path    <- file.path(w.dir, sub.dir)
dir.create(path, showWarnings = F, recursive = T)
setwd(path)

#guardamos el df en un archivo csv
write.csv(data, file = 'Postwork_02.csv', row.names = FALSE)
setwd(w.dir)

#' 
#' # 📂 POSTWORK 3 {#p300}
#' 
#' Importamos las bibliotecas:
#' 
library(ggplot2)
library(plotly)
library(dplyr)
library(ggplot2)
library(viridis)
library(viridisLite)

#' 
#' Con el data frame obtenido realizaremos algunas gráficas.
#' 
data <- "https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_2/Postwork_02.csv" %>%
        read.csv %>% 
        mutate(Date = as.Date(Date, "%Y-%m-%d"))

#' 
#' ## 📋 Local: Probabilidad Marginal
#' 
#' Calcularemos la probabilidad marginal de que el equipo local anote '*x'*
#' goles mediante una función previa:
#' 
prob.m.local <- 
  get.prob.df(data, FTHG, "Gol.Home", "P.Marginal") 

(prob.m.local %>% get.round(3))

#' 
#' ## 🔢 plot.bar()
#' 
#' Realizamos una función de una gráfica para vizualizar los datos:
#' 
plot.bar <- function(data, x.lab, y.lab, f.lab, title){
  Goles         <- data[1] %>% unlist()
  Porcentaje    <- (data[3]*100 ) %>% unlist %>% round(., digits=2)
  Prob.Marginal <- data[3] %>% unlist() %>% round(., digits=4)
  data %>%
  ggplot() +
  geom_bar(stat = 'identity') +
  aes(  x = Goles
      , y = Porcentaje
      , fill = Porcentaje
      , text = paste("Prob Marginal", Prob.Marginal)
                      #, group = interaction(Goles, Porcentaje)
       ) +
  labs(  x     = x.lab
       , y     = y.lab
       , fill  = f.lab
       , title = title
       ) +
  theme_minimal() +
  theme(  text = element_text(size=20)
        , legend.title = element_text(size=10) 
        ) +
  scale_fill_viridis(name=f.lab, direction = 1) +
  theme(  axis.text.x  = element_text(size = 15)  #tamaño de numeros en 
        , axis.text.y  = element_text(size = 15)  #tamaño de numeros en y
        , axis.title.x = element_text(size = 18)  #tamaño del letrero en x
        , axis.title.y = element_text(size = 18)  #tamaño del letrero en y
        , plot.title   = element_text(size = 20, hjust = 0.5) #tamaño del titulo
        )    
}

#' 
#' Realizamos una gráfica para vizualizar los datos:
#' 
plot.local <- plot.bar(prob.m.local
              , "Goles [n]"
              , "Probabilidad de ocurrencia [%]"
              , "%"
              , "Probabilidad de anotación del equipo local"
              )

#' 
#' ## 📊 Local: P(x) Marginal
#' 
ggplotly(plot.local) #versión interactiva

#' 
#' ## 🎞️ Animación Bar Plot P(x)
#' 

library(gganimate)

(plot.local.animation <- 
    plot.local + 
    transition_states(plot.local$data$Gol.Home, transition_length = 10) +
    enter_grow() +
    shadow_mark())


#' 
#' 
#' 
#' Lo generamos como video, para eso es necesario tener ffmpeg instalado:
#' Tenerlo descargado y agregar la ruta de la carpeta "folder_ffmpeg\bin" a las variables
#' de entorno en Windows
#' 
## # animate(plot.local.animation, duration = 2.7, renderer = ffmpeg_renderer())
## 

#' 
#' ## 📋 Visitante: Probabilidad Marginal
#' 
#' Ahora calcularemos la probabilidad para el equipo visitante:
#' 
prob.m.visit <- 
  get.prob.df(data, FTAG, "Gol.Away", "P.Marginal") 
 
(prob.m.visit %>% get.round(3))

#' 
#' Realizamos una gráfica para vizualizar los datos:
#' 
plot.visit <- plot.bar(prob.m.visit
              , "Goles [n]"
              , "Probabilidad de ocurrencia [%]"
              , "%"
              , "Probabilidad de anotación del equipo visitante"
              )

#' 
#' ## 📊 Visitante: P(y) Marginal
#' 
ggplotly(plot.visit) #versión interactiva

#' 
#' ## 🎞️ Animación Bar Plot P(y)
#' 
(plot.visit.animation <- 
    plot.visit + 
    transition_states(  plot.visit$data$Gol.Away
                      , transition_length = 10) +
    enter_grow() +
    shadow_mark())


#' 
#' 
#' Lo generamos como video, para eso es necesario tener ffmpeg instalado:
#' Tenerlo descargado y agregar la ruta de la carpeta "folder_ffmpeg\bin" a las variables
#' de entorno en Windows
#' 
## #animate(plot.visit.animation, duration = 2.7, renderer = ffmpeg_renderer())
## 

#' 
#' ## 📋 Probabilidad Conjunta P(x∩y)
#' 
#' La probabilidad conjunta de que el equipo que juega en casa anote *'x'*
#' goles y el equipo que juega como visitante anote '*y'* goles calculada
#' con una función previa:
#' 
prob.joint <- 
  get.prob.joint.tbl(data, data$FTHG, data$FTAG, "Local", "Visitante") 

(prob.joint  %<>% "*"(100) %>% round(2))

#' 
#' Realizamos un *heat map* con una función para visualizar los datos:
#' 
prob.joint %<>% as.data.frame() %>% rename(Probabilidad = Freq)

#' 
#' ## 🔢 plot.heatmap()
#' 
plot.heatmap <- function(data, x.lab, y.lab, f.lab, title){
  Local         <- data[1] %>% unlist
  Visitante     <- data[2] %>% unlist
  Probabilidad  <- data[3] %>% unlist
    ggplot(data) +
    aes(   Local
         , Visitante
         , fill = Probabilidad
         ) + #gráficamos
    geom_raster() +
    labs(  x     = x.lab
         , y     = y.lab
         , fill  = f.lab
         , title = title
         ) +
    theme(  text = element_text(size=18)
          , legend.title = element_text(size=15) 
          ) +
    scale_fill_viridis(  name=f.lab
                       , direction = 1 #, option = "H"
                       )  +
    theme(  axis.text.x  = element_text(size = 15)  #tamaño de numeros en 
    , axis.text.y  = element_text(size = 15)  #tamaño de numeros en y
    , axis.title.x = element_text(size = 18)  #tamaño del letrero en x
    , axis.title.y = element_text(size = 18)  #tamaño del letrero en y
    , plot.title   = element_text(size = 20, hjust = 0.5) #tamaño del titulo
    ) 
}

#' 
#' Realizamos una gráfica para visualizar los datos:
#' 
plot.mapa.calor <-
plot.heatmap(  prob.joint
             , "Local [goles]"
             , "Visitante [goles]"
             , "Probabilidad [%]"
             , "Probabilidad Conjunta de anotación"
             )

#' 
#' ## 📊 Heat Map P(x∩y) Conjunta
#' 
ggplotly(plot.mapa.calor)    #versión interactiva

#' 
#' ## 🎞️ Animación Heat Map P(x∩y)
#' 
(heatmap.animation <- 
   plot.mapa.calor + 
   transition_states(  plot.mapa.calor$data$Probabilidad/100
                     , transition_length = 20) +
   enter_grow() +
  shadow_mark())


#' 
#' 
#' Lo generamos como video, para eso es necesario tener ffmpeg instalado:
#' Tenerlo descargado y agregar la ruta de la carpeta "folder_ffmpeg\bin" a las variables
#' de entorno en Windows
#' 
## #animate(heatmap.animation, duration = 7, renderer = ffmpeg_renderer())

#' 
#' # 📂 POSTWORK 4 {#p400}
#' 
get.prob.df<- function(data, team, name.gol, name.prob){ 
  team      <- enquo(team)
  
  data %>% 
      pull(!!team) %>% 
      table(., dnn = (name.gol)) %>% 
      as.data.frame %>% 
      mutate(!!name.prob := Freq/sum(Freq)) 
}

get.prob.m.tbl <- function(data, team, name.prob){
  team <- enquo(team)
  data %>%
    pull(!!team) %>%
    table %>%
    prop.table -> prob.tbl
  
  names(attributes(prob.tbl)$dimnames) <- name.prob
  return(prob.tbl)
}

get.prob.joint.tbl <- function(data, team.h, team.a, name.h, name.a){
  data %>%
    {table( team.h,  team.a, dnn = c(name.h, name.a)) } %>% 
    prop.table
}


#' 
#' Ahora obtendremos una tabla de cocientes al dividir las probabilidades
#' conjuntas por el producto de las probabilidades correspondientes:
#' 
#' $$Cocientes = \dfrac{P\left(x \cap y\right)_{conjunta}}
#'               {P\left(x\right)_{marginal} P\left(y\right)_{marginal}} $$
#' 
data <- read.csv("https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_2/Postwork_02.csv")

#' 
#' ## 📋 Probabilidad Marginal
#' 
#' Para la probabilidad marginal de los goles metidos por locales con una
#' función previa:
#' 
prob.m.local <- get.prob.df(data, FTHG, "Gol.Home", "Prob.Marginal") 

(prob.m.local %>% get.round(3))

#' 
#' Para los goles metidos por el equipo visitante:
#' 
prob.m.visit <- get.prob.df(data, FTAG, "Gol.Away", "Prob.Marginal") 

(prob.m.visit %>% get.round(3))

#' 
#' ## 📋 Producto de Probabilidades Marginales
#' 
#' Hacemos el producto de las probabilidades marginales obtenidas con una
#' función previa:
#' 
tbl.local <- get.prob.m.tbl(data, FTHG, "Home")
tbl.visit <- get.prob.m.tbl(data, FTAG, "Away")

product.prob.m <- tbl.local %o% tbl.visit

(product.prob.m %>% round(3))

#' 
#' ## 📋 Probabilidad Conjunta P(x∩y)
#' 
#' Obtenemos la probabilidad conjunta con una función previa:
#' 
prob.conjunta <-
  get.prob.joint.tbl(data, data$FTHG, data$FTAG, "Home", "Away")

(prob.conjunta %>% round(3))

#' 
#' ## 📋 Cociente de Probabilidades
#' 
#' Realizamos el cociente:
#' 
cociente <- prob.conjunta/product.prob.m

(cociente %>% round(2))

#' 
#' ## 📋 Bootstrap
#' 
#' Para determinar si el número de goles del equipo local o el de el equipo
#' visitante son dependientes o independientes, realizaremos un
#' procedimiento de bootstrap para obtener más cocientes similares y
#' analizar la distribución.
#' 
#' Transformamos el data frame a columna para facilitar el bootstrap.
#' 
data_origin <- as.data.frame(as.vector(unlist(cociente)))

colnames(data_origin) <- "values"

#' 
#' Utilizamos la biblioteca `"rsample"` para poder hacer las muestras
#' bootstrap:
#' 
library(rsample)

#' 
#' Fijamos la semilla para poder reproducir los datos:
#' 
set.seed(83928782)

#' 
#' ## 🔢 bootstraps() rsample
#' 
#' Aplicamos la función bootstraps, para generar 1000 muestras,
#' guardándolas en boot:
#' 
boot <- bootstraps(data_origin, times = 1000)

#' 
#' Cargamos las siguientes bibliotecas para visualizar datos:
#' 
library(purrr)
library(modeldata)
library(viridis)
library(tidyverse)
library(hrbrthemes)
library(forcats)
library(viridisLite)

#' 
#' Realizamos una función para hacer una columna de las medias muestrales
#' obtenidas por bootstrap:
#' 
obtener_media <- function(boot_splits) {
  data_mean         <- analysis(boot_splits)
  medias_muestrales <- mean(data_mean[,1])
  return(medias_muestrales)
}

#' 
#' ## 📋 Medias Muestrales
#' 
#' Observamos el valor de la media de las medias muestrales tras aplicar la
#' función:
#' 
boot$means <- map_dbl(boot$splits, obtener_media)

length(boot$means); summary(boot$means)

#' 
#' ## 📋 Intervalos de confianza
#' 
#' Comprobamos la hipótesis de que la media se encuentra en 1 con las
#' medias muestrales bootstrap y obtenemos el intervalo de confianza al 95%
#' con una prueba t:
#' 
t_boot    <- t.test(boot$means, alternative = "two.sided", mu = 1)
t_boot_ic <- round(t_boot$conf.int,3)
t_boot_ic

#' 
#' ## 🔢 plot.histogram()
#' 
#' Realizamos una función para un histograma:
#' 
plot.histogram <- function(  data, data_mean, ic_2, n_bins
                           , title, x.lab, y.lab, f.lab ){
    ggplot(data) +
    geom_histogram(  bins = n_bins
                   , color=NA
                   , aes(fill=..count..)
                   ) + 
    aes(data_mean) + 
    labs(  x     = x.lab
         , y     = y.lab
         , fill  = f.lab
         , title = title
         ) +
    geom_vline(
                aes( xintercept = mean(data_mean))
                ) +
    geom_vline(  xintercept = c(ic_2), ####
                 linetype = c(2,2)
                 ) +
    scale_fill_viridis(name = f.lab) +
    theme_minimal() +
    #theme_ipsum() +
    theme(  axis.text=element_text(size=18)
          , axis.title = element_text(size=24) #text = element_text(size=18)
          , title = element_text(size=18)
          , legend.text = element_text(size=15) 
          , legend.title = element_text(size=20) 
          , panel.spacing = unit(0.1, "lines")
          , strip.text.x = element_text(size = 10) + xlim(c(0.3, 1.3)) + scale_x_continuous(limits = c(-0.3, 1.3))
          #legend.position="none",
    )
}

#' 
#' Realizamos el histograma de las medias muestrales obtenidas por
#' bootstrap.
#' 
ic_mean_ic <- c(t_boot_ic[1], 
                mean(boot$means), 
                t_boot_ic[2])

hist_boot <- 
  plot.histogram(  boot
               , boot$means
               , t_boot_ic
               , 18 #14 #12 #10  #10#10 #8 #8 #8 #5 #18
               , paste("Histograma de las medias muestrales bootstrap"
                       , "\n<i><b>n="
                       , length(boot$means)
                       , "</b></i>"
                       , sep = ""
                       )
               , "Valor de la Media"
               , "Frecuencia"
               , "Frec"
  )

#' 
#' ## 📊 Histograma medias bootstrap
#' 
ggplotly(hist_boot)

#' 
#' > La línea sólida indica la posición de la media y las punteadas, la
#' > posición de los límites del intervalo de confianza.
#' 
#' De igual modo lo hacemos para la muestra original:
#' 
t_origin <- t.test(data_origin$values, alternative = "two.sided", mu = 1)
t_origin_ic <- t_origin$conf.int %>% round(3)
t_origin_ic

#' 
ori_ic_mean_ic <- c(t_origin_ic[1], 
                    mean(data_origin$values), 
                    t_origin_ic[2])

hist_origin <- 
      plot.histogram(  data_origin
                 , data_origin$values
                 , t_origin_ic
                 , 11
                 , "Histograma de la muestra original"
                 , "Valor de la Muestra"
                 , "Frecuencia"
                 , "Frec"
                 )

#' 
#' ## 📊 Histograma original
#' 
ggplotly(hist_origin)

#' 
#' > La línea sólida indica la posición de la media y las punteadas, la
#' > posición de los límites del intervalo de confianza.
#' 
#' ## 📋 Pruebas t
#' 
#' Vemos los datos de los estadísticos de las pruebas t para ambos
#' conjuntos de datos.
#' 
#' Remuestreo bootstrap:
#' 
t_boot

#' 
#' Muestras originales:
#' 
t_origin

#' 
#' ## 🏁 Conclusiones
#' 
#' Observamos como la distribución de la media de los cocientes no era normal.
#' Usamos la media muestral de 1000 muestras generadas por método bootstrap y acorde
#' al Teorema del Límite Central observamos que la distribución de las medias muestrales
#' bootstrap es normal.
#' 
#' Por medio de una prueba de hipótesis y los intervalos de confianza vimos que 
#' no hay evidencia significativa para establecer que la media de los cocientes 
#' tienda a 1. Por tanto, podemos considerar a las variables como dependientes.
#' 
#' # 📂 POSTWORK 5 {#p500}
#' 
#' Con los datos de la liga de primera división española, de las temporadas
#' 2017/2018, 2018/2019 y 2019/2020 haremos una predicción de los
#' resultados de los partidos de la fecha 07/16/2020.
#' 
#' Utilizamos la librería dplyr para manipulaciín de datos:
#' 
library(dplyr)

#' 
#' Y la librería fbRanks para las predicciones en base al modelo de Dixon y
#' Coles:
#' 
library(fbRanks)

#' 
#' ## 📋 Escribir soccer.csv
#' 
#' Guardamos los datos de las 3 temporadas en una lista. Seleccionamos
#' unicamente los elementos "Date", "HomeTeam", "FTHG", "AwayTeam" ,"FTAG",
#' los cuales son esenciales para el modelo y cambiamos los nombres de las
#' columnas por requerimiento de la biblioteca. Unimos las 3 temporadas en
#' una sola y Cambiamos la forma del año, para poder aplicar un único
#' formato date:
#' 
columns <- c(  date       = "Date"
             , home.team  = "HomeTeam" 
             , away.team  = "AwayTeam"
             , home.score = "FTHG"
             , away.score = "FTAG"
             )

SmallData <-
    c( "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
     , "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
     , "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
     ) %>% 
        lapply (read.csv)                     %>%
        lapply (select, all_of(columns))      %>% 
        do.call(rbind, .)                     %>% 
        mutate (date = gsub("/(1[78])$", 
                            "/20\\1", date))  %>%
        mutate (date = as.Date(date, "%d/%m/%Y"))

#' 
#' Guardamos los cambios en un archivo csv:
#' 
w.dir   <- getwd()
sub.dir <- "equipo10"
path    <- file.path(w.dir, sub.dir)
dir.create(path, showWarnings = F, recursive = T)
setwd(path)

write.csv(SmallData, file = './soccer.csv', row.names = F)
setwd(w.dir)

#' 
#' ## 📋 fbRanks: anotaciones y equipos
#' 
#' Aplicamos la primera función de la biblioteca,
#' "create.fbranks.dataframes" con la finalidad de poder hacer una limpieza
#' de datos, excluyendo los datos nulos para las puntuaciones, así como
#' nombres repetidos, los cambios incluyen la transformación del formato de
#' columna "date":
#' 
listasoccer <- create.fbRanks.dataframes(scores.file = "soccer.csv")

(listasoccer)

#' 
#' Como residuo, la función nos devuelve una lista con un data.frame scores
#' con los datos de nuestro csv limpios y las fechas en orden ascendente,
#' como también el data frame teams, en este caso generado del csv, con los
#' nombres de los equipos sin repetir, el data.frame teams.resolver y por
#' último el data.frame raw.scores, con algunas configuraciones para el uso
#' de otras funciones.
#' 
## View(listasoccer)

#' 
#' Guardamos el data.frame scores generados, con las puntuaciones en las
#' temporadas
#' 
anotaciones = listasoccer$scores

#' 
#' Guardamos la lista de equipos
#' 
equipos = listasoccer$teams

#' 
#' ## 📋 Ranking de Equipos
#' 
#' Guardamos las fechas sin repetir
#' 
fecha = unique(listasoccer$scores$date)

#' 
#' Y la cantidad de las fechas para un mejor control
#' 
n = length(fecha)

#' 
#' Aplicamos la función "rank.teams", el cual aplica una regresión lineal
#' usando como modelo la distribución de Poisson, tomando como rango de
#' tiempo la duración de un partido.
#' 
#' La función requiere los datos scores, la lista de equipos y las fechas
#' entre las que generamos nuestro ranking
#' 
ranking = rank.teams(  anotaciones
                     , equipos
                     , max.date = fecha[n-1]
                     , min.date = fecha[1]
                     , date.format = "%Y-%m-%d")

#' 
#' Como resultado nos da una lista con las especificaciones que le dimos a
#' nuestra función Y nos presenta una tabla ranking, con los coeficientes
#' de la regresión, tanto de ataque y defensa, modificados para su mejor
#' comprensión, y uniéndolos en un total, posicionando a los equipos con
#' base en el total.
#' 
(ranking)

#' 
#' La función da como resultado una clase única de la librería "fbRanks"
#' necesaria para el uso de otras funciones
#' 
class(ranking)

#' 
#' En la lista "ranking" es posible encontrar los coeficientes de la
#' regresión en crudo, como sus especificaciones
#' 
ranking[1]

#' 
## View(ranking[1])

#' 
#' Podemos extraer los datos de la función rank
#' 
columns = c(  team    = "ranks.team"
            , total   = "ranks.total"
            , attack  = "ranks.attack"
            , defense = "ranks.defense"
            , n.games.Var1 = "ranks.n.games.Var1"
            , n.games.Freq = "ranks.n.games.Freq"
            )
Ranking.datos <-
    ranking %>% 
    print   %>% 
    as.data.frame %>% 
    select(all_of(columns))

#' 
#' y aprovecharlos para hacer diversos análisis teniéndolos en formato csv
#' ya exportado podemos tomarlo del link de nuestro repositorio
#' 
## Ranking.datos <- read.csv("https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/ranking20.csv")

#' 
#' Basándonos en nuestro modelo, podemos ver el máximo atacante, y el
#' máximo defensor
#' 
(Max.atacante <- Ranking.datos %>% filter(attack  == max(attack)))
(Max.defensor <- Ranking.datos %>% filter(defense == max(defense)))

#' 
#' Como también el resumen
#' 
summary(Ranking.datos)

#' 
#' ## 📋 Predicción Última Fecha
#' 
#' Para las predicciones referentes a los últimos partidos de la liga,
#' utilizaremos la función "predict", que se basa en el modelo de Dixon and
#' Coles, el cual es una modificación del modelo de predicciones de
#' distribuciones de Poisson, con una modificación en cuanto a la
#' sobre-estimación de los datos fuera de la media.
#' 
#' La función requiere un objeto clase "fbRank", ya que hace uso de los
#' coeficientes de la regresión lineal.
#' 
prediccion = predict.fbRanks(ranking, date = fecha[n])

#' 
#' La función regresa una lista, con data.frames y vectores, nos
#' enfocaremos principalmente en su primer objeto "scores", a diferencia de
#' nuestro csv, solo contiene los datos sobre los últimos partidos, dando
#' como información las probabilidades de victoria, derrota, empate
#' victoria-derrota con 0 goles del rival, ademas de incluir los
#' coeficientes de la regresión
#' 
## View(prediccion[1])

#' 
#' Centrándonos en las probabilidades de victoria, derrota, empate para el
#' equipo de local como también la predicción del número de goles
#' redondeada:
#' 

columns <-
c(  Home.team      = "home.team"
  , Away.team      = "away.team"
  , Prob.win.home  = "home.win"
  , Prob.win.away  = "away.win"
  , Prob.tie       = "tie"
  , Pred.home.score = "pred.home.score"
  , Pred.away.score = "pred.away.score"
  )

predict.prob <-
  prediccion$scores %>% as.data.frame %>% select(all_of(columns)) %>% 
      mutate(  Pred.home.score = Pred.home.score %>% round(0)
             , Pred.away.score = Pred.away.score %>% round(0))

(predict.prob)

#' 
#' Hacemos una comparación con los datos reales:
#' 

columns <-
c(  Home.team           = "home.team"
  , Home.score          = "home.score"
  , Pred.home.score     = "pred.home.score"
  , Away.team           = "away.team"
  , Away.score          = "away.score"
  , Pred.away.score     = "pred.away.score"
  )

   
comparacion <-
    prediccion$scores %>% as.data.frame %>% select(all_of(columns)) %>% 
      mutate(  Pred.home.score = Pred.home.score %>% round(0)
             , Pred.away.score = Pred.away.score %>% round(0))

#' 
#' Los partidos en los cuales acertó el número de goles del equipo de local
#' 

(comparacion %>% filter(Home.score == Pred.home.score))

#' 
#' Y en donde acertó los goles del equipo visitante
#' 
(comparacion %>% filter(Away.score == Pred.away.score))

#' 
#' ## 📋 Matriz de Confusión
#' 
#' Hacemos la predicción de todas las fechas de los partidos usando el
#' objeto ranking y obtenemos la matriz de confusión donde las clases son
#' el número de goles
#' 

prediccion.total.partidos = predict.fbRanks(ranking, date = fecha)
columns <-
c(  Home.team           = "home.team"
  , Home.score          = "home.score"
  , Pred.home.score     = "pred.home.score"
  , Away.team           = "away.team"
  , Away.score          = "away.score"
  , Pred.away.score     = "pred.away.score"
  )

comparacion.total.partidos <-
    prediccion.total.partidos$scores %>% as.data.frame %>% select(all_of(columns)) %>% 
      mutate(  Pred.home.score = Pred.home.score %>% round(0)
             , Pred.away.score = Pred.away.score %>% round(0))

#' 
library(caret)
library(lattice)

#' 
#' ### Matriz de confusión de la predicción de los goles locales
#' 
confusion.m.local <-
    confusionMatrix(  factor(  comparacion.total.partidos$Pred.home.score
                             , levels = 0:max(comparacion.total.partidos$Home.score))
                    , factor(  comparacion.total.partidos$Home.score
                             , levels = 0:max(comparacion.total.partidos$Home.score))
                    , dnn = c("Prediccion", "Valores Reales"))

(confusion.m.local)

#' 
#' Vemos que la exactitud (accuracy), que es la cantidad de predicciones
#' positivas que fueron correctas y que esta dada por la suma de la
#' diagonal entre la suma total, es de 35.53%. El modelo acertó en el
#' 35.53% de su predicción.
#' 
#' Además se aprecia la relación entre los valores predecidos y los reales.
#' 
#' En la sensibilidad (sensitivity), que es la proporción de casos
#' positivos que fueron correctamente identificados, vemos que el valor más
#' alto es cuando predice que el equipo local anota 1 gol. El modelo acertó
#' en el 81.77% de las anotaciones reales que fueron de 1 gol.
#' 
#' Al ver la distribución de la matriz notamos que el modelo no acertó la
#' predicción en ningún valor de 5 a 8 goles. Ni siquiera hubo esos valores
#' en su predicción.
#' 
#' Y el "Pos Pred Value" es la proporción de predicciones correctamente
#' identificadas del total de predicciones para cada clase. De todos los
#' marcadores con gol cero que predijo, el 61.53% fue acertado. Los demás
#' están por abajo del 50%.
#' 
#' ### Matriz de confusión de la predicción de los goles del visitante
#' 
confusion.m.visit <-
    confusionMatrix(  factor(  comparacion.total.partidos$Pred.away.score
                             , levels = 0:max(comparacion.total.partidos$Away.score)
                             )
                    , factor(  comparacion.total.partidos$Away.score
                             , levels = 0:max(comparacion.total.partidos$Away.score))
                    , dnn = c("Prediccion", "Valores Reales"))

(confusion.m.visit)

#' 
#' Vemos que la exactitud (accuracy) es de 34.91%. El modelo acertó en el
#' 34.91% de su predicción.
#' 
#' En la sensibilidad (sensitivity) vemos que el valor más alto es cuando
#' predice que el equipo visitante anota 1 gol. El modelo acertó en el
#' 77.06% de las anotaciones reales que fueron de 1 gol.
#' 
#' Al ver la distribución de la matriz notamos que el modelo no acertó la
#' predicción en ningún valor de 5 y 6 goles. Ni siquiera hubo esos valores
#' en su predicción.
#' 
#' Y en el "Pos Pred Value" vemos que de todos los marcadores con gol cero
#' que predijo, el 92.30% fue acertado. Los demás están por en el 50% o
#' debajo.
#' 
#' ## 🏁 Conclusiones
#' 
#' En las últimas comparaciones de las predicciones hechas por la librería
#' fbRanks podemos notar que la predicción en cuanto el número de goles
#' debe tomarse con mucho cuidado, esto ya que por ejemplo en el partido
#' jugado por Barcelona, en el partido éste anotó 5 goles, estos casos son
#' muy poco probables, más si es usado el modelo de Dixon y Coles, ya que
#' reduce la posible sobre-estimación de estos casos, debido a su rareza,
#' de 10 partidos acertó totalmente en 1 partido, por lo que estos análisis
#' pueden servir como referencia, pero la predicción de resultados de
#' juegos puede ser muy compleja, por el número de factores involucrados.
#' 
#' En las métricas obtenidas por la matriz de confusión de la predicción
#' para todas las fechas con las que trabajamos obtuvimos un acierto
#' aproximado de 35% tanto para las predicciones de los goles de casa como
#' visitante.
#' 
#' # 📂 POSTWORK 6 {#p600}
#' 
#' Importamos el conjunto de datos match.data.csv a R:
#' 

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-06/Postwork/match.data.csv")

head(df)
summary(df)


#' 
#' Agregamos una nueva columna "sumagoles" que contiene la suma de goles
#' por partido.
#' 
#' ## 📋 Columna "sumagoles"
#' 
df["sumagoles"] <- df$home.score+df$away.score

#' 
#' ## 📋 Promedio por mes
#' 
#' Obtuvimos el promedio por mes de la columna suma de goles. Revisamos el
#' tipo de dato de la fecha
#' 
class(df$date)
df$date <- as.Date(df$date)
class(df$date)

df$fecha <- format(df$date, format = "%Y-%m")
golesxmes <- aggregate( df$sumagoles ~ df$fecha, df , mean)
View(golesxmes)


#' 
#' ## 📋 Serie de Tiempo
#' 
#' Creamos la serie de tiempo del promedio por mes de la suma de goles
#' hasta diciembre de 2019.
#' 
golesxmes.ts <- ts(golesxmes[ ,2], start = c(2010,08), end = c(2019,12), frequency = 10)
golesxmes.ts

#' 
#' ## 📊 Serie de Tiempo
#' 
plot(golesxmes.ts, xlab = "Tiempo", ylab = "Promedio de goles", type = "o",
     pch = 12, col = "black", lwd = 2, cex = 2) + #Grafica de la serie de tiempo
title(main = "Serie de tiempo", sub = "Frecuencia = 10", cex.sub = 1,
      font.main =2) #modificaciones del titulo


#' 
#' ## 📋 Modelo Aditivo
#' 
gxm.decom.A <- decompose(golesxmes.ts)
plot(gxm.decom.A, xlab = "Tiempo", 
     sub = "Descomposición de los datos de goles por mes")


#' 
Tendencia <- gxm.decom.A$trend
Estacionalidad <- gxm.decom.A$seasonal
Aleatorio <- gxm.decom.A$random

plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2,
     sub = "Tendencia con efectos estacionales aditivos sobrepuestos")  +
     lines(Tendencia, lwd = 2, col = "blue") +
     lines(Tendencia + Estacionalidad, lwd = 2, col = "red", lty = 2)


#' 
ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
        ylab = "Promedio de goles por mes", lty = 1:2, 
        col = c("blue", "red"), lwd = 2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")

#' 
#' Comprobamos un punto de la suma de las componentes con la serie de
#' tiempo
#' 
(Tendencia[20] + Estacionalidad[20] + Aleatorio[20])
(golesxmes.ts[20])

#' 
plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2) +
     lines(Tendencia, lwd = 2, col = "blue") +
     lines(Tendencia + Estacionalidad, lwd = 2, col = "red", lty = 2)


#' 
#' ## 📋 Modelo Multiplicativo
#' 
gxm.decom.M <- decompose(golesxmes.ts, type = "mult")

plot(gxm.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de goles por mes")

#' 
Trend <- gxm.decom.M$trend
Seasonal <- gxm.decom.M$seasonal
Random <- gxm.decom.M$random

plot(golesxmes.ts, 
     xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
     ylab = "Promedio de goles por mes", lwd = 2,
     sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos") +
     lines(Trend, lwd = 2, col = "blue") +
     lines(Trend * Seasonal, lwd = 2, col = "red", lty = 2)

#' 
ts.plot(cbind(Trend, Trend * Seasonal), 
        xlab = "Tiempo", main = "Datos de goles por mes, 2010.08 - 2019.12", 
        ylab = "Promedio de goles por mes", lty = 1:2, 
        col = c("blue", "red"), lwd = 2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")

#' 
#' Comprobamos un punto de la multiplicación de las componentes con la
#' serie de tiempo
#' 
Trend[20] * Seasonal[20] * Random[20]
golesxmes.ts[20]

#' 
#' Ambos modelos arrojan el mismo resultado por lo que sería indistinto
#' elegir entre uno u otro
#' 
#' ## 🏁 Conclusiones
#' 
#' Notemos que la frecuencia es 10 y no 12, ya que aunque la serie de
#' tiempo se realiza tomando en cuenta los meses, las temporadas se juegan
#' de agosto a mayo, por lo tanto como no se realizan juegos durante los
#' meses de junio y julio sólo se toman en cuenta los 10 meses donde se
#' generan los goles
#' 
#' # 📂 POSTWORK 7 {#p700}
#' 
library(mongolite)
library(dplyr)

#' 
#' Importamos el csv
#' 
match.db <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-07/Postwork/match.data.csv")

#' 
#' ## 📋 Alojar match.data.csv a match_games
#' 
#' Conectamos el cluster con la base de datos "match_games" y la colección
#' "match"
#' 
connection <- mongo(  collection = "match"
                    , db  = "match_games"
                    , url = "mongodb+srv://equipo10:bedu@postwork7.8voq3.mongodb.net/test")

#' 
#' Verificamos que si no hay documentos agregue los datos del csv
#' 
if (connection$count() == 0) {
  connection$insert(match.db)
}

#' 
#' ## 📋 Número de Registros
#' 
#' Consultamos el número de registros
#' 
connection$count()

#' 
#' ## 📋 Consulta Sintaxis Mongodb
#' 
#' Armamos el cuerpo de la consulta con sintaxis de mongodb
#' 
query = c('{ "$or": [ 
                    {"home_team": "Real Madrid"}
                  , {"away_team": "Real Madrid"}
                  ],
             "date": "2015-12-20"
        }')

#' 
#' Realizamos la consulta y 'find' convierte el resultado de la colección a
#' dataframe
#' 
q.response <- connection$find(query)

#' 
#' Notamos que el Real Madrid solo jugó como local; contamos los goles y
#' vemos quien fue el equipo contrincante
#' 
n.goles  <- 
    q.response %>% 
    filter(home_team == "Real Madrid") %>% 
    pull(home_score) %>% 
    sum()

vs.team  <-
    q.response %>% 
    filter(home_team == "Real Madrid") %>%
    pull(away_team)

#' 
#' Vemos los resultados
#' 
cat(paste( "Cantidad de goles metidos el 20-12-2015 por el Real Madrid: "
          , n.goles , "\n"
          , "Contra el equipo: "
          , vs.team
          , sep = ""))
if (n.goles > 4){cat(" ¡¡Fue una goleada!! ")}

#' 
#' ## 📋 Cerrar Conexión
#' 
#' Desconectamos la conexión
#' 
connection$disconnect()

#' 
#' Referencias:
#' 
#' <https://cran.r-project.org/web/packages/mongolite/mongolite.pdf>
#' 
#' <https://jeroen.github.io/mongolite/query-data.html>
#' 
#' <https://jeroen.github.io/mongolite/manipulate-data.html>
#' 
#' # 📂 POSTWORK 8 {#p800}
#' 
#' Link para la aplicacion Shiny:
#' <https://omar-magaa.shinyapps.io/Postwork8/>
#' 
#' Debido a la identación y anidación que se genera al hacer el código se
#' presenta por bloques comentado
#' 
#' ## 📋 Creamos el logo para la aplicación Shiny
#' 
## library(shiny)
## library(shinydashboard)
## library(dashboardthemes)
## library(shinythemes)
## library(plotly)
## 
## customLogo <- shinyDashboardLogoDIY(
##     boldText = tags$a("Equipo 10",href="https://github.com/napoleonleal/R-BEDU-Project")
##     , mainText = ""
##     , textSize = 16
##     , badgeText = "BEDU"
##     , badgeTextColor = "white"
##     , badgeTextSize = 3
##     , badgeBackColor = "#000000"
##     , badgeBorderRadius = 5
## )

#' 
#' ## 📋 Definimos la UI para la aplicacion
#' 
## ui <- fluidPage(
##     # Creamos la pagina con un dashboard
##     dashboardPage( title = "Equipo 10",
##                    # Definimos el header de la pagina
##                    dashboardHeader(title = customLogo
##                    ),
##                    # Creamos una SideBar y definimos los elementos que contendra
##                    dashboardSidebar(
##                        sidebarMenu(
##                            menuItem(  "Inicio"
##                                     , tabName = "home"
##                                     , icon = icon("home")),
##                            menuItem(  "Graficas de barras"
##                                     , tabName = "graficas"
##                                     , icon = icon("bar-chart")),
##                            menuItem(  "Goles casa - visitante"
##                                     , tabName = "post3"
##                                     , icon = icon("area-chart")),
##                            menuItem(  "Data Table"
##                                     , tabName = "data_table"
##                                     , icon = icon("table")),
##                            menuItem(  "Factores de Ganacias"
##                                     , tabName = "ganacias"
##                                     , icon = icon("money")),
##                            menuItem(  "Repositorios"
##                                     , tabName = "gh"
##                                     , icon = icon("github"))
##                        )
##                    ),

#' 
##                    # Definimos el body del dashboar
##                    dashboardBody(
##                        shinyDashboardThemes(
##                           # Especificamos el tema que vamos a utlizar para la aplicacion
##                            theme = "purple_gradient"
##                        ),
##                        # Definimos el cuerpo para cada tab del menu
##                        tabItems(
##                            # Inicio
##                            tabItem(tabName = "home",
##                                    fluidRow(
##                                        column(8, align="center", offset = 2,
##                                               strong( h1("BIENVENIDO AL SHINY DEL EQUIPO 10"),
##                                                       h2("AQUI PODRÁS INTERACTUAR CON NUESTROS RESULTADOS"),
##                                                       tags$br(),
##                                                       h2("INTEGRANTES DEL EQUIPO: "),
##                                                       h3("María Magdalena Castro Sam"),
##                                                       h3("Sergio Napoleón Leal"),
##                                                       h3("Jesús Omar Magaña Medina"),
##                                                       h3("Fernando Itzama Novales Campos"),
##                                                       h3("Adrián Ramírez Cortés"),
##                                                       h3("Efraín Soto Olmos")
##                                                     ),
##                                               img(src = "blob.png", height = 250, width = 250),
##                                               tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
##                                        )
##                                    )
##                            ),

#' 
##                            # Grafica de barras de los goles
##                            tabItem(tabName = "graficas",
##                                    fluidRow(
##                                      column(8, align="center",
##                                             offset = 2,
##                                             h1("Goles a favor y en contra por equipo")),
##                                      selectInput("x", "Selecciona el valor de x",
##                                                  choices = c("Goles Locales",
##                                                              "Goles Visitantes")),
##                                      plotlyOutput("grafica8",height = 800)
##                                    ),
##                            ),

#' 
##                            # Graficas Postwork 3
##                            tabItem(tabName = "post3",
##                                    fluidRow(
##                                        column(8, align="center", offset = 2,
##                                               strong( h1("Gráficas del PostWork 3"))),
##                                        box(title = "P(x) Marginal Equipo Local meta Gol",
##                                            plotlyOutput("plotPW1")),
##                                        box(title = "P(y) Marginal Equipo Visitante meta Gol",
##                                            plotlyOutput("plotPW2")),
##                                        box(title = "Figura 3.3 P(x∩y) conjunta",
##                                            plotlyOutput("plotPW3"))
##                                    )
##                            ),

#' 
##                            # Data table del archivo match.data.csv
##                            tabItem(tabName = "data_table",
##                                    fluidRow(
##                                      column(8, align="center",
##                                             offset = 2,
##                                             titlePanel(h3("Data Table de la Liga Española"))),
##                                        dataTableOutput ("data_table")
##                                    )
##                            ),

#' 
##                            # Grafica de Momios
##                            tabItem(tabName = "ganacias",
##                                    fluidRow(
##                                        column(8, align="center", offset = 2,
##                                               strong(
##                                                 h1("Gráficas de los factores de ganancia mínimo y máximo"))),
##                                        column(9, align="right", offset = 2,
##                                               radioButtons("picture", "Tipo de momios:",
##                                                           c("Escenario con momios máximos",
##                                                             "Escenario con momios promedio"))
##                                               ),imageOutput("imagenMomios")
##                                    )
##                            ),

#' 
##                            # Repositorios
##                            tabItem(tabName = "gh",
##                                    fluidRow(
##                                       box(title= "Repositorio version GitHub",
##                                           tags$a(img(src= "git.png",
##                                                      height = 320, width = 580),
##                                                      href= "https://github.com/napoleonleal/R-BEDU-Project")),
##                                       box(title="Repositorio version HTML",
##                                           tags$a(img(src= "page.png", height = 320, width = 580),
##                                                     href= "https://itzamango.github.io/postwork-equipo-10/"))
##                                    )
##                            )
##                        )
##                    )
##     )
## )

#' 
#' ## 📋 Definimos las funciones del servidor
#' 
## server <- function(input, output) {
## 
##     library(ggplot2)
##     library(dplyr)
##     library(viridis)
##     library(viridisLite)
## 
##     # Leemos el archivo de los resultados de los partidos de la liga española desde 2010
##     # hasta 2020
##     df.resultado = read.csv("https://raw.githubusercontent.com/omar17md/ejemplo_1/main/df_resultado.csv")

#' 
##     #Gráfico de goles
##     output$grafica8 <- renderPlotly({
##       if(input$x == "Goles Locales"){
##         p =  ggplot(df.resultado, aes( x = home.score ) ) +
##           geom_bar(aes(fill = Result)) +
##           facet_wrap( ~ away.team ) +
##           labs(y = "Goles")+
##           scale_fill_discrete(name = "Resultados",
##                               labels = c("Gano Visitante",
##                                          "Empate", "Gano Local"))
##         #  +scale_y_continuous(limits = c(0,50))
## 
##       }else{
##         p =  ggplot(df.resultado, aes( x = away.score ) ) +
##           geom_bar(aes(fill = Result)) +
##           facet_wrap( ~ away.team ) +
##           labs(y = "Goles")+
##           scale_fill_discrete(name = "Resultados",
##                               labels = c("Gano Visitante",
##                                          "Empate", "Gano Local"))
##         #  +scale_y_continuous(limits = c(0,50))
##       }
## 
## 
##       ggplotly(p)
## 
##     })

#' 
##     # Función para obtener la frecuencia y probabilidad
##     get.prob.df<- function(data, team, name.gol, name.prob){
##         team      <- enquo(team)
##         data %>%
##             pull(!!team) %>%
##             table(., dnn = (name.gol)) %>%
##             as.data.frame %>%
##             mutate(!!name.prob := Freq/sum(Freq))
##     }
## 
##     # Función para redondear 3 digitos
##     get.round <- function(data, digits){
##         data %>% mutate_if(is.numeric, round, digits=digits)
##     }
## 
##     # Función para obtener la probabilidad conjunta en objeto tabla
##     get.prob.joint.tbl <- function(data, team.h, team.a, name.h, name.a){
##       data %>%
##         {table( team.h,  team.a, dnn = c(name.h, name.a)) } %>%
##         prop.table
##     }
## 

#' 
##     # Gráficas del postwork3
##     # Leemos el archivo que se genero en el postwork2
##     data <- "https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_2/Postwork_02.csv" %>%
##         read.csv %>%
##         mutate(Date = as.Date(Date, "%Y-%m-%d"))
## 
##     #  calculamos la probabilidad marginal de los goles metidos por locales:
##     prob.m.local <-
##         get.prob.df(data, FTHG, "Gol.Home", "P.Marginal")
## 
##     prob.m.local %>% get.round(3)
## 
##     # Creamos una función para gráficar los datos
##     plot.bar <- function(data, x.lab, y.lab, f.lab, title){
##         Goles         <- data[1] %>% unlist()
##         Porcentaje    <- (data[3]*100 ) %>% unlist %>% round(., digits=2)
##         Prob.Marginal <- data[3] %>% unlist() %>% round(., digits=4)
##         data %>%
##             ggplot() +
##             geom_bar(stat = 'identity') +
##             aes(   x = Goles
##                    , y = Porcentaje
##                    , fill = Porcentaje
##                    , text = paste("Prob Marginal", Prob.Marginal,
##                                   group = interaction(Goles, Porcentaje))
##             ) +
##             labs(  x     = x.lab
##                    , y     = y.lab
##                    , fill  = f.lab
##                    , title = title
##             ) +
##             theme_minimal() +
##             theme( text = element_text(size=15)
##                    ,legend.title = element_text(size=10)
##             ) +
##             scale_fill_viridis(name=f.lab, direction = 1)
##     }
## 

#' 
##    #Primera grafica del postwork3
##    output$plotPW1 <- renderPlotly({
## 
##        plot.local = plot.bar(prob.m.local
##                               , "Goles [n]"
##                               , "Probabilidad de ocurrencia [%]"
##                               , "%"
##                               , "Probabilidad de anotación del equipo local" )
##        ggplotly(plot.local)
##    })
## 
## 
## 
##     prob.m.visit <-
##       get.prob.df(data, FTAG, "Gol.Away", "P.Marginal")
## 
## 
##     prob.m.visit %>% get.round(3)
## 
##     #Segunda grafica del postwork 3
##     output$plotPW2 <- renderPlotly({
##       plot.visit <- plot.bar(prob.m.visit
##                              , "Goles [n]"
##                              , "Probabilidad de ocurrencia [%]"
##                              , "%"
##                              , "Probabilidad de anotación del equipo visitante" )
## 
##          ggplotly(plot.visit)
##     })

#' 
##     # Calculamos la probabilidad conjunta de que el equipo que juega en casa anote *'x'*
##     # goles y el equipo que juega como visitante anote '*y'* goles:
##     prob.joint <-
##       get.prob.joint.tbl(data, data$FTHG, data$FTAG, "Local", "Visitante")
## 
##     prob.joint  %<>% "*"(100) %>% round(2)
## 
##     prob.joint %<>% as.data.frame() %>% rename(Probabilidad = Freq)
## 
##     # Creamos la fucion para graficar el mapa de calor
##     plot.heat <- function(data, x.lab, y.lab, f.lab, title){
##       ggplot(data) +
##         aes(   Local
##                , Visitante
##                , fill = Probabilidad
##         ) + #gráficamos
##         geom_raster() +
##         labs(  x     = x.lab
##                , y     = y.lab
##                , fill  = f.lab
##                , title = title
##         ) +
##         theme(  text = element_text(size=18)
##                 , legend.title = element_text(size=15)
##         ) +
##         scale_fill_viridis(  name=f.lab
##                              , direction = 1 #, option = "H"
##         )
##     }

#' 
##     # Tercera grafica del postwork 3
##     output$plotPW3 <- renderPlotly({
##       plot.mapa.calor <-
##         plot.heat(  prob.joint
##                     , "Local [goles]"
##                     , "Visitante [goles]"
##                     , "Probabilidad [%]"
##                     , "Probabilidad Conjunta de anotación"
##         )
## 
##       ggplotly(plot.mapa.calor)    #versión interactiva
##     })
## 

#' 
##     #Data Table
##     df = read.csv("https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_8/match.data.csv")
##     output$data_table <- renderDataTable( {df},
##                                           options = list(aLengthMenu = c(5,25,50),
##                                                          iDisplayLength = 50)
##     )
## 
##     # Imagenes momios
##     output$imagenMomios <- renderImage({
##         if(input$picture == "Escenario con momios máximos"){
##             list(src = "www/momios1.png", height = 520, width = 1200)
##         }
##         else if(input$picture == "Escenario con momios promedio"){
##             list(src = "www/momios2.png", height = 520, width = 1200)
##         }
##     }, deleteFile = FALSE)
## }
## 
## # Corremos la aplicacion
## shinyApp(ui = ui, server = server)

#' 
#' # 🏁 CONCLUSIONES FINALES
#' 
#' En la elaboración del proyecto fue evidente que en el área de análisis
#' de datos es necesario no sólo conocimientos de programación sino también
#' un entendimiento claro de conceptos matemáticos y estadísticos y sobre
#' el área del negocio para poder dar una interpretación adecuada a los
#' datos, puesto que de otra manera podría prestarse a conclusiones
#' erróneas o que no aportan valor a los objetivos.
#' 
#' El análisis de las variables juega un papel importante, en nuestro caso
#' el descubrir que las variables de los goles no son independientes dio
#' pie a buscar un análisis más profundo, ya que en un partido hay muchos
#' factores que intervienen tales como las tarjetas amarillas o rojas, una
#' lesión que ponga a un jugador fuera de juego, partidos arreglados,
#' corrupción, entre muchos otros.
#' 
#' Por esta razón asegurar la victoria solo basados en los goles metidos
#' sería erróneo. Por lo tanto podemos concluir que los datos solo pueden
#' dar una predicción segura con cierto margen de confianza hasta cierto
#' punto y es necesario tomar en cuenta muchos elementos.
#' 
#' El análisis de datos en el sector del deporte es una industria creciente
#' pues posee gran potencial. En el caso del presente trabajo, el saber que
#' los modelos son acertados en cuanto a predecir las probabilidades de
#' ganar o perder en algún partido posee gran importancia pues puede dar
#' pie a generar estrategias para planificar el entrenamiento o poner
#' énfasis en las jugadas frente a cierto equipo, o bien a generar apuesta
#' con un porcentaje de riesgo relativamente bajo.
#' 
#' Identificar las variables dependientes es una parte importante, puesto
#' que a partir de la relación entre ellas se encontrarán patrones y
#' tendencias. Identificarlas para hacer una propuesta a partir de ello va
#' a dar valor a los datos. De igual manera realizar series de tiempo y
#' saber en qué situaciones aplicarlas es de gran importancia ya que puede
#' dar un estimado sobre las variables estacionales que se pueden presentar
#' en los datos y poder hacer predicciones significativas.
#' 
#' Otros datos importantes que pueden ayudar en el contexto del fútbol son
#' los análisis físicos puesto que a través de estas estadísticas se pueden
#' identificar puntos de mejora para los jugadores. La comunicación de
#' estas estadísticas físicas permite tener un mejor entendimiento entre
#' los miembros y al conocerse mejor se puede tener una mejor dinámica de
#' equipo lo cual es de vital importancia en el futbol, así como ayudar a
#' los jugadores a conocer áreas de potencial mejora y entrenamiento. Los
#' análisis sobre las jugadas y pases durante los partidos pueden ayudar a
#' tomar decisiones tácticas y a planear mejores entrenamientos para tener
#' un mejor rendimiento en la cancha.
#' 
#' A pesar de que en el futbol existen muchas variables impredecibles que
#' influyen en el resultado de un partido, el análisis de los datos puede
#' ser de ayuda para obtener estadísticas importantes a tomar en cuenta
#' para fijar cursos de acción futuros. El correcto análisis de los datos
#' puede ayudar a que se puedan puedan tomar decisiones mas informadas y
#' objetivas, tomando en cuenta el contexto de los jugadores y del pánorama
#' deportivo actual.
