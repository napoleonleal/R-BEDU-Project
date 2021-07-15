# Postwork 4
# Ahora obtendremos una tabla de cocientes al dividir las probabilidades 
# conjuntas por el producto de las probabilidades correspondientes:

data <- read.csv("https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_02.csv")

### Probabilidad Marginal
prob.m.local <- get.prob.df(data, FTHG, "Gol.Home", "Prob.Marginal") 

(prob.m.local %>% get.round(3))

# Para los goles metidos por el equipo visitante:
prob.m.visit <- get.prob.df(data, FTAG, "Gol.Away", "Prob.Marginal") 

(prob.m.visit %>% get.round(3))

### Producto de Probabilidades Marginales
# Hacemos el producto de las probabilidades marginales obtenidas con una función previa:
tbl.local <- get.prob.m.tbl(data, FTHG, "Home")
tbl.visit <- get.prob.m.tbl(data, FTAG, "Away")

product.prob.m <- tbl.local %o% tbl.visit

(product.prob.m %>% round(3))

### Probabilidad Conjunta P(x∩y)
prob.conjunta <-
  get.prob.joint.tbl(data, data$FTHG, data$FTAG, "Home", "Away")

(prob.conjunta %>% round(3))

### Cociente de Probabilidades
# Realizamos el cociente:
cociente <- prob.conjunta/product.prob.m

(cociente %>% round(2))

### Bootstrap
# Para determinar si el número de goles del equipo local o el de el equipo visitante son dependientes 
# o independientes, realizaremos un procedimiento de bootstrap para obtener 
# más cocientes similares y analizar la distribución.

# Transformamos el data frame a columna para facilitar el bootstrap.
data_origin <- as.data.frame(as.vector(unlist(cociente)))

colnames(data_origin) <- "values"

# Utilizamos la biblioteca "rsample" para poder hacer las muestras bootstrap:
library(rsample)

# Fijamos la semilla para poder reproducir los datos:
set.seed(83928782)

###  bootstraps() rsample
# Aplicamos la función bootstraps, para generar 1000 muestras, guardándolas en boot:
boot <- bootstraps(data_origin, times = 1000)

# Cargamos las siguientes bibliotecas para visualizar datos:
library(purrr)
library(modeldata)
library(viridis)
library(tidyverse)
library(hrbrthemes)
library(forcats)
library(viridisLite)

# Realizamos una función para hacer una columna de las medias muestrales obtenidas por bootstrap:
obtener_media <- function(boot_splits) {
  data_mean         <- analysis(boot_splits)
  medias_muestrales <- mean(data_mean[,1])
  return(medias_muestrales)
}

### Medias Muestrales
# Observamos el valor de la media de las medias muestrales tras aplicar la función:
boot$means <- map_dbl(boot$splits, obtener_media)

length(boot$means); summary(boot$means)

### Intervalos de confianza
# Comprobamos la hipótesis de que la media se encuentra en 1 con las medias muestrales bootstrap 
# y obtenemos el intervalo de confianza al 95% con una prueba t:
t_boot    <- t.test(boot$means, alternative = "two.sided", mu = 1)
t_boot_ic <- round(t_boot$conf.int,3)
t_boot_ic

### Graficamos el histograma
# Realizamos una función para un histograma:
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
    theme(  text = element_text(size=18)
          , legend.title = element_text(size=15) 
          , panel.spacing = unit(0.1, "lines")
          , strip.text.x = element_text(size = 10)
          #legend.position="none",
    )
}

# Realizamos el histograma de las medias muestrales obtenidas por bootstrap.
ic_mean_ic <- c(t_boot_ic[1], 
                mean(boot$means), 
                t_boot_ic[2])

hist_boot <- 
  plot.histogram(  boot
               , boot$means
               , t_boot_ic
               , 18
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

# Graficamos el  histograma
# La línea sólida indica la posición de la media y las punteadas, la posición de los límites del intervalo de confianza.
ggplotly(hist_boot)


# De igual modo lo hacemos para la muestra original:
t_origin <- t.test(data_origin$values, alternative = "two.sided", mu = 1)
t_origin_ic <- t_origin$conf.int %>% round(3)
t_origin_ic

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

#  Histograma original
# La línea sólida indica la posición de la media y las punteadas, la posición de los límites del intervalo de confianza
ggplotly(hist_origin)

### Pruebas t
# Vemos los datos de los estadísticos de las pruebas t para ambos conjuntos de datos.
# Remuestreo bootstrap:
t_boot

# Muestras originales:
t_origin


