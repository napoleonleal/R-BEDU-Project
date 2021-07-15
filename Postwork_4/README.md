# 📂 POSTWORK 4

Ahora obtendremos una tabla de cocientes al dividir las probabilidades
conjuntas por el producto de las probabilidades correspondientes:

$$Cocientes = \\dfrac{P\\left(x \\cap y\\right)\_{conjunta}}
              {P\\left(x\\right)\_{marginal} P\\left(y\\right)\_{marginal}} $$

``` r
data <- read.csv("https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_2/Postwork_02.csv")
```

## 📋 Probabilidad Marginal

Para la probabilidad marginal de los goles metidos por locales con una
función previa:

``` r
prob.m.local <- get.prob.df(data, FTHG, "Gol.Home", "Prob.Marginal") 

(prob.m.local %>% get.round(3))
```

    ##   Gol.Home Freq Prob.Marginal
    ## 1        0  363         0.239
    ## 2        1  503         0.331
    ## 3        2  402         0.264
    ## 4        3  157         0.103
    ## 5        4   59         0.039
    ## 6        5   27         0.018
    ## 7        6    7         0.005
    ## 8        7    1         0.001
    ## 9        8    1         0.001

Para los goles metidos por el equipo visitante:

``` r
prob.m.visit <- get.prob.df(data, FTAG, "Gol.Away", "Prob.Marginal") 

(prob.m.visit %>% get.round(3))
```

    ##   Gol.Away Freq Prob.Marginal
    ## 1        0  516         0.339
    ## 2        1  540         0.355
    ## 3        2  315         0.207
    ## 4        3   90         0.059
    ## 5        4   43         0.028
    ## 6        5   12         0.008
    ## 7        6    4         0.003

## 📋 Producto de Probabilidades Marginales

Hacemos el producto de las probabilidades marginales obtenidas con una
función previa:

``` r
tbl.local <- get.prob.m.tbl(data, FTHG, "Home")
tbl.visit <- get.prob.m.tbl(data, FTAG, "Away")

product.prob.m <- tbl.local %o% tbl.visit

(product.prob.m %>% round(3))
```

    ##     Away
    ## Home     0     1     2     3     4     5     6
    ##    0 0.081 0.085 0.049 0.014 0.007 0.002 0.001
    ##    1 0.112 0.118 0.069 0.020 0.009 0.003 0.001
    ##    2 0.090 0.094 0.055 0.016 0.007 0.002 0.001
    ##    3 0.035 0.037 0.021 0.006 0.003 0.001 0.000
    ##    4 0.013 0.014 0.008 0.002 0.001 0.000 0.000
    ##    5 0.006 0.006 0.004 0.001 0.001 0.000 0.000
    ##    6 0.002 0.002 0.001 0.000 0.000 0.000 0.000
    ##    7 0.000 0.000 0.000 0.000 0.000 0.000 0.000
    ##    8 0.000 0.000 0.000 0.000 0.000 0.000 0.000

## 📋 Probabilidad Conjunta P(x∩y)

Obtenemos la probabilidad conjunta con una función previa:

``` r
prob.conjunta <-
  get.prob.joint.tbl(data, data$FTHG, data$FTAG, "Home", "Away")

(prob.conjunta %>% round(3))
```

    ##     Away
    ## Home     0     1     2     3     4     5     6
    ##    0 0.078 0.083 0.051 0.018 0.006 0.003 0.000
    ##    1 0.112 0.123 0.062 0.022 0.009 0.002 0.001
    ##    2 0.086 0.097 0.060 0.012 0.008 0.001 0.001
    ##    3 0.039 0.032 0.022 0.006 0.003 0.001 0.001
    ##    4 0.015 0.013 0.007 0.001 0.003 0.000 0.000
    ##    5 0.007 0.005 0.005 0.000 0.001 0.000 0.000
    ##    6 0.002 0.002 0.000 0.001 0.000 0.000 0.000
    ##    7 0.000 0.001 0.000 0.000 0.000 0.000 0.000
    ##    8 0.000 0.000 0.001 0.000 0.000 0.000 0.000

## 📋 Cociente de Probabilidades

Realizamos el cociente:

``` r
cociente <- prob.conjunta/product.prob.m

(cociente %>% round(2))
```

    ##     Away
    ## Home    0    1    2    3    4    5    6
    ##    0 0.97 0.98 1.02 1.26 0.88 1.74 0.00
    ##    1 1.00 1.05 0.91 1.14 0.91 0.76 0.76
    ##    2 0.95 1.03 1.09 0.76 1.06 0.63 1.89
    ##    3 1.13 0.86 1.01 0.97 0.90 1.61 2.42
    ##    4 1.15 0.95 0.90 0.29 2.40 0.00 0.00
    ##    5 1.20 0.83 1.25 0.00 1.31 0.00 0.00
    ##    6 1.26 1.21 0.00 2.41 0.00 0.00 0.00
    ##    7 0.00 2.81 0.00 0.00 0.00 0.00 0.00
    ##    8 0.00 0.00 4.83 0.00 0.00 0.00 0.00

## 📋 Bootstrap

Para determinar si el número de goles del equipo local o el de el equipo
visitante son dependientes o independientes, realizaremos un
procedimiento de bootstrap para obtener más cocientes similares y
analizar la distribución.

Transformamos el data frame a columna para facilitar el bootstrap.

``` r
data_origin <- as.data.frame(as.vector(unlist(cociente)))

colnames(data_origin) <- "values"
```

Utilizamos la biblioteca `"rsample"` para poder hacer las muestras
bootstrap:

``` r
library(rsample)
```

Fijamos la semilla para poder reproducir los datos:

``` r
set.seed(83928782)
```

## 🔢 bootstraps() rsample

Aplicamos la función bootstraps, para generar 1000 muestras,
guardándolas en boot:

``` r
boot <- bootstraps(data_origin, times = 1000)
```

Cargamos las siguientes bibliotecas para visualizar datos:

``` r
library(purrr)
library(modeldata)
library(viridis)
library(tidyverse)
library(hrbrthemes)
library(forcats)
library(viridisLite)
```

Realizamos una función para hacer una columna de las medias muestrales
obtenidas por bootstrap:

``` r
obtener_media <- function(boot_splits) {
  data_mean         <- analysis(boot_splits)
  medias_muestrales <- mean(data_mean[,1])
  return(medias_muestrales)
}
```

## 📋 Medias Muestrales

Observamos el valor de la media de las medias muestrales tras aplicar la
función:

``` r
boot$means <- map_dbl(boot$splits, obtener_media)

length(boot$means); summary(boot$means)
```

    ## [1] 1000
    
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.4996  0.7523  0.8256  0.8288  0.9048  1.1796

## 📋 Intervalos de confianza

Comprobamos la hipótesis de que la media se encuentra en 1 con las
medias muestrales bootstrap y obtenemos el intervalo de confianza al 95%
con una prueba t:

``` r
t_boot    <- t.test(boot$means, alternative = "two.sided", mu = 1)
t_boot_ic <- round(t_boot$conf.int,3)
t_boot_ic
```

    ## [1] 0.822 0.835
    ## attr(,"conf.level")
    ## [1] 0.95

## 🔢 plot.histogram()

Realizamos una función para un histograma:

``` r
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
```

Realizamos el histograma de las medias muestrales obtenidas por
bootstrap.

``` r
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
```

## 📊 Histograma medias bootstrap

``` r
ggplotly(hist_boot)
```

<img src="https://github.com/itzamango/postwork-equipo-10/blob/7d498adad44bc306fff4238f65491e09f4578516/img/4.png?raw=true">

> La línea sólida indica la posición de la media y las punteadas, la
> posición de los límites del intervalo de confianza.

De igual modo lo hacemos para la muestra original:

``` r
t_origin <- t.test(data_origin$values, alternative = "two.sided", mu = 1)
t_origin_ic <- t_origin$conf.int %>% round(3)
t_origin_ic
```

    ## [1] 0.613 1.053
    ## attr(,"conf.level")
    ## [1] 0.95

``` r
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
```

## 📊 Histograma original

``` r
ggplotly(hist_origin)
```


<img src="https://github.com/itzamango/postwork-equipo-10/blob/7d498adad44bc306fff4238f65491e09f4578516/img/5.png?raw=true">


> La línea sólida indica la posición de la media y las punteadas, la
> posición de los límites del intervalo de confianza.

## 📋 Pruebas t

Vemos los datos de los estadísticos de las pruebas t para ambos
conjuntos de datos.

Remuestreo bootstrap:

``` r
t_boot
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  boot$means
    ## t = -50.464, df = 999, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 1
    ## 95 percent confidence interval:
    ##  0.8221057 0.8354231
    ## sample estimates:
    ## mean of x 
    ## 0.8287644

Muestras originales:

``` r
t_origin
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  data_origin$values
    ## t = -1.5194, df = 62, p-value = 0.1338
    ## alternative hypothesis: true mean is not equal to 1
    ## 95 percent confidence interval:
    ##  0.6130959 1.0527411
    ## sample estimates:
    ## mean of x 
    ## 0.8329185

## 🏁 Conclusiones

Observamos como la distribución de la media de los cocientes no era normal.
Usamos la media muestral de 1000 muestras generadas por método bootstrap y acorde
al Teorema del Límite Central observamos que la distribución de las medias muestrales
bootstrap es normal.

Por medio de una prueba de hipótesis y los intervalos de confianza vimos que 
no hay evidencia significativa para establecer que la media de los cocientes 
tienda a 1. Por tanto, podemos considerar a las variables como dependientes.
