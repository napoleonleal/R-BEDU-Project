
# 📂 POSTWORK 3

Importamos las bibliotecas:

``` r
library(ggplot2)
library(plotly)
library(dplyr)
library(ggplot2)
library(viridis)
library(viridisLite)
```

Con el data frame obtenido realizaremos algunas gráficas.

``` r
data <- "https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_2/Postwork_02.csv" %>%
        read.csv %>% 
        mutate(Date = as.Date(Date, "%Y-%m-%d"))
```

## 📋 Local: Probabilidad Marginal

Calcularemos la probabilidad marginal de que el equipo local anote ’*x’*
goles mediante una función previa:

``` r
prob.m.local <- 
  get.prob.df(data, FTHG, "Gol.Home", "P.Marginal") 

(prob.m.local %>% get.round(3))
```

    ##   Gol.Home Freq P.Marginal
    ## 1        0  265      0.232
    ## 2        1  373      0.327
    ## 3        2  304      0.267
    ## 4        3  128      0.112
    ## 5        4   40      0.035
    ## 6        5   22      0.019
    ## 7        6    6      0.005
    ## 8        7    1      0.001
    ## 9        8    1      0.001

## 🔢 plot.bar()

Realizamos una función de una gráfica para vizualizar los datos:

``` r
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
  theme(  text = element_text(size=15)
        , legend.title = element_text(size=10) 
        ) +
  scale_fill_viridis(name=f.lab, direction = 1) 
}
```

Realizamos una gráfica para vizualizar los datos:

``` r
plot.local <- plot.bar(prob.m.local
              , "Goles [n]"
              , "Probabilidad de ocurrencia [%]"
              , "%"
              , "Probabilidad de anotación del equipo local"
              )
```

## 📊 Local: P(x) Marginal

``` r
ggplotly(plot.local) #versión interactiva
`````

<img src="https://github.com/itzamango/postwork-equipo-10/blob/main/img/1.png?raw=true">

## 📋 Visitante: Probabilidad Marginal

Ahora calcularemos la probabilidad para el equipo visitante:

``` r
prob.m.visit <- 
  get.prob.df(data, FTAG, "Gol.Away", "P.Marginal") 
 
(prob.m.visit %>% get.round(3))
```

    ##   Gol.Away Freq P.Marginal
    ## 1        0  401      0.352
    ## 2        1  388      0.340
    ## 3        2  242      0.212
    ## 4        3   62      0.054
    ## 5        4   33      0.029
    ## 6        5   11      0.010
    ## 7        6    3      0.003

Realizamos una gráfica para vizualizar los datos:

``` r
plot.visit <- plot.bar(prob.m.visit
              , "Goles [n]"
              , "Probabilidad de ocurrencia [%]"
              , "%"
              , "Probabilidad de anotación del equipo visitante"
              )
```

## 📊 Visitante: P(y) Marginal

``` r
ggplotly(plot.visit) #versión interactiva
```

<img src="https://github.com/itzamango/postwork-equipo-10/blob/main/img/2.png?raw=true">

## 📋 Probabilidad Conjunta P(x∩y)

La probabilidad conjunta de que el equipo que juega en casa anote *‘x’*
goles y el equipo que juega como visitante anote ’*y’* goles calculada
con una función previa:

``` r
prob.joint <- 
  get.prob.joint.tbl(data, data$FTHG, data$FTAG, "Local", "Visitante") 

(prob.joint  %<>% "*"(100) %>% round(2))
```

    ##      Visitante
    ## Local     0     1     2     3     4     5     6
    ##     0  7.81  8.07  4.56  1.84  0.53  0.44  0.00
    ##     1 11.58 11.49  6.84  1.75  0.88  0.18  0.00
    ##     2  8.77  9.39  6.14  1.14  0.88  0.18  0.18
    ##     3  4.47  3.25  2.46  0.61  0.18  0.18  0.09
    ##     4  1.40  1.05  0.70  0.00  0.35  0.00  0.00
    ##     5  0.88  0.53  0.44  0.00  0.09  0.00  0.00
    ##     6  0.26  0.18  0.00  0.09  0.00  0.00  0.00
    ##     7  0.00  0.09  0.00  0.00  0.00  0.00  0.00
    ##     8  0.00  0.00  0.09  0.00  0.00  0.00  0.00

Realizamos un *heat map* con una función para visualizar los datos:

``` r
prob.joint %<>% as.data.frame() %>% rename(Probabilidad = Freq)
```

## 🔢 plot.heatmap()

``` r
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
                       ) 
}
```

Realizamos una gráfica para visualizar los datos:

``` r
plot.mapa.calor <-
plot.heatmap(  prob.joint
             , "Local [goles]"
             , "Visitante [goles]"
             , "Probabilidad [%]"
             , "Probabilidad Conjunta de anotación"
             )
```

## 📊 Heat Map P(x∩y) Conjunta

``` r
ggplotly(plot.mapa.calor)    #versión interactiva
```

<img src="https://github.com/itzamango/postwork-equipo-10/blob/main/img/3.png?raw=true">
