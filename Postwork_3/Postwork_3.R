### POSTWORK 3
# Importamos las bibliotecas:
library(ggplot2)
library(plotly)
library(dplyr)
library(ggplot2)
library(viridis)
library(viridisLite)

# Con el data frame obtenido realizaremos algunas gráficas.
data <- "https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_2/Postwork_02.csv" %>%
        read.csv %>% 
        mutate(Date = as.Date(Date, "%Y-%m-%d"))

### Local: Probabilidad Marginal

# Calcularemos la probabilidad marginal de que el equipo local anote ’x’ goles mediante una función previa:
prob.m.local <- 
  get.prob.df(data, FTHG, "Gol.Home", "P.Marginal") 

(prob.m.local %>% get.round(3))

# Realizamos una función de una gráfica para vizualizar los datos:
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

# Realizamos una gráfica para vizualizar los datos:
plot.local <- plot.bar(prob.m.local
              , "Goles [n]"
              , "Probabilidad de ocurrencia [%]"
              , "%"
              , "Probabilidad de anotación del equipo local"
              )

# Local: P(x) Marginal
ggplotly(plot.local) #versión interactiva


### Visitante: Probabilidad Marginal

# Ahora calcularemos la probabilidad para el equipo visitante:
prob.m.visit <- 
  get.prob.df(data, FTAG, "Gol.Away", "P.Marginal") 
 
(prob.m.visit %>% get.round(3))

# Realizamos una gráfica para vizualizar los datos:
plot.visit <- plot.bar(prob.m.visit
              , "Goles [n]"
              , "Probabilidad de ocurrencia [%]"
              , "%"
              , "Probabilidad de anotación del equipo visitante"
              )

# Visitante: P(y) Marginal
ggplotly(plot.visit) #versión interactiva

### Probabilidad Conjunta P(x∩y)

# La probabilidad conjunta de que el equipo que juega en casa anote ‘x’ goles
# y el equipo que juega como visitante anote ’y’ goles calculada con una función previa:
prob.joint <- 
  get.prob.joint.tbl(data, data$FTHG, data$FTAG, "Local", "Visitante") 

(prob.joint  %<>% "*"(100) %>% round(2))

# Realizamos un heat map con una función para visualizar los datos:
prob.joint %<>% as.data.frame() %>% rename(Probabilidad = Freq)

# Declaramos una funcion para graficar la probabilidad conjunta
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

# Realizamos una gráfica para visualizar los datos:
plot.mapa.calor <-
plot.heatmap(  prob.joint
             , "Local [goles]"
             , "Visitante [goles]"
             , "Probabilidad [%]"
             , "Probabilidad Conjunta de anotación"
             )
# Graficamos el heatmap
ggplotly(plot.mapa.calor)    #versión interactiva

