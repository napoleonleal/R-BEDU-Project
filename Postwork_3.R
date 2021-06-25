#POSTWORK 3
library(ggplot2)
#Ahora generaremos gráficas con la tabla que se generó en el postwork 2

#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)
goles.local <- table(data$FTHG)
goles.visitante <- table(data$FTAG)

(total_partidos <- length(data$FTHG)) #total de partidos
(pm.local <- round((goles.local/total_partidos)*100,2)) #prob marginal 
(goles.local <- data.frame(goles.local, pm.local)[-3]) 
colnames(goles.local)=c("Goles","Frecuencia", "Prob.Marginal")
goles.local

#Un gráfico de barras para las probabilidades marginales estimadas del 
#número de goles que anota el equipo de casa.
ggplot(goles.local, aes(x = Goles, y = pm.local)) + geom_bar(stat="identity")

#La probabilidad marginal de que el equipo que juega como visitante 
#anote y goles 
(pm.vis <- round((goles.visitante/total_partidos)*100,2))
goles.visitante <- data.frame(goles.visitante, pm.vis)[-3]
colnames(goles.visitante)=c("Goles","Freq", "Prob.Marginal")
goles.visitante

#Un gráfico de barras para las probabilidades marginales estimadas del número 
#de goles que anota el equipo visitante.
ggplot(goles.visitante, aes(x = Goles, y = Pmg)) + geom_bar(stat="identity")

#La probabilidad conjunta de que el equipo que juega en casa anote x goles y 
#el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)
goles.partidos = table(data$FTHG, data$FTAG, dnn=c("x", "y"))
(prob.conjunta = prop.table(goles.partidos))
(prob.conjunta <- round(prob.conjunta * 100,2))

#Un HeatMap para las probabilidades conjuntas estimadas de los números de 
#goles que anotan el equipo de casa y el equipo visitante en un partido.
str(prob.conjunta)

library(lattice)
library(plotly)
library(viridisLite)
library(grid)

#Utilizando lattice se hace el heat map
(heat.map <- as.matrix(prob.conjunta) )# volvemos la tabla una matriz
inter <-levelplot(heat.map, col.regions = viridis(100))

#agregando ejes y título
(heat.map <- as.matrix(prob.conjunta) )
levelplot(heat.map, col.regions = viridis(100), xlab= "goles local", ylab="goles invitado", main = "Mapa de calor de las probabilidades conjuntas", colorkey )
trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
grid.text("probabilidad [%]", 0.4, 1.1, hjust=0.5, vjust=1.5)
trellis.unfocus()

#Para utilizar plotly y volverlo interactivo  convertimos la matriz a df
heat.df <- as.data.frame(t(heat.map))
heat.df

p <- ggplot(heat.df, aes(x, y, fill= Freq)) +  #gráfica
  geom_raster() +
  scale_fill_viridis_c()

ggploty(p) 

