#postwork3
library(ggplot2)
setwd("C:/Users/adria/OneDrive/Bedu - Data Science/Programación y estadística en R/postwork")

#Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas 
#de frecuencias relativas para estimar las siguientes probabilidades:
df <- read.csv("postwork2.csv")

#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)

fthg <- table(df$FTHG)
ftag <- table(df$FTAG)

(partidos <- length(df$FTHG))
(pfthg <- round((fthg/partidos)*100,2))
(goles.local <- data.frame(fthg, pfthg)[-3])
colnames(goles.local)=c("Goles","Freq", "Pmg")
goles.local

ggplot(goles.local, aes(x = Goles, y = Pmg)) + geom_bar(stat="identity")


#La probabilidad (marginal) de que el equipo que juega como visitante 
#anote y goles (y=0,1,2,)
(pftag <- round((ftag/partidos)*100,2))
goles.visitante <- data.frame(ftag, pftag)[-3]
colnames(goles.visitante)=c("Goles","Freq", "Pmg")
goles.visitante

ggplot(goles.visitante, aes(x = Goles, y = Pmg)) + geom_bar(stat="identity")
