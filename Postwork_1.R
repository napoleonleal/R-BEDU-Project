#Postwork 1
#Éstos códigos analizan los datos de la temporada 2019/2020 de 
#la primera división de la liga española
# los datos fueron obtenidos de https://www.football-data.co.uk/spainm.php
#Informacion sobre los datos puede encontrarse en https://www.football-data.co.uk/notes.txt

#Importamos datos
df <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

#Extraemos la columna para los goles locales
#Con table generamos una tabla que nos indica la frecuencia de los goles 
goles.local <-table(df$FTHG)

#La probabilidad marginal es la probabilidad de que ocurra un evento simple, por lo tanto
#estará dada por el número de goles sobre el total de los partidos

#Calculamos el total de los partidos
total_partidos = length(df$FTHG)

#La probabilidad estará dada por
prob.marginal = goles.local / total_partidos

table.local = data.frame(goles.local, prob.marginal, check.names = T)[-3]

#Renombramos las columnas
colnames(table.local) = c("Goles", "Frecuencia", "P. Marginal")

#En el caso de los goles visitantes el procedimiento es análogo
goles.visitante <-table(df$FTAG)

prob.marginal = goles.visitante / total_partidos

table.visitante = data.frame(goles.visitante, prob.marginal, check.names = T)[-3]

colnames(table.visitante) = c("Goles", "Frecuencia", "P. Marginal")

#La probabilidad conjunta toma en cuenta la probabilidad de dos eventos sobre el total de
#resultados posibles
goles.partidos = table(df$FTHG, df$FTAG, dnn=c("x", "y"))

#La probabilidad conjunta de que tanto el equipo en casa como el equipo visitante anoten goles 
#respetivamente está dado por
prob.conjunta = prop.table(goles.partidos)

prob.conjunta <- round(prob.conjunta * 100,2)
