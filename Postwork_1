df <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

total_partirdos = length(df$FTHG)


goles.local <-table(df$FTHG)

prob.marginal = goles.local / total_partirdos

table.local = data.frame(goles.local, prob.marginal, check.names = T)[-3]

colnames(table.local) = c("Goles", "Frecuencia", "P. Marginal")


goles.visitante <-table(df$FTAG)

prob.marginal = goles.visitante / total_partirdos

table.visitante = data.frame(goles.visitante, prob.marginal, check.names = T)[-3]

colnames(table.visitante) = c("Goles", "Frecuencia", "P. Marginal")



goles.partidos = table(df$FTHG, df$FTAG, dnn=c("x", "y"))

prob.conjunta = prop.table(goles.partidos)

prob.conjunta <- round(prob.conjunta * 100,2)
