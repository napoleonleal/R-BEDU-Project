library(mongolite)
library(dplyr)

# Obtenemos el csv
match.db <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-07/Postwork/match.data.csv")

# Conectamos con la base de datos match_games y la colección match
connection <- mongo(collection = "match"
                    , db ="match_games"
                    , url = "mongodb+srv://equipo10:bedu@postwork7.8voq3.mongodb.net/test")

# Verificamos que si no hay documentos los agregue del csv
if (connection$count() == 0) {
  connection$insert(match.db)
}

# Armamnos el cuerpo de la consulta con sintaxis de mongo
query = c('{ "$or": [ 
                    {"home_team": "Real Madrid"}
                  , {"away_team": "Real Madrid"}
                  ],
             "date": "2015-12-20"
        }')

# Realizamos la consulta y find convierte el resultado de la colección a dataframe
q.result <- connection$find(query)

# Notamos que el Real Madrid solo jugó como home; 
# contamos los goles y vemos quien fue el equipo contrincante
n.goles  <- q.result %>% filter(home_team == "Real Madrid") %>% pull(home_score) %>% sum()
vs.team  <- q.result %>% filter(home_team == "Real Madrid") %>% pull(away_team)

# Vemos los resultados
print(paste(  c('Cantidad de goles metidos el 20-12-2015 por el Real Madrid:'), n.goles
             ,c('contra el equipo:'), vs.team))

print("¡¡Fue una goleada!!")

# Desconectamos la conexión
connection$disconnect()


# Referencias:
# https://cran.r-project.org/web/packages/mongolite/mongolite.pdf
# https://jeroen.github.io/mongolite/query-data.html
# https://jeroen.github.io/mongolite/manipulate-data.html