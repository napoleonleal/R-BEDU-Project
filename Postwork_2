library(dplyr)

setwd("D:/Omar/Programacion/Codigos/R/Bedu/Postwork/")

lista <- lapply(dir(), read.csv)

str(lista); head(lista); View(lista); summary(lista)

lista <- lapply(lista, select, c("Date", "HomeTeam", "AwayTeam","FTHG","FTAG","FTR")) 

data <- do.call(rbind, lista)

data <- mutate(data, Date = as.Date(Date, "%d/%m/%y"))

