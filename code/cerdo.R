# https://en.wikipedia.org/wiki/Pig_(dice_game)

library(tidyverse)

########################################################################
# Simulamos la evolucion de un jugador que tira como mucho 5 veces
########################################################################

max_tiradas <- 5
total_puntos <- 0
evolu_puntos <- numeric()

while(total_puntos < 100){
  parcial <- 0
  tiradas <- 0
  while(tiradas <= max_tiradas){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial <- 0
      break
    }else{
      parcial <- parcial + resultado    
      tiradas <- tiradas + 1
      if(total_puntos + parcial >= 100) break
    }
  }
  evolu_puntos <- append(evolu_puntos, parcial)
  total_puntos <- total_puntos + parcial
}

print(paste0("Ha tardado ", length(evolu_puntos), " rondas en ganar"))

plot(cumsum(evolu_puntos), 
     type = "l",
     main = "Una partida de TIRA5",
     xlab="Ronda", 
     ylab="Puntos")


########################################################################
# Dos jugadores, uno tira 5 y otro tira 3
########################################################################

max_tiradas1 <- 5
max_tiradas2 <- 3

total_puntos1 <- 0
total_puntos2 <- 0

evolu_puntos1 <- numeric()
evolu_puntos2 <- numeric()

while(total_puntos1 < 100 & total_puntos2 < 100){
  
  parcial1 <- 0
  tiradas1 <- 0
  while(tiradas1 <= max_tiradas1){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial1 <- 0
      break
    }else{
      parcial1 <- parcial1 + resultado    
      tiradas1 <- tiradas1 + 1
      if(total_puntos1 + parcial1 >= 100) break
    }
  }
  evolu_puntos1 <- append(evolu_puntos1, parcial1)
  total_puntos1 <- total_puntos1 + parcial1
  
  parcial2 <- 0
  tiradas2 <- 0
  while(tiradas2 <= max_tiradas2){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial2 <- 0
      break
    }else{
      parcial2 <- parcial2 + resultado    
      tiradas2 <- tiradas2 + 1
      if(total_puntos2 + parcial2 >= 100) break
    }
  }
  evolu_puntos2 <- append(evolu_puntos2, parcial2)
  total_puntos2 <- total_puntos2 + parcial2
  
  
}

ganador <- ifelse(total_puntos1 >= 100 & total_puntos2 >= 100, 
                  "Empate", 
                  ifelse(total_puntos1 > total_puntos2, "TIRA5", "TIRA3"))


print(paste0("Ha ganado el jugador ", ganador, " en ", length(evolu_puntos1), " rondas."))


ggplot() +
  geom_line(aes(x = 1:length(evolu_puntos1), y = cumsum(evolu_puntos1), col = "TIRA5")) +
  geom_line(aes(x = 1:length(evolu_puntos2), y = cumsum(evolu_puntos2), col = "TIRA3")) +
  scale_x_continuous(breaks=1:max(length(evolu_puntos1), length(evolu_puntos2))) +
  labs(title = "TIRA5 contra TIRA3",
       colour = "",
       x = "Ronda", 
       y = "Puntos")
  

########################################################################
# ¿Es esto suficiente para demostrarlo?
# Simulamos 2000 partidas
########################################################################

max_tiradas1 <- 5
max_tiradas2 <- 3

partidas <- 2000
resultados <- tibble(partida = numeric(), ganador = character())


for (i in 1:partidas)
{
  total_puntos1 <- 0
  total_puntos2 <- 0
  while(total_puntos1 < 100 & total_puntos2 < 100){
    
    parcial1 <- 0
    tiradas1 <- 0
    while(tiradas1 <= max_tiradas1){
      resultado <- sample(1:6, 1)
      if (resultado == 1) {
        parcial1 <- 0
        break
      }else{
        parcial1 <- parcial1 + resultado    
        tiradas1 <- tiradas1 + 1
        if(total_puntos1 + parcial1 >= 100) break
      }
    }
    total_puntos1 <- total_puntos1 + parcial1
    
    parcial2 <- 0
    tiradas2 <- 0
    while(tiradas2 <= max_tiradas2){
      resultado <- sample(1:6, 1)
      if (resultado == 1) {
        parcial2 <- 0
        break
      }else{
        parcial2 <- parcial2 + resultado    
        tiradas2 <- tiradas2 + 1
        if(total_puntos2 + parcial2 >= 100) break
      }
    }
    total_puntos2 <- total_puntos2 + parcial2
  }
  
  resultados <- add_row(resultados,
                        partida = i,
                        ganador = ifelse(total_puntos1 >= 100 & total_puntos2 >= 100, 
                                         "Empate", 
                                         ifelse(total_puntos1 > total_puntos2, "TIRA5", "TIRA3")))
  
}

table(resultados$ganador)

########################################################################
# El jugador CONSIGUE20
########################################################################

total_puntos <- 0
evolu_puntos <- numeric()
consigue <- 20

while(total_puntos < 100){
  parcial <- 0
  tiradas <- 0
  while(parcial < consigue){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial <- 0
      break
    }else{
      parcial <- parcial + resultado    
      tiradas <- tiradas + 1
      if(total_puntos + parcial >= 100) break
    }
  }
  evolu_puntos <- append(evolu_puntos, parcial)
  total_puntos <- total_puntos + parcial
}

print(paste0("Ha tardado ", length(evolu_puntos), " rondas en ganar"))

plot(cumsum(evolu_puntos), 
     type = "l",
     main = "Una partida de CONSIGUE20",
     xlab="Ronda", 
     ylab="Puntos")

############################################################
# TIRA5 contra CONSIGUE20: Una partida
############################################################

max_tiradas1 <- 5
consigue2 <- 20

total_puntos1 <- 0
total_puntos2 <- 0

evolu_puntos1 <- numeric()
evolu_puntos2 <- numeric()

while(total_puntos1 < 100 & total_puntos2 < 100){
  
  parcial1 <- 0
  tiradas1 <- 0
  while(tiradas1 <= max_tiradas1){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial1 <- 0
      break
    }else{
      parcial1 <- parcial1 + resultado    
      tiradas1 <- tiradas1 + 1
      if(total_puntos1 + parcial1 >= 100) break
    }
  }
  evolu_puntos1 <- append(evolu_puntos1, parcial1)
  total_puntos1 <- total_puntos1 + parcial1
  
  parcial2 <- 0
  tiradas2 <- 0
  while(parcial2 < consigue2){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial2 <- 0
      break
    }else{
      parcial2 <- parcial2 + resultado    
      tiradas2 <- tiradas2 + 1
      if(total_puntos2 + parcial2 >= 100) break
    }
  }
  evolu_puntos2 <- append(evolu_puntos2, parcial2)
  total_puntos2 <- total_puntos2 + parcial2
  
  
}

ganador <- ifelse(total_puntos1 >= 100 & total_puntos2 >= 100, 
                  "Empate", 
                  ifelse(total_puntos1 > total_puntos2, "TIRA5", "CONSIGUE20"))


print(paste0("Ha ganado el jugador ", ganador, " en ",length(evolu_puntos1) , " rondas."))

ggplot() +
  geom_line(aes(x = 1:length(evolu_puntos1), y = cumsum(evolu_puntos1), col = "TIRA5")) +
  geom_line(aes(x = 1:length(evolu_puntos2), y = cumsum(evolu_puntos2), col = "CONSIGUE20")) +
  scale_x_continuous(breaks=1:max(length(evolu_puntos1), length(evolu_puntos2)))+
  labs(title = "TIRA5 contra CONSIGUE20",
       colour = "",
       x = "Ronda", 
       y = "Puntos")

############################################################
# TIRA5 contra CONSIGUE20: 2000 partidas
############################################################

partidas <- 2000
max_tiradas1 <- 5
consigue2 <- 20

resultados <- tibble(partida = numeric(), ganador = character())


for (i in 1:partidas)
{
  total_puntos1 <- 0
  total_puntos2 <- 0
  
  while(total_puntos1 < 100 & total_puntos2 < 100){
    
    parcial1 <- 0
    tiradas1 <- 0
    while(tiradas1 <= max_tiradas1){
      resultado <- sample(1:6, 1)
      if (resultado == 1) {
        parcial1 <- 0
        break
      }else{
        parcial1 <- parcial1 + resultado    
        tiradas1 <- tiradas1 + 1
        if(total_puntos1 + parcial1 >= 100) break
      }
    }
    evolu_puntos1 <- append(evolu_puntos1, parcial1)
    total_puntos1 <- total_puntos1 + parcial1
    
    parcial2 <- 0
    tiradas2 <- 0
    while(parcial2 < consigue2){
      resultado <- sample(1:6, 1)
      if (resultado == 1) {
        parcial2 <- 0
        break
      }else{
        parcial2 <- parcial2 + resultado    
        tiradas2 <- tiradas2 + 1
        if(total_puntos2 + parcial2 >= 100) break
      }
    }
    evolu_puntos2 <- append(evolu_puntos2, parcial2)
    total_puntos2 <- total_puntos2 + parcial2
    
    
  }
  
  
  resultados <- add_row(resultados,
                        partida = i,
                        ganador = ifelse(total_puntos1 >= 100 & total_puntos2 >= 100, 
                                         "Empate", 
                                         ifelse(total_puntos1 > total_puntos2, "TIRA5", "CONSIGUE20")))
  
}


table(resultados$ganador)


