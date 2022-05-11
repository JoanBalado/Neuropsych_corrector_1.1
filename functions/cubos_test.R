library(stringr)

# Dades
data_ud <- read.csv(file.choose(), header = T)

data_ud_cc <- data_ud[,c('cc', 'ccet', 'edad', 'sexo', 'est' )]

data_ud_cc$sexo <- factor(data_ud_cm$sexo, 1:2, c('Masculino', 'Femenino'))

##################
### Tests cmt ###
################

correction <- c()

for(i in 1:nrow(data_ud_cc)){
  correction[i] <- cubos_fun(data_ud_cm$cc[i], data_ud_cm$edad[i])
  print(i)
}

plot(data_ud_cm$ccet, correction)

summary(correction)
summary(data_ud_cm$ccet)