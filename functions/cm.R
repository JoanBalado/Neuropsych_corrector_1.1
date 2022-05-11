library(stringr)

# Dades
data_ud <- read.csv(file.choose(), header = T)

data_ud_cm <- data_ud[,c('cm', 'cmt', 'mcwmsiii', 'mcwiiipt', 'edad', 'sexo', 'est' )]

data_ud_cm$sexo <- factor(data_ud_cm$sexo, 1:2, c('Masculino', 'Femenino'))

##################
### Tests cmt ###
################

correction <- c()

for(i in 1:nrow(data_ud_cm)){
  correction[i] <- cm_fun(data_ud_cm$cm[i], data_ud_cm$edad[i], data_ud_cm$sexo[i])
  print(i)
}

plot(data_ud_cm$cmt, correction)

summary(correction)
summary(data_ud_cm$cmt)

########################
### Tests mcwms-iii ###
######################

correction <- c()

for(i in 1:nrow(data_ud_cm)){
  correction[i] <- mcwmsiii_fun(data_ud_cm$mcwmsiii[i], data_ud_cm$edad[i])
  print(i)
}

plot(data_ud_cm$mcwiiipt, correction)

summary(correction)
summary(data_ud_cm$mcwiiipt)