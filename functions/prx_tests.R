library(stringr)

# Dades
data_ud <- read.csv(file.choose(), header = T)

target <- paste(c('imitd', 'imiti', 'imitb', 'secd', 'coord', 'secmot', 'secgraf'), collapse = '|')
target2 <- names(data_ud)[str_detect(names(data_ud),  regex(target, ignore_case = T))]

data_ud_prx <- data_ud[,c(target2, 'edad', 'sexo', 'est' )]

data_ud_prx$sexo <- factor(data_ud_prx$sexo, 1:2, c('Masculino', 'Femenino'))

####################
### Tests imitd ###
##################

correction <- c()

for(i in 1:nrow(data_ud_prx)){
  correction[i] <- imitd_fun(data_ud_prx$imitd[i], data_ud_prx$edad[i], data_ud_prx$est[i])
  print(i)
}

plot(data_ud_prx$imitdt, correction)

summary(correction)
summary(data_ud_prx$imitdt)


####################
### Tests imiti ###
##################

correction <- c()

for(i in 1:nrow(data_ud_prx)){
  correction[i] <- imiti_fun(data_ud_prx$imiti[i], data_ud_prx$edad[i], data_ud_prx$est[i])
  print(i)
}

plot(data_ud_prx$imitit, correction)

summary(correction)
summary(data_ud_prx$imitit)


####################
### Tests imitb ###
##################

correction <- c()

for(i in 1:nrow(data_ud_prx)){
  correction[i] <- imitb_fun(data_ud_prx$imitb[i], data_ud_prx$edad[i], data_ud_prx$est[i])
  print(i)
}

plot(data_ud_prx$imitbt, correction)

summary(correction)
summary(data_ud_prx$imitbt)

####################
### Tests coord ###
##################

correction <- c()

for(i in 1:nrow(data_ud_prx)){
  correction[i] <- coord_fun(data_ud_prx$coord[i], data_ud_prx$edad[i], data_ud_prx$est[i])
  print(i)
}

plot(data_ud_prx$coordt, correction)

summary(correction)
summary(data_ud_prx$coordt)

#########################
### Tests secuencias ###
#######################

correction <- c()

for(i in 1:nrow(data_ud_prx)){
  correction[i] <- secpos_fun(data_ud_prx$secd[i], data_ud_prx$edad[i], data_ud_prx$est[i])
  print(i)
}

plot(data_ud_prx$secdt, correction)

summary(correction)
summary(data_ud_prx$secdt)