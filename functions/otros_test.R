library(stringr)

# Dades
data_ud <- read.csv(file.choose(), header = T)

data_ud_otros <- data_ud[,c('sdmto', 'sdmtot', 'calc', 'calct', 'edad', 'sexo', 'est' )]

data_ud_otros$sexo <- factor(data_ud_cm$sexo, 1:2, c('Masculino', 'Femenino'))

###################
### Tests sdmt ###
#################

correction <- c()

for(i in 1:nrow(data_ud_otros)){
  correction[i] <- sdmto_fun(data_ud_otros$sdmto[i], data_ud_otros$edad[i], data_ud_otros$est[i])
  print(i)
}

plot(data_ud_otros$sdmtot, correction)

summary(correction)
summary(data_ud_otros$sdmtot)

View(cbind(data_ud_otros$sdmtot, correction, correction-data_ud_otros$sdmtot, data_ud_otros$sdmto, data_ud_otros$edad, data_ud_otros$est)) # condiciones spss jacas mal realizadas (edu)

###############
### CALCULO ##
#############


correction <- c()

for(i in 1:nrow(data_ud_otros)){
  correction[i] <- calc_fun(data_ud_otros$calc[i], data_ud_otros$edad[i], data_ud_otros$est[i])
  print(i)
}

plot(data_ud_otros$sdmtot, correction)

summary(correction)
summary(data_ud_otros$sdmtot)
