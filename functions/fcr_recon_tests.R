library(stringr)

# Dades
data_ud <- read.csv(file.choose(), header = T)

data_ud_fcr <- data_ud[,c('edad', 'sexo', 'est', 'fcrrt', 'rtpt', 'rtnt', 'rfpot', 'rfnt', 'fcrr', 'rtp', 'rtn', 'rfpo', 'rfn')]

data_ud_fcr$sexo <- factor(data_ud_wms$sexo, 1:2, c('Masculino', 'Femenino'))

###################
### Tests fcrr ###
#################

correction <- c()

for(i in 1:nrow(data_ud_fcr)){
  correction[i] <- fcr_recon(data_ud_fcr$fcrr[i], data_ud_fcr$edad[i], type = 'fcr_recon')
  print(i)
}

plot(data_ud_fcr$fcrrt, correction)

summary(data_ud_fcr$fcrrt)
summary(correction)

View(cbind(data_ud_fcr$fcrr, data_ud_fcr$fcrrt, correction, data_ud_fcr$edad, round(data_ud_fcr$fcrrt - correction, 3)))


####################
### Tests fcrtp ###
##################

correction <- c()

for(i in 1:nrow(data_ud_fcr)){
  correction[i] <- fcr_recon(data_ud_fcr$rtp[i], data_ud_fcr$edad[i], type = 'fcrtp')
  print(i)
}

plot(data_ud_fcr$rtpt, correction)

summary(data_ud_fcr$rtpt)
summary(correction)

####################
### Tests fcrfp ###
##################

correction <- c()

for(i in 1:nrow(data_ud_fcr)){
  correction[i] <- fcr_recon(data_ud_fcr$rfpo[i], data_ud_fcr$edad[i], type = 'fcrfp')
  print(i)
}

plot(data_ud_fcr$rfpot, correction)

summary(data_ud_fcr$rfpot)
summary(correction)


####################
### Tests fcrtn ###
##################

correction <- c()

for(i in 1:nrow(data_ud_fcr)){
  correction[i] <- fcr_recon(data_ud_fcr$rtn[i], data_ud_fcr$edad[i], type = 'fcrtn')
  print(i)
}

plot(data_ud_fcr$rtnt, correction)

summary(data_ud_fcr$rtnt)
summary(correction)

####################
### Tests fcrfn ###
##################

correction <- c()

for(i in 1:nrow(data_ud_fcr)){
  correction[i] <- fcr_recon(data_ud_fcr$rfn[i], data_ud_fcr$edad[i], type = 'fcrfn')
  print(i)
}

plot(data_ud_fcr$rfnt, correction)

summary(data_ud_fcr$rfnt)
summary(correction)
