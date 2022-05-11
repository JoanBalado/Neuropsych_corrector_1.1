library(stringr)

# Dades
data_ud <- read.csv(file.choose(), header = T)

target <- paste(c('corden', 'bnt', 'dimag', 'fas'), collapse = '|')
target2 <- names(data_ud)[str_detect(names(data_ud),  regex(target, ignore_case = T))]

data_ud_leng <- data_ud[,c(target2, 'edad', 'sexo', 'est' )]

data_ud_leng$sexo <- factor(data_ud_leng$sexo, 1:2, c('Masculino', 'Femenino'))

#####################
### Tests BNT ######
###################

correction <- c()

for(i in 1:nrow(data_ud_leng)){
  correction[i] <- bnt_fun(data_ud_leng$bnt30[i], data_ud_leng$edad[i], data_ud_leng$sexo[i], data_ud_leng$est[i])
  print(i)
}

data_ud_leng$bntt <- ifelse(data_ud_leng$bntt < 0, 0, data_ud_leng$bntt)
plot(data_ud_leng$bntt, correction)

summary(correction)
summary(data_ud_leng$bntt)

View(cbind(correction, data_ud_leng$bntt, data_ud_leng$bntt-correction, data_ud_leng$est, data_ud_leng$sexo, data_ud_leng$edad))


#####################
### Tests BCN ######
###################

correction <- c()

for(i in 1:nrow(data_ud_leng)){
  correction[i] <- dimag_fun(data_ud_leng$dimag[i], data_ud_leng$edad[i], data_ud_leng$est[i])
  print(i)
}

plot(data_ud_leng$dimagt, correction)

summary(correction)
summary(data_ud_leng$dimagt)


#####################
### Tests BCN ######
###################

correction <- c()

for(i in 1:nrow(data_ud_leng)){
  correction[i] <- fas_fun(data_ud_leng$fas[i], data_ud_leng$est[i])
  print(i)
}

plot(data_ud_leng$fasct, correction)

summary(correction)
summary(data_ud_leng$fasct)