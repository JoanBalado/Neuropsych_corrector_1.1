library(stringr)

# Dades
data_ud <- read.csv(file.choose(), header = T)

target <- paste(c('fsl', '15obj', 'hoop', 'jlo', 'clockr'), collapse = '|')
target2 <- names(data_ud)[str_detect(names(data_ud),  regex(target, ignore_case = T))]

data_ud_viso <- data_ud[,c(target2, 'edad', 'sexo', 'est' )]

data_ud_viso$sexo <- factor(data_ud_viso$sexo, 1:2, c('Masculino', 'Femenino'))

#####################
### Tests BNT ######
###################

correction <- c()

for(i in 1:nrow(data_ud_viso)){
  correction[i] <- fsl_fun(data_ud_viso$fsl[i])
  print(i)
}

data_ud_viso$fslt <- ifelse(data_ud_viso$fslt < 0, 0, data_ud_viso$fslt)
plot(data_ud_viso$fslt, correction)

summary(correction)
summary(data_ud_viso$fslt)



#########################
### Tests Hoopper ######
#######################

correction <- c()

for(i in 1:nrow(data_ud_viso)){
  correction[i] <- hooper_fun(data_ud_viso$hooper[i], data_ud_viso$edad[i], data_ud_viso$est[i])
  print(i)
}

plot(data_ud_viso$hoopct, correction)

summary(correction)
summary(data_ud_viso$hoopct)



#####################
### Tests JLO ######
###################

correction <- c()

for(i in 1:nrow(data_ud_viso)){
  correction[i] <- jlo_fun(data_ud_viso$jlo[i], data_ud_viso$edad[i], data_ud_viso$sexo[i])
  print(i)
}

library(ggplot2)
ggplot(data_ud_viso, aes(jlocst, correction)) + geom_jitter(alpha = 0.3)

summary(correction)
summary(data_ud_viso$jlocst)

View(cbind(data_ud_viso$jlocst, correction, correction - data_ud_viso$jlocst, data_ud_viso$jlo, data_ud_viso$sexo, data_ud_viso$edad))


###################
### CLOCK R ######
#################

# en el merge no aparece clockr...
data_clockr <- foreign::read.spss(file.choose(), to.data.frame = T, use.value.labels = T)

correction <- c()

for(i in 1:nrow(data_clockr)){
  correction[i] <- clockr_fun(data_clockr$clockr[i], data_clockr$edad[i])
  print(i)
}

library(ggplot2)
ggplot(data_clockr, aes(clockrt2, correction)) + geom_point(alpha = 0.3)

summary(correction)
summary(data_clockr$clockr)

View(cbind(data_ud_viso$jlocst, correction, correction - data_ud_viso$jlocst, data_ud_viso$jlo, data_ud_viso$sexo, data_ud_viso$edad))