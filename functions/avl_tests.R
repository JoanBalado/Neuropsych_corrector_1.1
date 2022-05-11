library(stringr)

# Dades
data_ud <- read.csv(file.choose(), header = T)
targets <- names(data_ud)[str_detect(names(data_ud), regex('a1|a2|a3|a4|a5|a66|a7', ignore_case = T))]
data_ud_wms <- data_ud[,c(targets, 'edad', 'sexo', 'est', 'r', 'rt', 'recupert', 
                          'fpr', 'apren', 'aprent', 'reconcot', 'rfpt', 'infover', 'aptot', 'aptott', 
                          'infovert', 'retriefi', 'retrieft', 'stprett','ltprrat', 'ltperr')]

data_ud_wms$sexo <- factor(data_ud_wms$sexo, 1:2, c('Masculino', 'Femenino'))

#################
### Tests a1 ###
###############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- a1_fun(data_ud_wms$a1[i], data_ud_wms$edad[i])
  print(i)
}


#################
### Tests a2 ###
###############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- a2_fun(data_ud_wms$a2[i], data_ud_wms$edad[i])
  print(i)
}

#################
### Tests a3 ###
###############


correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- a3_fun(data_ud_wms$a3[i], data_ud_wms$edad[i])
  print(i)
}


#################
### Tests a4 ###
###############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- a4_fun(data_ud_wms$a4[i], data_ud_wms$edad[i])
  print(i)
}


#################
### Tests a5 ###
###############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- a5_fun(data_ud_wms$a5[i], data_ud_wms$edad[i])
  print(i)
}

plot(data_ud_wms$a5t, correction)


################
### Tests b ###
##############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- b_fun(data_ud_wms$b[i], data_ud_wms$edad[i])
  print(i)
}

plot(data_ud_wms$b, correction)

#################
### Tests a6 ###
###############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- a6_fun(data_ud_wms$a66[i], data_ud_wms$edad[i])
  print(i)
}

plot(data_ud_wms$a6t, correction)

#################
### Tests a7 ###
###############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- a7_fun(data_ud_wms$a7[i], data_ud_wms$edad[i])
  print(i)
}

plot(data_ud_wms$a7t, correction)

################
### Tests r ###
##############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- r_fun(data_ud_wms$r[i], data_ud_wms$edad[i])
  print(i)
}

plot(data_ud_wms$rt, correction)
     
#####################
### RECUPERACION ###
###################

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- recuper_fun(data_ud_wms$a7[i], data_ud_wms$r[i], data_ud_wms$fpr[i], data_ud_wms$edad[i], data_ud_wms$sexo[i])
  print(i)
}

plot(data_ud_wms$recupert, correction)



##############
### APREN ###
############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- apren_fun(data_ud_wms$a5[i], data_ud_wms$a1[i], data_ud_wms$edad[i])
  print(i)
}

plot(data_ud_wms$aprent, correction)



##############
### RECON ###
############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- recon_fun(data_ud_wms$r[i], data_ud_wms$fpr[i], data_ud_wms$edad[i], data_ud_wms$sexo[i])
  print(i)
}

plot(data_ud_wms$reconcot, correction)


##############
### rfprT ###
############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- fpr_fun(data_ud_wms$fpr[i], data_ud_wms$edad[i], data_ud_wms$sexo[i])
  print(i)
}

plot(data_ud_wms$rfpt, correction)

################
### infover ###
##############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- infovert_fun(data_ud_wms$infover[i], data_ud_wms$edad[i], data_ud_wms$sexo[i])
  print(i)
}

plot(data_ud_wms$infovert, correction)

#################
### retriefi ###
###############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- retriefit_fun(data_ud_wms$retriefi[i], data_ud_wms$edad[i], data_ud_wms$sexo[i])
  print(i)
}

plot(data_ud_wms$retrieft, correction)

#################
#### aptot #####
###############

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- aptot_fun(data_ud_wms$aptot[i], data_ud_wms$edad[i])
  print(i)
}

plot(data_ud_wms$aptott, correction)

##############
## ltprrat ##
############
ltprrat <- c()
for(i in 1:nrow(data_ud_wms)){
  ltprrat[i] <- stpret_fun(data_ud_wms$a5[i], data_ud_wms$a7[i])
}

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- avl_pr_fun(ltprrat[i], data_ud_wms$edad[i], 'ltperrat')
  print(i)
}

plot(data_ud_wms$ltprrat, correction)
View(cbind(data_ud_wms$ltprrat, correction, data_ud_wms$edad, correction - data_ud_wms$ltprrat, ltprrat, data_ud_wms$ltperr))


##############
## strprat##
############
stpret <- c()
for(i in 1:nrow(data_ud_wms)){
  stpret[i] <- stpret_fun(data_ud_wms$a5[i], data_ud_wms$a6[i])
}

correction <- c()

for(i in 1:nrow(data_ud_wms)){
  correction[i] <- avl_pr_fun(stpret[i], data_ud_wms$edad[i], 'stpret')
  print(i)
}

plot(data_ud_wms$stprett, correction)
View(cbind(data_ud_wms$stprett, correction, data_ud_wms$edad, correction - data_ud_wms$stprett, stpret, data_ud_wms$stprett))
