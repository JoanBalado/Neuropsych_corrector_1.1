require(shiny)
require(ggplot2)
require(reshape2)
require(dplyr)
require(DT)
library(reactlog)
library(stringr)
options(shiny.reactlog = TRUE)


########################################################
################ Directorio de trabajo ################
######################################################

directory <- c('data/')

####################################################
############### Vectores de nombres ###############
##################################################

avl_names1 <- c('A-1', 'A-2', 'A-3', 'A-4', 'A-5', 'B-list', 'A-6', 'A-7', 'A-Reconocimiento')
avl_names2 <- c('Aprendizaje Total','Indice de Aprendizaje', '% Recuperacion Informacion-Alm.', '% Recuperacion Corto-Plazo', '% Recuperacion Largo-Plazo',  '% Eficiencia Recuperacion', '% Reconocimiento', 'Num. Falsos Positivos', '% Sobrecarga Informacion')
digprx_names <- c('Digitos Directos', 'Digitos Inversos', 'Control mental WMS-III', 'Imitacion derecha', 'Imitacion izquierda', 'Imitacion bilateral', 'Coordinacion reciproca', 'Secuencias posturas')
visoconst_names <- c('Reloj', 'Cubos WAIS', 'FCR-Copia', 'FCR-Tiempo', 'FCR-RI', 'FCR-RD', 'FCR-Reconocimiento', 'FCR-VP', 'FCR-FP', 'FCR-VN', 'FCR-FN')
lenguaje_names <- c('BNT-30', 'Denominacion BCN', 'Boston-Ordenes', 'Repeticion palabras', 'Repeticion logotomas', 'Repeticion frases', 'Fluencia verbal-Letra P', 'P-Repeticiones', 'P-intrusiones', 'Fluencia verbal-Animales', 'Animales-Repeticiones','Animales-Intrusiones', 'FAS')
visospatial_names <- c('Jaeger', '15-Objetos', 'FSL', 'Hooper', 'BNT-errores VE', 'JLO', 'Lectura Relojes', 'VOSP-Deci.Objetos', 'VOSP-Siluetas.Prgr.', 'VOSP-Disc.Posiciones', 'VOSP-Loc.Numeros') 
ffee1_names <- c('Stroop-P', 'Stroop-P err.', 'Stroop-C', 'Stroop-C err.', 'Stroop-PC', 'Stroop-PC err.', 'TMT-A', 'TMT-A err.', 'TMT-B', 'TMT-B err.')
ffee2_names <- c('Prueba A', 'SDMTo-Correctos', 'SDMTo-err.', 'Calculo Mental', 'Semejanzas', 'TOL-Correctos', 'TOL-Movimientos', 'TOL-Tiempo Latencia', 'TOL-Tiempo Ejecucion', 'TOL-Tiempo Resolucion')

#################################################
############### FUNCIONES AVL ##################
###############################################

a1_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 6.6)/1.7
  else if (edad <= 49) output = (score - 6.6)/1.7
  else if (edad <= 59) output = (score - 6.2)/1.6
  else if (edad <= 69) output = (score - 5.9)/1.6
  else if (edad <= 84) output = (score - 5.5)/1.6 
  else output = (score - 4.0)/1.5
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}
a2_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 9.6)/2.1
  else if (edad <= 49) output = (score - 9.3)/1.9
  else if (edad <= 59) output = (score - 9.0)/1.9
  else if (edad <= 69) output = (score - 8.4)/2.0
  else if (edad <= 84) output = (score - 7.7)/2.1
  else output = (score - 6.0)/1.8
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}
a3_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 11.1)/2.1
  else if (edad <= 49) output = (score - 10.8)/2.1
  else if (edad <= 59) output = (score - 10.5)/1.9
  else if (edad <= 69) output = (score - 9.8)/2.3
  else if (edad <= 84) output = (score - 8.8)/2.1
  else output = (score - 7.4)/2.2 
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}
a4_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 12.0)/1.9
  else if (edad <= 49) output = (score - 11.7)/2.1 
  else if (edad <= 59) output = (score - 11.4)/1.9 
  else if (edad <= 69) output = (score - 10.9)/2.3
  else if (edad <= 84) output = (score - 9.8)/2.4 
  else output = (score - 7.9)/2.4
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}
a5_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 12.6)/1.9
  else if (edad <= 49) output = (score - 12.3)/1.9
  else if (edad <= 59) output = (score - 12.1)/2.1
  else if (edad <= 69) output = (score - 11.3)/2.3
  else if (edad <= 84) output = (score - 10.3)/2.4
  else output = (score - 9.1)/2.3
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}
b_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 6.6)/2.1
  else if (edad <= 49) output = (score - 6.1)/1.9
  else if (edad <= 59) output = (score - 5.7)/2.2
  else if (edad <= 69) output = (score - 5.1)/1.3
  else if (edad <= 84) output = (score - 3.9)/1.6 
  else output = (score - 3.1)/1.4 
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}
a6_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 11.2)/2.6
  else if (edad <= 49) output = (score - 10.4)/2.6
  else if (edad <= 59) output = (score - 9.9)/2.8
  else if (edad <= 69) output = (score - 9.3)/2.9
  else if (edad <= 84) output = (score - 8.1)/3.0
  else output = (score - 6.2)/2.6
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}
a7_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 11.0)/2.8
  else if (edad <= 49) output = (score - 10.2)/2.8
  else if (edad <= 59) output = (score - 9.9)/3.2
  else if (edad <= 69) output = (score - 8.8)/3.0
  else if (edad <= 84) output = (score - 7.0)/2.4
  else output = (score - 5.4)/2.7
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}
r_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 14.2)/1.2 
  else if (edad <= 49) output = (score - 14.0)/1.4 
  else if (edad <= 59) output = (score - 13.9)/1.4
  else if (edad <= 69) output = (score - 13.5)/1.3
  else if (edad <= 84) output = (score - 13.3)/1.5
  else output = (score - 12.3)/2.3
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}

recuper_fun <- function(a7, r, fpr){
  if(is.na(a7) | a7 > 15 | a7 < 0 | 
     is.na(r) | r > 15 | r < 0 |
     is.na(fpr) | fpr > 30 | fpr < 0){
    return(NA_real_)
  }
  
  fpr <- ifelse(a7 == 0 & r == 0 & fpr == 0, 0.1, fpr)
  r <- ifelse(a7 == 0 & r == 0, 0.1, r)
  a7 <- a7/15
  
  return(round_fun(a7/(0.033333*r - 0.025*fpr + 0.5)))
}
apren_fun <- function(a5, a1){
  if(is.na(a5) | a5 > 15 | a5 < 0 | 
     is.na(a1) | a1 > 15 | a1 < 0 ){
    return(NA_real_)
  }
  return(a5 - a1)
}
infover_fun <- function(digits, a1){
  if(is.na(digits) | digits > 9 | digits == 0 |
     is.na(a1) | a1 > 15 | a1 < 0){
    return(NA_real_)
  }
  return(round_fun(a1/digits))
}
recon_fun <- function(r, fpr){
  if(is.na(r) | r > 15 | r < 0 |
     is.na(fpr) | fpr > 20 | fpr < 0){
    return(NA_real_)
  }
  
  fpr <- ifelse(r == 0 & fpr == 0, 0.1, fpr)
  r <- ifelse(r == 0, 0.1, r)
  
  return(round_fun(0.5*(1+(r/15)-(fpr/20))))
}
retriefi_fun <- function(a7, r){
  if(is.na(a7) | a7 > 15 | a7 < 0 | 
     is.na(r) | r > 15 | r < 0 | r == 0){
    return(NA_real_)
  }
  return(round_fun(a7/r))
}
aptot_fun <- function(a1, a2, a3, a4, a5){
  if(is.na(a1) | is.na(a2) | is.na(a3) | is.na(a4) |is.na(a5) |
     a1 < 0 | a2 < 0 | a3 < 0 | a4 < 0 | a5 < 0 |
     a1 > 15 | a2 > 15 | a3 > 15 | a4 > 15 | a5 > 15){
    return(NA_real_)
  }
  return(a1+a2+a3+a4+a5)
}

recupert_fun <- function(recuper, edad, sexo){
  if(is.na(recuper) | 
     is.na(edad) | is.na(sexo)){
    return(NA_real_)
  }
  
  output <- c()
  
  if(sexo == 'Masculino'){
    if (edad < 20) output = (recuper - 0.8)/0.1
    else if (edad <= 29) output = (recuper - 0.8)/0.2
    else if (edad <= 39) output = (recuper - 0.8)/0.2
    else if (edad <= 49) output = (recuper - 0.8)/0.2
    else if (edad <= 59) output = (recuper - 0.7)/0.2
    else if (edad <= 69) output = (recuper - 0.6)/0.3
    else output = (recuper - 0.5)/0.2
  }
  
  if(sexo == 'Femenino'){
    if (edad < 20) output = (recuper - 0.8)/0.2
    else if(edad <= 29) output = (recuper - 0.8)/0.2
    else if(edad <= 39) output = (recuper - 0.9)/0.2
    else if(edad <= 49) output = (recuper - 0.8)/0.2
    else if(edad <= 59) output = (recuper - 0.8)/0.2
    else if(edad <= 69) output = (recuper - 0.8)/0.2
    else output = (recuper - 0.7)/0.1
  }
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}
aprent_fun <- function(apren, edad){
  if(is.na(apren) |
     is.na(edad)){
    return(NA_real_)
  }
  
  output <- c()
  
  if (edad <= 29) output = (apren - 6.0)/2.0
  else if (edad <= 49) output = (apren - 5.3)/1.9
  else if (edad <= 65) output = (apren - 5.7)/2.0
  else if (edad <= 70) output = (apren - 5.6)/2.9
  else if (edad <= 75) output = (apren - 5.2)/2.5
  else output = (apren - 4.7)/2.7
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}
recont_fun <- function(recon, edad, sexo){
  if(is.na(recon) |
     is.na(edad) | is.na(sexo)){
    return(NA_real_)
  }
  
  output <- c()
  
  if(sexo == 'Masculino'){
    if( edad <= 29) output = (recon - 0.90)/0.05
    else if(edad <= 39) output = (recon - 0.92)/0.04
    else if(edad <= 49) output = (recon - 0.92)/0.06
    else if(edad <= 59) output = (recon - 0.90)/0.06
    else if(edad <= 69) output = (recon - 0.82)/0.13
    else output = (recon - 0.81)/0.10
  }
  
  if(sexo == 'Femenino'){
    if( edad <= 29) output = (recon - 0.91)/0.09
    else if(edad <= 39) output = (recon - 0.89)/0.08
    else if(edad <= 49) output = (recon - 0.88)/0.07
    else if(edad <= 59) output = (recon - 0.88)/0.08
    else if(edad <= 69) output = (recon - 0.90)/0.06
    else output = (recon - 0.84)/0.11
  }
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}
fpr_fun <- function(fpr, edad, sexo){
  if(is.na(fpr) | fpr > 30 | fpr < 0 |
     is.na(edad) | is.na(sexo)){
    return(NA_real_)
  }
  
  output <- c()
  
  if(sexo == 'Masculino'){
    if( edad < 20) output = (fpr - 1.0)/0.1
    else if(edad <= 29) output = (fpr - 3.2)/2.0
    else if(edad <= 39) output = (fpr - 1.5)/1.4
    else if(edad <= 49) output = (fpr - 2.6)/2.7
    else if(edad <= 59) output = (fpr - 2.9)/2.6
    else if(edad <= 69) output = (fpr - 4.3)/4.0
    else output = (fpr - 3.1)/3.0
  }
  if(sexo == 'Femenino'){
    if( edad < 20) output = (fpr - 0.9)/0.1
    else if(edad <= 29) output = (fpr - 3.6)/3.9
    else if(edad <= 39) output = (fpr - 4.2)/3.4
    else if(edad <= 49) output = (fpr - 4.6)/2.7
    else if(edad <= 59) output = (fpr - 3.7)/3.7
    else if(edad <= 69) output = (fpr - 2.9)/3.1
    else output = (fpr - 5.6)/5.7
  }
  
  return(ifelse((output*-10+50) < 0, 0, round_fun(output*-10+50)))
}
infovert_fun <- function(infover, edad, sexo){
  if(is.na(infover) | 
     is.na(edad) | is.na(sexo)){
    return(NA_real_)
  }
  
  output <- c()
  
  if(sexo == 'Masculino'){
    if( edad < 20) output = (infover - 1.1)/0.3
    else if(edad <= 29) output = (infover - 1.2)/0.2
    else if(edad <= 39) output = (infover - 0.9)/0.2
    else if(edad <= 49) output = (infover - 1.0)/0.4
    else if(edad <= 59) output = (infover - 1.0)/0.3
    else if(edad <= 69) output = (infover - 0.8)/0.2
    else output = (infover - 0.6)/0.2
  }
  if(sexo == 'Femenino'){
    if( edad < 20) output = (infover - 1.2)/0.4
    else if(edad <= 29) output = (infover - 1.0)/0.2
    else if(edad <= 39) output = (infover - 1.2)/0.5
    else if(edad <= 49) output = (infover - 1.0)/0.4
    else if(edad <= 59) output = (infover - 1.0)/0.3
    else if(edad <= 69) output = (infover - 0.9)/0.3
    else output = (infover - 1.0)/0.3
  }
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}
retriefit_fun <- function(retrief, edad, sexo){
  if(is.na(retrief) | 
     is.na(edad) | is.na(sexo)){
    return(NA_real_)
  }
  
  output <- c()
  
  if(sexo == 'Masculino'){
    if( edad < 20) output = (retrief - 0.8)/0.1
    else if(edad <= 29) output = (retrief - 0.8)/0.2
    else if(edad <= 39) output = (retrief - 0.8)/0.2
    else if(edad <= 49) output = (retrief - 0.8)/0.2
    else if(edad <= 59) output = (retrief - 0.7)/0.2
    else if(edad <= 69) output = (retrief - 0.6)/0.3
    else output = (retrief - 0.5)/0.2
  }
  if(sexo == 'Femenino'){
    if( edad < 20) output = (retrief - 0.8)/0.2
    else if(edad <= 29) output = (retrief - 0.8)/0.2
    else if(edad <= 39) output = (retrief - 0.9)/0.2
    else if(edad <= 49) output = (retrief - 0.8)/0.2
    else if(edad <= 59) output = (retrief - 0.8)/0.2
    else if(edad <= 69) output = (retrief - 0.8)/0.2
    else output = (retrief - 0.7)/0.1
  }
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}  
aptott_fun <- function(score, edad){
  if(is.na(score) | score > 75 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 53.0)/7.8
  else if (edad <= 49) output = (score - 51.1)/8.6
  else if (edad <= 59) output = (score - 47.6)/8.1 
  else if (edad <= 69) output = (score - 43.4)/7.7
  else output = (score - 37.1)/7.5
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}

avl_calc_fun <- function(a5, a67){
  if(is.na(a5) | is.na(a67) |
     a5 > 15 | a5 < 0 | 
     a67 > 15 | a67 < 0){
    return(NA_real_)
  }
  if(a5 == 0) a5 = 0.1
  
  return(round((a67/a5)*100, digits = 2))
}

avlt_calct_fun <- function(score, edad, test){
  if(is.na(edad) | is.na(score) |
     edad > 100 | edad < 18){
    return(NA_real_)
  }
  output <- c()
  if(edad < 50 & test == 'stpret') return(ifelse((((score-91.83)/13.01)*10+50)<0, 0, round_fun(((score-91.83)/13.01)*10+50)))
  else if(edad < 50 & test == 'ltperrat') return(ifelse((((score-90.51)/15.47)*10+50)<0, 0, round_fun(((score-90.51)/15.47)*10+50)))
  else{
    if(score > 100) score = 100
    score = round(score)
    grupos <- read.delim(paste0(directory, 'grupo_edad_stpret_ltprrat_avl.txt'))
    grupo <- grupos[grupos$edad==edad, 'grup']
    grid <- read.delim(paste0(directory, test, '.txt'))
    for(i in 1:nrow(grid)){
      if(score <= grid[i,grupo]){
        output = grid[i,grupo+1]
        break
      }
    }
    return(output)
  }
}


##############################################
############ FUNCIONES NEURONORMA ###########
############################################

T_fun <- function(x){
  if(is.na(x)) return(NA_real_)
  round((10/3)*x+(50/3), 2)
}

neuronorma_fun <- function(score, edad, educacion, test){
  if(is.na(score) | is.na(edad) | is.na(educacion) | is.na(test) | edad < 18 | educacion < 0){
    return(NA_real_)
  }
  
  edad <- floor(edad)
  educacion <- floor(educacion)
  
  if(educacion > 20) educacion = 20
  if(edad > 100) edad = 100
  
  output <- c()
  
  grupos_edad <- read.delim(paste0(directory, 'grupo_edad_neuronorma.txt'))
  grupo <- grupos_edad[grupos_edad$edad == edad, 'grupo']
  grid <- read.delim(paste0(directory, test, '.txt'))
  
  for(i in 1:nrow(grid)){
    if(score <= grid[i,grupo]){
      output = grid[i,grupo+1]
      break
    }
  }
  
  if(grupo == 1){
    if(test %in% c('digitsf', 'digitsb', 'fcrc', 'ecat', 'p', 'tmtb', 'stroopp', 'strooppc')){
      edu <- read.delim(paste0(directory, test, '_edu_young.txt'))
      edu_adj = edu[edu$Anys == educacion, 'Corrc']
      output = output + edu_adj
    } else if(test %in% c('fcrir', 'fcrdr', 'tmta')){
      edu <- read.delim(paste0(directory, test, '_edu_young.txt'))
      educacion <-ifelse(educacion < 8, 9, educacion)
      edurow = educacion - 7
      agecol = edad - 17
      edu_adj = edu[edurow,agecol]
      output = output + edu_adj
    } else output = output 
  } 
  
  if(grupo >= 2) {
    edu <- read.delim(paste0(directory, test, '_edu.txt'))
    edu_adj <- edu[edu$Anys == educacion, 'Corrc']
    output = output + edu_adj
  }
  
  return(T_fun(output))
}

neuronorma_tol_fun <- function(score, edad, educacion, test){
  if(is.na(score) | is.na(edad) | is.na(educacion) | is.na(test) | edad < 18 | educacion < 0){
    return(NA_real_)
  }
  edad <- floor(edad)
  educacion <- floor(educacion)
  educacion <- ifelse(educacion > 20, 20, educacion)
  edad <- ifelse(edad > 100, 100, edad)
  output <- c()
  
  
  tests <- c('tolcor', 'tolmov', 'tollat', 'toleje', 'tolres')
  hash <- match(test, tests)-1
  
  grupos_edad <- read.delim(paste0(directory, 'grupo_edad_tol.txt'))
  grupo <- grupos_edad[grupos_edad$edad == edad, 'grupo']
  grid <- read.delim(paste0(directory, 'tol.txt'))
  
  if(test == 'tolcor'){
    for(i in 1:nrow(grid)){
      if(is.na(grid[i,grupo+hash]) == F){
        if(score <= grid[i,grupo+hash]){
          output = grid[i,1]
          break
        }  
      }
    }
  } else {
    for(i in nrow(grid):1){
      if(is.na(grid[i,grupo+hash]) == F){
        if(score == grid[i,grupo+hash]){
          output = grid[i,1]
          break
        } else if(score <= grid[i,grupo+hash]){
          output = grid[i+1,1]
          break
        }
      }
      if(i == nrow(grid)) output = 2
    }
  }
  
  if(grupo == 2){
    edu <- read.delim(paste0(directory, 'tol_edu_young.txt'))
    if(test == 'tolmov') output = output + edu[edu$Anys == educacion, 'movtot']
    else if (test == 'tollat') output = output + edu[edu$Anys == educacion, 'lat']
    else output = output
  }
  
  if(grupo > 2){
    edu <- read.delim(paste0(directory, 'tol_edu.txt'))
    edu_adj <- edu[edu$Anys == educacion, hash+2]
    output = output + edu_adj
  }
  
  return(T_fun(output))
}

###########################################
############## RESTO FUNCIONES############
#########################################

round_fun <- function(x){
  round(x, 2)
}

################
### Praxias ###
##############
secpos_fun <- function(secmot, secgraf){
  if(is.na(secmot) | is.na(secgraf) |
     secmot > 4 | secgraf > 4){
    return(NA_real_)
  }
  
  return(secmot + secgraf)
  
}

imitd_fun <- function(score, edad, educacion){
  if(is.na(score) | score < 0 | score > 10 |
     is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  
  if(edad < 50) output = (score - 10)/0.5
  else if(edad <= 70 & educacion <= 5) output = (score - 10)/0.5
  else if(edad <= 70 & educacion <= 12) output = (score - 10)/0.5
  else if(edad <= 70 & educacion > 12) output = (score - 10)/0.5
  else output = (score - 9.948)/0.394
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}
imiti_fun <- function(score, edad, educacion){
  if(is.na(score) | score < 0 | score > 10 |
     is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  
  if(edad < 50) output = (score - 9.991)/0.095
  else if(edad <= 70 & educacion <= 5) output = (score - 9.766)/0.666
  else if(edad <= 70 & educacion <= 12) output = (score - 9.978)/0.207
  else if(edad <= 70 & educacion > 12) output = (score - 10)/0.5
  else output = (score - 9.914)/0.657
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}
imitb_fun <- function(score, edad, educacion){
  if(is.na(score) | score < 0 | score > 8 |
     is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  
  if(edad < 50) output = (score - 7.836)/0.447
  else if(edad <= 70 & educacion <= 5) output = (score - 7.592)/0.864
  else if(edad <= 70 & educacion <= 12) output = (score - 7.935)/0.288
  else if(edad <= 70 & educacion > 12) output = (score -  8)/0.5
  else output = (score - 7.085)/1.430
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}
secpost_fun <- function(secpos, edad, educacion){
  if(is.na(secpos) | secpos < 0 | secpos > 8 |
     is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  
  if(edad < 50) output = (secpos - 7.855)/0.446
  else if(edad <= 70 & educacion <= 5) output = (secpos - 7.279)/1.533
  else if(edad <= 70 & educacion <= 12) output = (secpos - 7.617)/0.929
  else if(edad <= 70 & educacion > 12) output = (secpos -  7.852)/0.602
  else output = (secpos - 6.649)/1.727
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}
coord_fun <- function(score, edad, educacion){
  if(is.na(score) | score < 0 | score > 4 |
     is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  
  if(edad < 50) output = (score - 4.00)/0.5
  else if(edad <= 70 & educacion <= 5) output = (score - 3.88)/0.33
  else if(edad <= 70 & educacion <= 12) output = (score - 3.89)/0.43
  else if(edad <= 70 & educacion > 12) output = (score -  4.00)/0.5
  else output = (score - 3.95)/0.22
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}

###################
### CM-WMS III ###
#################
cm_fun <- function(score, edad){
  if(is.na(score) | score < 0 | score > 40 | 
     is.na(edad) | edad < 18 | edad > 100){
    return(NA_real_)
  }
  
  output <- c()
  score <- as.integer(score)
  
  grupos_edad <- read.delim(paste0(directory, 'grupo_edad_mcwmsiii.txt'))
  grupo <- grupos_edad[grupos_edad$edad == edad, 'grupo']
  grid <- read.delim(paste0(directory, 'mcwmsiii.txt'))
  
  for(i in 1:nrow(grid)){
    if(score <= grid[i,grupo]){
      output = grid[i,grupo+1]
      break
    }
  }
  return(ifelse(T_fun(output)>=0, T_fun(output), 0))
}

###################
### Cubos-WAIS ###
#################
cubos_fun <- function(score, edad){
  if(is.na(score) | is.na(edad) |
     score > 68 | score < 0){
    return(NA_real_)
  }
  
  group <- c()
  output <- c()
  
  if(edad <= 19) group = 1
  else if(edad <= 24) group = 3
  else if(edad <= 34) group = 5
  else if(edad <= 54) group = 7
  else if(edad <= 69) group = 9
  else group = 11
  
  grid <- read.delim(paste0(directory, 'cubos.txt'))
  
  for(i in 1:nrow(grid)){
    if(score <= grid[i,group]){
      output = grid[i,group+1]
      break
    }
  }
  return(T_fun(output))
}

##################
### FCR-Recon ###
################
fcrtn_fun <- function(fcrfp){
  if(is.na(fcrfp) | 
     fcrfp < 0 | fcrfp > 12){
    return(NA_real_)
  }
  return(12 - fcrfp)
}
fcrfn_fun <- function(fcrtp){
  if(is.na(fcrtp) | 
     fcrtp < 0 | fcrtp > 12){
    return(NA_real_)
  }
  return(12 - fcrtp)
}
fcr_recon_fun <- function(fcrtp, fcrtn){
  if(is.na(fcrtp) | is.na(fcrtn) |
     fcrtp < 0 | fcrtn < 0 | fcrtp > 12 | fcrtn > 12){
    return(NA_real_)
  }
  return(fcrtp + fcrtn)
}

fcr_recont_fun <- function(score, edad, type){
  if(is.na(edad) | is.na(score) | is.na(type)){
    return(NA_real_)
  }
  
  output <- c()
  
  if(type == 'fcr_recon'){
    if(edad <= 19) output = (score - 21.74) / 1.48
    else if(edad <=24) output = (score - 21.30) / 1.35
    else if(edad <=29) output = (score - 21.53) / 1.65
    else if(edad <=34) output = (score - 21.58) / 1.27
    else if(edad <=39) output = (score - 21.44) / 1.32
    else if(edad <=44) output = (score - 20.10) / 1.76
    else if(edad <=49) output = (score - 20.86) / 2.03
    else if(edad <=54) output = (score - 20.47) / 1.69
    else if(edad <=64) output = (score - 20.23) / 1.54
    else if(edad <=69) output = (score - 19.42) / 1.66
    else if(edad <=74) output = (score - 20.14) / 1.49
    else output = (score - 19.33) / 1.45
  }
  
  if(type == 'fcrtp'){
    if(edad <= 19) output = (score - 10.13) / 1.45
    else if(edad <=24) output = (score -  9.82) / 1.28
    else if(edad <=29) output = (score - 9.91) / 1.57
    else if(edad <=34) output = (score - 10.05) / 1.54
    else if(edad <=39) output = (score - 9.84) / 1.43
    else if(edad <=44) output = (score - 8.86) / 1.79
    else if(edad <=49) output = (score - 9.49) / 2.13
    else if(edad <=54) output = (score - 9.59) / 1.54
    else if(edad <=59) output = (score - 8.67) / 1.52
    else if(edad <=64) output = (score - 8.87) / 1.69
    else if(edad <=69) output = (score - 8.73) / 1.88
    else if(edad <=74) output = (score - 8.76) / 1.65
    else if(edad <=79) output = (score - 9.35) / 1.19
    else output = (score - 8.93) / 1.28
  }
  
  if(type == 'fcrfp'){
    if(edad <= 19) output = (score - 0.39) / 0.65
    else if(edad <=24) output = (score -  0.52) / 0.85
    else if(edad <=29) output = (score - 0.38) / 0.66
    else if(edad <=34) output = (score - 0.47) / 0.89
    else if(edad <=39) output = (score - 0.40) / 0.58
    else if(edad <=44) output = (score - 0.76) / 0.93
    else if(edad <=49) output = (score - 0.63) / 0.84
    else if(edad <=54) output = (score - 1.12) / 1.39
    else if(edad <=59) output = (score - 0.63) / 0.96
    else if(edad <=64) output = (score - 0.65) / 0.95
    else if(edad <=69) output = (score - 1.30) / 1.31
    else if(edad <=74) output = (score - 0.61) / 0.91
    else if(edad <=79) output = (score - 1.30) / 1.19
    else output = (score - 1.60) / 1.06
  }
  
  if(type == 'fcrtn'){
    if(edad <= 19) output = (score - 11.61) / 0.65
    else if(edad <=24) output = (score -  11.48) / 0.85
    else if(edad <=29) output = (score - 11.62) / 0.66
    else if(edad <=34) output = (score - 11.53) / 0.89
    else if(edad <=39) output = (score - 11.61) / 0.58
    else if(edad <=44) output = (score - 11.24) / 0.93
    else if(edad <=49) output = (score - 11.37) / 0.84
    else if(edad <=54) output = (score - 10.88) / 1.39
    else if(edad <=59) output = (score - 11.37) / 0.96
    else if(edad <=64) output = (score - 11.36) / 0.95
    else if(edad <=69) output = (score - 10.70) / 1.31
    else if(edad <=74) output = (score - 11.39) / 0.91
    else if(edad <=79) output = (score - 10.70) / 1.19
    else output = (score - 10.40) / 1.06
  }
  if(type == 'fcrfn'){
    if(edad <= 19) output = (score - 1.88) / 1.45
    else if(edad <=24) output = (score -  2.18) / 1.28
    else if(edad <=29) output = (score - 2.09) / 1.57
    else if(edad <=34) output = (score - 1.95) / 1.54
    else if(edad <=39) output = (score - 2.16) / 1.43
    else if(edad <=44) output = (score - 3.14) / 1.79
    else if(edad <=49) output = (score - 2.51) / 2.13
    else if(edad <=54) output = (score - 2.41) / 1.54
    else if(edad <=59) output = (score - 3.33) / 1.52
    else if(edad <=64) output = (score - 3.13) / 1.69
    else if(edad <=69) output = (score - 3.27) / 1.88
    else if(edad <=74) output = (score - 3.25) / 1.65
    else if(edad <=79) output = (score - 2.65) / 1.19
    else output = (score - 3.07) / 1.28
  }
  
  if(type %in% c('fcrfp', 'fcrfn')){
    return(ifelse(50 - output*10 < 0, 0, round_fun(50 - output*10)))
  } else return(ifelse(output*10+50 < 0, 0, round_fun(output*10+50)))
}
###################
#### Lenguaje ####
#################
# BNT
bnt_fun <- function(score, edad, sexo, escolaridad){
  if(is.na(score) | is.na(edad) | is.na(sexo) | is.na(escolaridad) |
     score > 30 | score < 0){
    return(NA_real_)
  }
  
  output <- c()
  
  if(sexo == 'Masculino'){
    if(escolaridad <= 10){
      if(edad <= 29) output = (score-22.09)/3.42
      else if(edad <= 39) output = (score-22.39)/2.29
      else if(edad <= 49) output = (score-22.92)/3.68
      else output = (score-24.11)/2.14
    }
    else{
      if(edad <= 29) output = (score-22.52)/3.28
      else if(edad <= 39) output = (score-26.56)/2.68
      else if(edad <= 49) output = (score- 27.82)/1.94
      else output = (score-27.17)/2.56
    }
  }
  
  if(sexo == 'Femenino'){
    if(escolaridad <= 10){
      if(edad <= 29) output = (score-20.17)/2.29
      else if(edad <= 39) output = (score-21.59)/2.96
      else if(edad <= 49) output = (score-21.81)/2.29
      else output = (score-20.52)/4.12
    }
    else{
      if(edad <= 29) output = (score-21.77)/3.95
      else if(edad <= 39) output = (score-24.57)/2.82
      else if(edad <= 49) output = (score-25.64)/1.57
      else output = (score-24.15)/2.85
    }
  }
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}
# DIMAG
dimag_fun <- function(score, edad, escolaridad){
  if(is.na(score) | is.na(edad) | is.na(escolaridad) |
     score > 14 | score < 0){
    return(NA_real_)
  }
  
  output <- c()
  
  if(edad < 50) output = (score-13.964)/0.187
  else if(edad <= 70 & escolaridad <= 5) output = (score-13.776)/0.798
  else if(edad <= 70 & escolaridad <= 12) output = (score-13.975)/0.145
  else if(edad <= 70 & escolaridad > 12) output = (score-14)/0.5
  else output = (score-13.729)/0.639
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}

#FAS
fas_fun <- function(f, a, s){
  if(is.na(f) | is.na(a) | is.na(s) |
     f > 50 | f < 0 | 
     a > 50 | a < 0 | 
     s > 50 | s < 0){
    return(NA_real_)
  }
  
  return(f+a+s)
}

fast_fun <- function(score, escolaridad){
  if(is.na(score) | is.na(escolaridad) |
     score > 300 | score < 0){
    return(NA_real_)
  }
  
  output <- c()
  
  if(escolaridad < 5) score = score + 12
  else if(escolaridad <= 7) score = score + 9
  else if(escolaridad <= 11) score = score + 8
  else if(escolaridad <= 15) score = score + 5
  else score = score
  
  grid <- read.delim(paste0(directory, 'fas.txt'))
  
  output = grid[grid$PD == score, 'PT']
  
  return(output)
}

comord_fun <- function(score, edad, escolaridad){
  if(is.na(score) | is.na(edad) | is.na(escolaridad) |
     score > 15 | score < 0 |
     edad > 100 | edad < 0 |
     escolaridad > 20 | escolaridad < 0){
    return(NA_real_)
  }
  
  output <- c()
  
  if(edad <= 35){
    if(escolaridad < 10) output = (score - 14)/1.1
    else if(escolaridad < 16) output = (score - 14.5)/1
    else output = (score - 14.3)/1
  }
  else{
    if(escolaridad < 10) output = (score - 14)/1.4
    else if(escolaridad < 16) output = (score - 14.6)/1
    else output = (score - 14.6)/0.7
  }
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}

#Repe palabras
reppal_fun <- function(score, edad, escolaridad){
  if(is.na(score) | is.na(edad) | is.na(escolaridad) |
     score > 10 | score < 0){
    return(NA_real_)
  }
  
  output <- c()
  
  if(edad < 50) output = (score-9.992)/0.190
  else if(edad <= 70 & escolaridad <= 5) output = (score-9.980)/0.143
  else if(edad <= 70 & escolaridad <= 12) output = (score-10)/0.1
  else if(edad <= 70 & escolaridad > 12) output = (score-10)/0.5
  else output = (score-9.983)/0.129
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}

#Repe logotomos
replog_fun <- function(score, edad, escolaridad){
  if(is.na(score) | is.na(edad) | is.na(escolaridad) |
     score > 8 | score < 0){
    return(NA_real_)
  }
  
  output <- c()
  
  if(edad < 50) output = (score-7.964)/0.187
  else if(edad <= 70 & escolaridad <= 5) output = (score-7.796)/0.645
  else if(edad <= 70 & escolaridad <= 12) output = (score-7.872)/0.626
  else if(edad <= 70 & escolaridad > 12) output = (score-8)/0.5
  else output = (score-7.467)/1.371
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}

#Repe frases
repfra_fun <- function(score){
  if(is.na(score) | 
     score > 60 | score < 0){
    return(NA_real_)
  }
  
  output <- c()
  
  output = (score-59.91)/0.28
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
  
}



############################
#### Fc. Visoespaciales ###
##########################
# t15 objetos
t15obj_fun <- function(score){
  if(is.na(score) | score > 15 | score < 0){
    return(NA_real_)
  }
  output <- c()
  if(score == 12) output = 43.26
  else if(score == 13) output = 50
  else if(score == 14) output = 56.74
  else if(score == 15) output = 62.82
  else output = 37.18
  
  return(output)
}
# fsl
fsl_fun <- function(score){
  if(is.na(score) | score > 14 | score < 0){
    return(NA_real_)
  }
  
  output = (score-12.82)/0.97
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}

# hooper 
hooper_fun <- function(score, edad, escolaridad){
  if(is.na(score) | is.na(edad) | is.na(escolaridad) |
     score > 30 | score < 0){
    return(NA_real_)
  }
  
  output <- c()
  correc <- c()
  
  correc <- 0.092*edad - 0.368*escolaridad
  score = score + correc
  output = (score-25.78)/4.779
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}


# jlo

jlo_fun <- function(score, edad, sexo){
  if(is.na(score) | is.na(edad) | is.na(sexo) |
     score > 30 | score < 0){
    return(NA_real_)
  }
  
  output <- c()
  score = as.integer(score)
  
  if(edad >= 50){
    if(edad <= 64) score = score+1
    else score = score+3
  }
  
  if(sexo == 'Femenino') score = score+2 
  if(score > 30) score = 30
  
  grid <- read.delim(paste0(directory, 'jlo', '.txt'))
  
  output = grid[grid$PD == score, 'PT']
  return(output)
}

# Clockr

clockr_fun <- function(score, edad){
  if(is.na(score) | is.na(edad) | 
     score > 15 | score < 0){
    return(NA_real_)
  } 
  
  output <- c()
  score <- as.integer(score)
  
  grid <- read.delim(paste0(directory, 'clockr', '.txt'))
  grupos <- read.delim(paste0(directory, 'grupo_edad_clockr', '.txt'))
  grupo <- grupos[grupos$edad == edad, 'grupo']
  
  output = grid[grid[,grupo] == score,grupo+1]
  
  return(100-output)
}
##################
### OTROS FEE ###
################
## SDMTO
sdmto_fun <- function(score, edad, educacion){
  if(is.na(score) | score < 0 | score > 110 | 
     is.na(educacion) | educacion > 20 | educacion < 0){
    return(NA_real_)
  }
  
  output <- c()
  
  if(educacion <= 12){
    if(edad <= 24) output = (score - 61.31) / 11.39
    else if(edad <= 34) output = (score - 60.57) /  9.14
    else if(edad <= 44) output = (score - 59.87) / 10.49
    else if(edad <= 54) output = (score - 53.91) / 10.40
    else if(edad <= 64) output = (score - 49.03) /  9.03
    else output = (score - 42.05) / 11.26
  }
  if(educacion > 12){
    if(edad <= 24) output = (score - 69.91) / 12.64
    else if(edad <= 34) output = (score - 65.71) / 11.64
    else if(edad <= 44) output = (score - 60.95) / 11.32
    else if(edad <= 54) output = (score - 58.31) / 8.67
    else if(edad <= 64) output = (score - 54.47) / 8.93
    else output = (score - 52.89) / 13.54
  }
  
  return(ifelse((output*10+50)>=0, round_fun(output*10+50), 0))
}

## Calculo mental
calc_fun <- function(score, edad, educacion){
  if(is.na(score) | score < 0 | score > 10 | 
     is.na(edad) | edad > 100 | edad < 18 |
     is.na(educacion) | educacion > 20 | educacion < 0){
    return(NA_real_)
  }
  
  output <- c()
  
  if(edad < 50) output = (score - 9.17)/1.20
  else if(edad <= 70 & educacion <= 5) output = (score - 5.00)/2.02
  else if(edad <= 70 & educacion <= 12) output = (score - 8.40)/1.48
  else if(edad <= 70 & educacion > 12) output = (score - 9.77)/0.60
  else output = (score - 7.94)/3.10
  
  return(ifelse((output*10+50)>=0, round_fun(output*10+50), 0))
}

## Simil
simil_fun <- function(score, edad, educacion){
  if(is.na(score) | score < 0 | score > 26 | 
     is.na(edad) | edad > 100 | edad < 18 |
     is.na(educacion) | educacion > 20 | educacion < 0){
    return(NA_real_)
  }
  
  grid <- read.delim(paste0(directory, 'simil.txt'))
  output <- grid[grid$PD == score,'PTb']
  
  if(educacion < 6){
    if(edad <= 34) output = output + 6 
    else if(edad <= 39) output = output + 7 
    else if(edad <= 44) output = output + 7
    else if(edad <= 49) output = output + 8 
    else if(edad <= 54) output = output + 8
    else if(edad <= 59) output = output +9 
    else if(edad <= 64) output = output + 10 
    else if(edad <= 69) output = output + 11 
    else if(edad <= 74) output = output + 12 
    else if(edad <= 79) output = output + 12
    else if(edad <= 84) output = output + 13
    else output = output + 14
  }
  
  if(educacion <= 8){
    if(edad <= 34) output = output + 0
    else if(edad <= 39) output = output + 1 
    else if(edad <= 44) output = output + 1
    else if(edad <= 49) output = output + 2 
    else if(edad <= 54) output = output + 2
    else if(edad <= 59) output = output + 3 
    else if(edad <= 64) output = output + 4 
    else if(edad <= 69) output = output + 5 
    else if(edad <= 74) output = output + 6 
    else if(edad <= 79) output = output + 6
    else if(edad <= 84) output = output + 7
    else output = output + 8
  }
  
  if(educacion <= 11){
    if(edad <= 34) output = output - 6
    else if(edad <= 39) output = output - 5 
    else if(edad <= 44) output = output - 5
    else if(edad <= 49) output = output - 4 
    else if(edad <= 54) output = output - 3
    else if(edad <= 59) output = output - 2 
    else if(edad <= 64) output = output - 1 
    else if(edad <= 69) output = output - 1 
    else if(edad <= 74) output = output  
    else if(edad <= 79) output = output + 1
    else if(edad <= 84) output = output + 2
    else output = output + 4
  }
  
  if(educacion == 12){
    if(edad <= 34) output = output - 10
    else if(edad <= 39) output = output - 9 
    else if(edad <= 44) output = output - 9
    else if(edad <= 49) output = output - 8 
    else if(edad <= 54) output = output - 7
    else if(edad <= 59) output = output - 6 
    else if(edad <= 64) output = output - 5 
    else if(edad <= 69) output = output - 5 
    else if(edad <= 74) output = output - 4 
    else if(edad <= 79) output = output - 3
    else if(edad <= 84) output = output - 2
    else output = output - 1
  }
  
  if(educacion <= 15){
    if(edad <= 34) output = output - 15
    else if(edad <= 39) output = output - 14 
    else if(edad <= 44) output = output - 14
    else if(edad <= 49) output = output - 13 
    else if(edad <= 54) output = output - 12
    else if(edad <= 59) output = output - 11 
    else if(edad <= 64) output = output - 11 
    else if(edad <= 69) output = output - 10 
    else if(edad <= 74) output = output - 9 
    else if(edad <= 79) output = output - 8
    else if(edad <= 84) output = output - 7
    else output = output - 6
  }
  
  if(educacion <= 17){
    if(edad <= 34) output = output - 19
    else if(edad <= 39) output = output - 18 
    else if(edad <= 44) output = output - 18
    else if(edad <= 49) output = output - 17 
    else if(edad <= 54) output = output - 16
    else if(edad <= 59) output = output - 15 
    else if(edad <= 64) output = output - 14 
    else if(edad <= 69) output = output - 14 
    else if(edad <= 74) output = output - 13 
    else if(edad <= 79) output = output - 12
    else if(edad <= 84) output = output - 11
    else output = output - 10
  }
  
  if(educacion > 17){
    if(edad <= 34) output = output - 24
    else if(edad <= 39) output = output - 23 
    else if(edad <= 44) output = output - 23
    else if(edad <= 49) output = output - 22 
    else if(edad <= 54) output = output - 22
    else if(edad <= 59) output = output - 21 
    else if(edad <= 64) output = output - 21 
    else if(edad <= 69) output = output - 20 
    else if(edad <= 74) output = output - 19 
    else if(edad <= 79) output = output - 18
    else if(edad <= 84) output = output - 17
    else output = output - 16
  }
  
  return(ifelse(output>=0, round_fun(output), 0))
}

########################################
########### SERVIDOR SHINY ############
######################################

server <- function(input, output, session) {
  
  ########################
  ### Triggers Inputs ###
  ######################
  
  # Digitos y praxias
  observeEvent(input$digitsf, {
    if(is.na(input$digitsf) == F){
      if(input$digitsf > 9 | input$digitsf < 0) {
        updateNumericInput(session, 'digitsf', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 9. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$digitsb, {
    if(is.na(input$digitsb) == F){
      if(input$digitsb > 8 | input$digitsb < 0) {
        updateNumericInput(session, 'digitsb', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 8. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$cm, {
    if(is.na(input$cm) == F){
      if(input$cm < 0 | input$cm > 40) {
        updateNumericInput(session, 'cm', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 40. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$imitd, {
    if(is.na(input$imitd) == F){
      if(input$imitd > 10 | input$imitd < 0) {
        updateNumericInput(session, 'imitd', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 10. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$imiti, {
    if(is.na(input$imiti) == F){
      if(input$imiti > 10 | input$imiti < 0) {
        updateNumericInput(session, 'imiti', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 10. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$imitb, {
    if(is.na(input$imitb) == F){
      if(input$imitb > 8 | input$imitb < 0) {
        updateNumericInput(session, 'imitb', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 8. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$coord, {
    if(is.na(input$coord) == F){
      if(input$coord > 4 | input$coord < 0) {
        updateNumericInput(session, 'coord', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 4. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$secmot, {
    if(is.na(input$secmot) == F){
      if(input$secmot > 4 | input$secmot < 0) {
        updateNumericInput(session, 'secmot', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 4. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$secgraf, {
    if(is.na(input$secgraf) == F){
      if(input$secgraf > 4 | input$secgraf < 0) {
        updateNumericInput(session, 'secgraf', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 4. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  
  # AVL
  observeEvent(input$a1, {
    if(is.na(input$a1) == F){
      if(input$a1 > 15 | input$a1 < 0) {
        updateNumericInput(session, 'a1', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$a2, {
    if(is.na(input$a2) == F){
      if(input$a2 > 15 | input$a2 < 0) {
        updateNumericInput(session, 'a2', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$a3, {
    if(is.na(input$a3) == F){
      if(input$a3 > 15 | input$a3 < 0) {
        updateNumericInput(session, 'a3', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$a4, {
    if(is.na(input$a4) == F){
      if(input$a4 > 15 | input$a4 < 0) {
        updateNumericInput(session, 'a4', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$a5, {
    if(is.na(input$a5) == F){
      if(input$a5 > 15 | input$a5 < 0) {
        updateNumericInput(session, 'a5', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  observeEvent(input$a6, {
    if(is.na(input$a6) == F){
      if(input$a6 > 15 | input$a6 < 0) {
        updateNumericInput(session, 'a6', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$a7, {
    if(is.na(input$a7) == F){
      if(input$a7 > 15 | input$a7 < 0) {
        updateNumericInput(session, 'a7', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  observeEvent(input$b, {
    if(is.na(input$b) == F){
      if(input$b > 15 | input$b < 0) {
        updateNumericInput(session, 'b', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  observeEvent(input$r, {
    if(is.na(input$r) == F){
      if(input$r > 15 | input$r < 0) {
        updateNumericInput(session, 'r', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$ras, {
    if(is.na(input$ras) == F){
      if(input$ras > 10 | input$ras < 0) {
        updateNumericInput(session, 'ras', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 10. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$raf, {
    if(is.na(input$raf) == F){
      if(input$raf > 10 | input$raf < 0) {
        updateNumericInput(session, 'raf', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 10. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$recb, {
    if(is.na(input$recb) == F){
      if(input$recb > 15 | input$recb < 0) {
        updateNumericInput(session, 'recb', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  
  # Visconst
  observeEvent(input$cubos, {
    if(is.na(input$cubos) == F){
      if(input$cubos > 68 | input$cubos < 0) {
        updateNumericInput(session, 'cubos', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 68. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$fcrc, {
    if(is.na(input$fcrc) == F){
      if(input$fcrc > 36 | input$fcrc < 0) {
        updateNumericInput(session, 'fcrc', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 36. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$fcrt, {
    if(is.na(input$fcrt) == F){
      if(input$fcrt > 1000 | input$fcrt < 0) {
        updateNumericInput(session, 'fcrt', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 1000. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$fcrir, {
    if(is.na(input$fcrir) == F){
      if(input$fcrir > 36 | input$fcrir < 0) {
        updateNumericInput(session, 'fcrir', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 36. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$fcrdr, {
    if(is.na(input$fcrdr) == F){
      if(input$fcrdr > 36 | input$fcrdr < 0) {
        updateNumericInput(session, 'fcrdr', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 36. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$fcrtp, {
    if(is.na(input$fcrtp) == F){
      if(input$fcrtp > 12 | input$fcrtp < 0) {
        updateNumericInput(session, 'fcrtp', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 12. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$fcrfp, {
    if(is.na(input$fcrfp) == F){
      if(input$fcrfp > 12 | input$fcrfp < 0) {
        updateNumericInput(session, 'fcrfp', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 12. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  
  #Lenguaje
  observeEvent(input$bnt, {
    if(is.na(input$bnt) == F){
      if(input$bnt > 30 | input$bnt < 0) {
        updateNumericInput(session, 'bnt', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 30. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$bnte, {
    if(is.na(input$bnte) == F){
      if(input$bnte > 30 | input$bnte < 0) {
        updateNumericInput(session, 'bnte', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 30. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$dimag, {
    if(is.na(input$dimag) == F){
      if(input$dimag > 14 | input$dimag < 0) {
        updateNumericInput(session, 'dimag', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 14. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$comord, {
    if(is.na(input$comord) == F){
      if(input$comord > 15 | input$comord < 0) {
        updateNumericInput(session, 'comord', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$ecat, {
    if(is.na(input$ecat) == F){
      if(input$ecat > 50 | input$ecat < 0) {
        updateNumericInput(session, 'ecat', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 50. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$ecatint, {
    if(is.na(input$ecatint) == F){
      if(input$ecatint > 50 | input$ecatint < 0) {
        updateNumericInput(session, 'ecatint', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 50. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$ecatrep, {
    if(is.na(input$ecatrep) == F){
      if(input$ecatrep > 50 | input$ecatrep < 0) {
        updateNumericInput(session, 'ecatrep', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 50. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$p, {
    if(is.na(input$p) == F){
      if(input$p > 50 | input$p < 0) {
        updateNumericInput(session, 'p', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 50. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$pint, {
    if(is.na(input$pint) == F){
      if(input$pint > 50 | input$pint < 0) {
        updateNumericInput(session, 'pint', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 50. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$prep, {
    if(is.na(input$prep) == F){
      if(input$prep > 50 | input$prep < 0) {
        updateNumericInput(session, 'prep', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 50. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  observeEvent(input$f, {
    if(is.na(input$f) == F){
      if(input$f > 50 | input$f < 0) {
        updateNumericInput(session, 'f', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 50. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$a, {
    if(is.na(input$a) == F){
      if(input$a > 50 | input$a < 0) {
        updateNumericInput(session, 'a', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 50. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$s, {
    if(is.na(input$s) == F){
      if(input$s > 50 | input$s < 0) {
        updateNumericInput(session, 's', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 50. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  observeEvent(input$repfra, {
    if(is.na(input$repfra) == F){
      if(input$repfra > 60 | input$repfra < 0) {
        updateNumericInput(session, 'repfra', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 60. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$reppal, {
    if(is.na(input$reppal) == F){
      if(input$reppal > 10 | input$reppal < 0) {
        updateNumericInput(session, 'reppal', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 10. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$replog, {
    if(is.na(input$replog) == F){
      if(input$replog > 8 | input$replog < 0) {
        updateNumericInput(session, 'replog', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 8. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  
  
  # Funciones visoperceptivas
  observeEvent(input$fsl, {
    if(is.na(input$fsl) == F){
      if(input$fsl > 14 | input$fsl < 0) {
        updateNumericInput(session, 'fsl', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 14. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$`15obj`, {
    if(is.na(input$`15obj`) == F){
      if(input$`15obj` > 15 | input$`15obj` < 0) {
        updateNumericInput(session, '15obj', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$hooper, {
    if(is.na(input$hooper) == F){
      if(input$hooper > 30 | input$hooper < 0) {
        updateNumericInput(session, 'hooper', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 30. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$jlo, {
    if(is.na(input$jlo) == F){
      if(input$jlo > 26 | input$jlo < 0) {
        updateNumericInput(session, 'jlo', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 26. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$clockr, {
    if(is.na(input$clockr) == F){
      if(input$clockr > 15 | input$clockr < 0) {
        updateNumericInput(session, 'clockr', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 15. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })

  
  
  # VOSP
  observeEvent(input$deciobj, {
    if(is.na(input$deciobj) == F){
      if(input$deciobj > 20 | input$deciobj < 0) {
        updateNumericInput(session, 'deciobj', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 20. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$silprog, {
    if(is.na(input$silprog) == F){
      if(input$silprog > 20 | input$silprog < 0) {
        updateNumericInput(session, 'silprog', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 20. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$discpos, {
    if(is.na(input$discpos) == F){
      if(input$discpos > 20 | input$discpos < 0) {
        updateNumericInput(session, 'discpos', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 20. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$locnum, {
    if(is.na(input$locnum) == F){
      if(input$locnum > 10 | input$locnum < 0) {
        updateNumericInput(session, 'locnum', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 10. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  
  # Funciones ejecutivas
  observeEvent(input$stroopp, {
    if(is.na(input$stroopp) == F){
      if(input$stroopp > 200 | input$stroopp < 0) {
        updateNumericInput(session, 'stroopp', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 200. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$stroopc, {
    if(is.na(input$stroopc) == F){
      if(input$stroopc > 200 | input$stroopc < 0) {
        updateNumericInput(session, 'stroopc', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 200. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$strooppc, {
    if(is.na(input$strooppc) == F){
      if(input$strooppc > 200 | input$strooppc < 0) {
        updateNumericInput(session, 'strooppc', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 200. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$strooppe, {
    if(is.na(input$strooppe) == F){
      if(input$strooppe > 20 | input$strooppe < 0) {
        updateNumericInput(session, 'strooppe', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 20. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$stroopce, {
    if(is.na(input$stroopce) == F){
      if(input$stroopce > 20 | input$stroopce < 0) {
        updateNumericInput(session, 'stroopce', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 20. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$strooppce, {
    if(is.na(input$strooppce) == F){
      if(input$strooppce > 20 | input$strooppce < 0) {
        updateNumericInput(session, 'strooppce', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 20. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  observeEvent(input$tmta, {
    if(is.na(input$tmta) == F){
      if(input$tmta > 300 | input$tmta < 0) {
        updateNumericInput(session, 'tmta', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 300. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$tmtb, {
    if(is.na(input$tmtb) == F){
      if(input$tmtb > 800 | input$tmtb < 0) {
        updateNumericInput(session, 'tmtb', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 800. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$tmtae, {
    if(is.na(input$tmtae) == F){
      if(input$tmtae > 20 | input$tmtae < 0) {
        updateNumericInput(session, 'tmtae', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 20. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$tmtbe, {
    if(is.na(input$tmtbe) == F){
      if(input$tmtbe > 20 | input$tmtbe < 0) {
        updateNumericInput(session, 'tmtbe', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 20. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  # ToL
  observeEvent(input$tolcor, {
    if(is.na(input$tolcor) == F){
      if(input$tolcor > 10 | input$tolcor < 0) {
        updateNumericInput(session, 'tolcor', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 10. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$tolmov, {
    if(is.na(input$tolmov) == F){
      if(input$tolmov > 145 | input$tolmov < 0) {
        updateNumericInput(session, 'tolmov', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 145. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$tollat, {
    if(is.na(input$tollat) == F){
      if(input$tollat > 2000 | input$tollat < 0) {
        updateNumericInput(session, 'tollat', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 2000. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$toleje, {
    if(is.na(input$toleje) == F){
      if(input$toleje > 2000 | input$toleje < 0) {
        updateNumericInput(session, 'toleje', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 2000. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$tolres, {
    if(is.na(input$tolres) == F){
      if(input$tolres > 2000 | input$tolres < 0) {
        updateNumericInput(session, 'tolres', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 2000. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  # otros tests
  observeEvent(input$sdmta, {
    if(is.na(input$sdmta) == F){
      if(input$sdmta > 110 | input$sdmta < 0) {
        updateNumericInput(session, 'sdmta', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 110. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$sdmte, {
    if(is.na(input$sdmte) == F){
      if(input$sdmte > 20 | input$sdmte < 0) {
        updateNumericInput(session, 'sdmte', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 20. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$simil, {
    if(is.na(input$simil) == F){
      if(input$simil > 26 | input$simil < 0) {
        updateNumericInput(session, 'simil', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 26. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  observeEvent(input$calc, {
    if(is.na(input$calc) == F){
      if(input$calc > 10 | input$calc < 0) {
        updateNumericInput(session, 'calc', value = NA_real_)
        shinyalert("Valor fuera de rango", "El valor debe ser un numero entre 0 y 10. La puntuacion no se corregira hasta que se introduzca un valor valido.", type = "error")
      }
    }
  })
  
  #########################
  ### Reactives Scores ###
  #######################
  
  # Curva AVL
  a1_adj <- reactive({a1_fun(input$a1, input$edad)})
  a2_adj <- reactive({a2_fun(input$a2, input$edad)})
  a3_adj <- reactive({a3_fun(input$a3, input$edad)})
  a4_adj <- reactive({a4_fun(input$a4, input$edad)})
  a5_adj <- reactive({a5_fun(input$a5, input$edad)})
  b_adj <- reactive({b_fun(input$b, input$edad)})
  a6_adj <- reactive({a6_fun(input$a6, input$edad)})
  a7_adj <- reactive({a7_fun(input$a7, input$edad)})
  r_adj <- reactive({r_fun(input$r, input$edad)})
  
  # Calculos AVL
  fpr <- reactive({input$ras + input$raf})
  aptot <- reactive({aptot_fun(input$a1, input$a2, input$a3, input$a4, input$a5)})
  recuper <- reactive({recuper_fun(input$a7, input$r, fpr())})
  apren <- reactive({apren_fun(input$a5, input$a1)})
  recon <- reactive({recon_fun(input$r, fpr())})
  infover <- reactive({infover_fun(input$digitsf, input$a1)})
  retriefi <- reactive({retriefi_fun(input$a7, input$r)})
  stpret <- reactive({avl_calc_fun(input$a5, input$a6)})
  ltperrat <- reactive({avl_calc_fun(input$a5, input$a7)})
  
  aptot_adj <- reactive({aptott_fun(aptot(), input$edad)})
  recuper_adj <- reactive({recupert_fun(recuper(), input$edad, input$sexo)})
  apren_adj <- reactive({aprent_fun(apren(), input$edad)})
  recon_adj <- reactive({recont_fun(recon(), input$edad, input$sexo)})
  fpr_adj <- reactive({fpr_fun(fpr(), input$edad, input$sexo)})
  infover_adj <- reactive({infovert_fun(infover(), input$edad, input$sexo)})
  retriefi_adj <- reactive({retriefit_fun(retriefi(), input$edad, input$sexo)})
  stpret_adj <- reactive({avlt_calct_fun(stpret(), input$edad, 'stpret')})
  ltperrat_adj <- reactive({avlt_calct_fun(ltperrat(), input$edad, 'ltperrat')})
  
  
  # Praxias
  secpos <- reactive({secpos_fun(input$secgraf, input$secmot)})
  
  imitd_adj <- reactive({imitd_fun(input$imitd, input$edad, input$edu)})
  imiti_adj <- reactive({imiti_fun(input$imiti, input$edad, input$edu)})
  imitb_adj <- reactive({imitb_fun(input$imitb, input$edad, input$edu)})
  coord_adj <- reactive({coord_fun(input$coord, input$edad, input$edu)})
  secpos_adj <- reactive({secpost_fun(secpos(), input$edad, input$edu)})
  
  # Control Mental WMS-III
  cm_adj <- reactive({cm_fun(input$cm, input$edad)})
  
  # FCR-recon
  fcrtn <- reactive({fcrtn_fun(input$fcrfp)})
  fcrfn <- reactive({fcrtn_fun(input$fcrtp)})
  fcr_recon <- reactive({fcr_recon_fun(input$fcrtp, fcrtn())})
  
  fcr_recon_adj <- reactive({fcr_recont_fun(fcr_recon(), input$edad, type = 'fcr_recon')})
  fcrtp_adj <- reactive({fcr_recont_fun(input$fcrtp, input$edad, type = 'fcrtp')})
  fcrtn_adj <- reactive({fcr_recont_fun(fcrtn(), input$edad, type = 'fcrtn')})
  fcrfp_adj <- reactive({fcr_recont_fun(input$fcrfp, input$edad, type = 'fcrfp')})
  fcrfn_adj <- reactive({fcr_recont_fun(fcrfn(), input$edad, type = 'fcrfn')})
  
  # Cubos 
  cubos_adj <- reactive({cubos_fun(input$cubos, input$edad)})
  
  # Neuronorma
  digitsf_adj <- reactive({neuronorma_fun(input$digitsf, input$edad, input$edu, 'digitsf')})
  digitsb_adj <- reactive({neuronorma_fun(input$digitsb, input$edad, input$edu, 'digitsb')})
  
  fcrc_adj <- reactive({neuronorma_fun(input$fcrc, input$edad, input$edu, 'fcr_copy')})
  fcrt_adj <- reactive({neuronorma_fun(input$fcrt, input$edad, input$edu, 'fcr_temps')})
  fcrir_adj <- reactive({neuronorma_fun(input$fcrir, input$edad, input$edu, 'fcr_ir')})
  fcrdr_adj <- reactive({neuronorma_fun(input$fcrdr, input$edad, input$edu, 'fcr_dr')})
  
  ecat_adj <- reactive({neuronorma_fun(input$ecat, input$edad, input$edu, 'ecat')})
  p_adj <- reactive({neuronorma_fun(input$p, input$edad, input$edu, 'p')})
  
  tmta_adj <- reactive({neuronorma_fun(input$tmta, input$edad, input$edu, 'tmta')})
  tmtb_adj <- reactive({neuronorma_fun(input$tmtb, input$edad, input$edu, 'tmtb')})
  
  stroopp_adj <- reactive({neuronorma_fun(input$stroopp, input$edad, input$edu, 'stroopp')})
  stroopc_adj <- reactive({neuronorma_fun(input$stroopc, input$edad, input$edu, 'stroopc')})
  strooppc_adj <- reactive({neuronorma_fun(input$strooppc, input$edad, input$edu, 'strooppc')})
  
  deciobj_adj <- reactive({neuronorma_fun(input$deciobj, input$edad, input$edu, 'deciobj')})
  silprog_adj <- reactive({neuronorma_fun(input$silprog, input$edad, input$edu, 'silprog')})
  discpos_adj <- reactive({neuronorma_fun(input$discpos, input$edad, input$edu, 'discpos')})
  locnum_adj <- reactive({neuronorma_fun(input$locnum, input$edad, input$edu, 'locnum')})
  
  tolcor_adj <- reactive({neuronorma_tol_fun(input$tolcor, input$edad, input$edu, 'tolcor')})
  tolmov_adj <- reactive({neuronorma_tol_fun(input$tolmov, input$edad, input$edu, 'tolmov')})
  tollat_adj <- reactive({neuronorma_tol_fun(input$tollat, input$edad, input$edu, 'tollat')})
  toleje_adj <- reactive({neuronorma_tol_fun(input$toleje, input$edad, input$edu, 'toleje')})
  tolres_adj <- reactive({neuronorma_tol_fun(input$tolres, input$edad, input$edu, 'tolres')})
  
  # Lenguaje 
  fas <- reactive({fas_fun(input$f, input$a, input$s)})
  comord_adj <- reactive({comord_fun(input$comord, input$edad, input$edu)})
  bnt_adj <- reactive({bnt_fun(input$bnt, input$edad, input$sexo, input$edu)})
  dimag_adj <- reactive({dimag_fun(input$dimag, input$edad, input$edu)})
  fas_adj <- reactive({fast_fun(fas(), input$edu)})
  reppal_adj <- reactive({reppal_fun(input$reppal, input$edad, input$edu)})
  replog_adj <- reactive({replog_fun(input$replog, input$edad, input$edu)})
  repfra_adj <- reactive({repfra_fun(input$repfra)})
  
  # Fc Visoespaciales
  fsl_adj <- reactive({fsl_fun(input$fsl)})
  hooper_adj <- reactive({hooper_fun(input$hooper, input$edad, input$edu)})
  jlo_adj <- reactive({jlo_fun(input$jlo, input$edad, input$sexo)})
  clockr_adj <- reactive({clockr_fun(input$clockr, input$edad)})
  t15obj_adj <- reactive({t15obj_fun(input$`15obj`)})
  
  # Otros FFEE
  sdmta_adj <- reactive({sdmto_fun(input$sdmta, input$edad, input$edu)})
  calc_adj <- reactive({calc_fun(input$calc, input$edad, input$edu)})
  simil_adj <- reactive({simil_fun(input$simil, input$edad, input$edu)})
  
  
  ###########################
  ### DATA-tables Scores ###
  #########################
  
  # Datatables AVL
  avl_scores_raw <- reactive({c(input$a1, 
                                input$a2, 
                                input$a3, 
                                input$a4,
                                input$a5, 
                                input$b, 
                                input$a6, 
                                input$a7,
                                input$r)}) 
  avl_scores_adj <- reactive({c(a1_adj(), 
                                a2_adj(), 
                                a3_adj(), 
                                a4_adj(),
                                a5_adj(),
                                b_adj(),
                                a6_adj(),
                                a7_adj(),
                                r_adj())}) 
  correccion_avl_1 <- reactive({data.frame(Orden = 1:length(avl_names1), Test = avl_names1, Punt.D = avl_scores_raw(), Punt.T = avl_scores_adj())})
  
  avl_calcs_raw <- reactive({c(aptot(),
                               apren(),
                               recuper()*100,
                               stpret(), 
                               ltperrat(),
                               retriefi()*100, 
                               recon()*100,
                               fpr(),
                               infover()*100
  )})
  avl_calcs_adj <- reactive({c(aptot_adj(), 
                               apren_adj(),
                               recuper_adj(),
                               stpret_adj(),
                               ltperrat_adj(),
                               retriefi_adj(), 
                               recon_adj(),
                               fpr_adj(),
                               infover_adj()
                               )})
  correccion_avl_2 <- reactive({data.frame(Orden = 1:length(avl_names2), Test = avl_names2, Punt.D = avl_calcs_raw(), Punt.T = avl_calcs_adj())})
  
  # Data-table digitos-praxias-control_mental (digprx)
  digprx_scores_raw <- reactive({c(input$digitsf, 
                                   input$digitsb,
                                   input$cm,
                                   input$imitd,
                                   input$imiti, 
                                   input$imitb, 
                                   input$coord,
                                   secpos())})
  digprx_scores_adj <- reactive({c(digitsf_adj(), 
                                   digitsb_adj(), 
                                   cm_adj(),
                                   imitd_adj(), 
                                   imiti_adj(),
                                   imitb_adj(), 
                                   coord_adj(), 
                                   secpos_adj())})
  correccion_digprx <- reactive({data.frame(Orden = 1:length(digprx_names), Test = digprx_names, Punt.D = digprx_scores_raw(), Punt.T = digprx_scores_adj())})
  
  # Data-table fcr-cubos-reloj
  visoconst_scores_raw <- reactive({c(input$reloj,
                                      input$cubos,
                                      input$fcrc, 
                                      input$fcrt, 
                                      input$fcrir,
                                      input$fcrdr, 
                                      fcr_recon(), 
                                      input$fcrtp, 
                                      input$fcrfp,
                                      fcrtn(), 
                                      fcrfn())})
  visoconst_scores_adj <- reactive({c(NA_real_,
                                      cubos_adj(),
                                      fcrc_adj(), 
                                      fcrt_adj(), 
                                      fcrir_adj(),
                                      fcrdr_adj(), 
                                      fcr_recon_adj(), 
                                      fcrtp_adj(),
                                      fcrfp_adj(),
                                      fcrtn_adj(),
                                      fcrfn_adj())})
  correccion_visoconst <- reactive({data.frame(Orden = 1:length(visoconst_names), Test = visoconst_names,  Punt.D = visoconst_scores_raw(), Punt.T = visoconst_scores_adj())})
  
  # Data-table Lenguaje
  lenguaje_scores_raw <- reactive({c(input$bnt,
                                     input$dimag,
                                     input$comord,
                                     input$reppal,
                                     input$replog, 
                                     input$repfra,
                                     input$p,
                                     input$prep, 
                                     input$pint, 
                                     input$ecat,
                                     input$ecatrep, 
                                     input$ecatint,
                                     fas())})
  lenguaje_scores_adj <- reactive({c(bnt_adj(),
                                     dimag_adj(),
                                     comord_adj(),
                                     reppal_adj(),
                                     replog_adj(),
                                     repfra_adj(),
                                     p_adj(),
                                     NA_real_,
                                     NA_real_,
                                     ecat_adj(),
                                     NA_real_,
                                     NA_real_,
                                     fas_adj())})
  correccion_lenguaje <- reactive({data.frame(Orden = 1:length(lenguaje_names), Test = lenguaje_names,  Punt.D = lenguaje_scores_raw(), Punt.T = lenguaje_scores_adj())})
  
  # Data-table Visopercepcion
  visospatial_scores_raw <- reactive({c(input$jaeger,
                                        input$`15obj`,
                                        input$fsl, 
                                        input$hooper,
                                        input$bnte,
                                        input$jlo,
                                        input$clockr,
                                        input$deciobj, 
                                        input$silprog, 
                                        input$discpos, 
                                        input$locnum)})
  visospatial_scores_adj <- reactive({c(NA_real_,
                                        t15obj_adj(), 
                                        fsl_adj(),
                                        hooper_adj(),
                                        NA_real_,
                                        jlo_adj(),
                                        clockr_adj(),
                                        deciobj_adj(),
                                        silprog_adj(), 
                                        discpos_adj(),
                                        locnum_adj())})
  correccion_visospatial <- reactive({data.frame(Orden = 1:length(visospatial_names), Test = visospatial_names,  Punt.D = visospatial_scores_raw(), Punt.T = visospatial_scores_adj())})
  
  # Data-table FFEE I
  ffee1_scores_raw <- reactive({c(input$stroopp,
                                  input$strooppe,
                                  input$stroopc,
                                  input$stroopce,
                                  input$strooppc,
                                  input$strooppce,
                                  input$tmta,
                                  input$tmtae, 
                                  input$tmtb,
                                  input$tmtbe)})
  ffee1_scores_adj <- reactive({c(stroopp_adj(),
                                  NA_real_,
                                  stroopc_adj(),
                                  NA_real_,
                                  strooppc_adj(),
                                  NA_real_, 
                                  tmta_adj(),
                                  NA_real_,
                                  tmtb_adj(),
                                  NA_real_)})
  correccion_ffee1 <- reactive({data.frame(Orden = 1:length(ffee1_names), Test = ffee1_names,  Punt.D = ffee1_scores_raw(), Punt.T = ffee1_scores_adj())})
  
  # Data-table FFEE II
  ffee2_scores_raw <- reactive({c(input$ascor,
                                  input$sdmta,
                                  input$sdmte,
                                  input$calc,
                                  input$simil,
                                  input$tolcor,
                                  input$tolmov,
                                  input$tollat,
                                  input$toleje,
                                  input$tolres)})
  ffee2_scores_adj <- reactive({c(NA_real_,
                                  sdmta_adj(),
                                  NA_real_,
                                  calc_adj(),
                                  simil_adj(),
                                  tolcor_adj(),
                                  tolmov_adj(),
                                  tollat_adj(),
                                  toleje_adj(),
                                  tolres_adj())})
  correccion_ffee2 <- reactive({data.frame(Orden = 1:length(ffee2_names), Test = ffee2_names,  Punt.D = ffee2_scores_raw(), Punt.T = ffee2_scores_adj())})
  
  
  ########################
  ### IMPORTACION CSV ###
  ######################
  datos_csv <- eventReactive(input$resumengo, {
    data.frame(
      nhc = input$nhc, 
      fecha_np = input$fecha_np, 
      edad = input$edad,
      sexo = input$sexo, 
      educacion = input$edu, 
      dominancia = input$dominancia, 
      neuropsico = input$neuropsicologa, 
      doctor = input$doctor, 
      a1 = input$a1, 
      a1_adj = a1_adj(),
      a2 = input$a2, 
      a2_adj = a2_adj(),
      a3 = input$a3, 
      a3_adj = a3_adj(),
      a4 = input$a4, 
      a4_adj = a4_adj(),
      a5 = input$a5, 
      a5_adj = a5_adj(),
      a6 = input$a6, 
      a6_adj = a6_adj(), 
      a7 = input$a7, 
      a7_adj = a7_adj(),
      b1 = input$b, 
      b1_adj = b_adj(), 
      a_rec = input$r,
      a_rec_adj = r_adj(), 
      ras = input$raf,
      raf = input$ras,
      fpb = input$recb,
      aptot = aptot(), 
      aptot_adj = aptot_adj(), 
      apren_ind = apren(), 
      apren_ind_adj = apren_adj(), 
      recuper = recuper(), 
      recuper_adj = recuper_adj(), 
      recuper_short = stpret(),
      recuper_short_adj = stpret_adj(),
      recuper_long = ltperrat(),
      recuper_long_adj = ltperrat_adj(), 
      recuper_efi = retriefi(), 
      recuper_efi_adj = retriefi_adj(), 
      recon = recon(), 
      recon_adj = recon_adj(), 
      fpr = fpr(), 
      fpr_adj = fpr_adj(), 
      infover = infover(), 
      infover_adj = infover_adj(), 
      digitosf = input$digitsf, 
      digitosf_adj = digitsf_adj(), 
      digitosb = input$digitsb, 
      digitosb_adj = digitsb_adj(),
      cont_mental = input$cm,
      cont_mental_adj = cm_adj(), 
      prx_imitd = input$imitd, 
      prx_imitd_adj = imitd_adj(), 
      prx_imiti = input$imiti, 
      prx_imiti_adj = imiti_adj(),
      prx_imitb = input$imitb, 
      prx_imitb_adj = imitb_adj(), 
      prx_coord = input$coord, 
      prx_coord_adj = coord_adj(),
      prx_secmot = input$secmot, 
      prx_secgraf = input$secgraf,
      prx_sec = secpos(), 
      prx_sec_adj = secpos_adj(),
      cubos = input$cubos, 
      cubos_adj = cubos_adj(), 
      reloj = input$reloj,
      fcr_copy = input$fcrc, 
      fcr_copy_adj = fcrc_adj(), 
      fcr_time = input$fcrt, 
      fcr_time_adj = fcrt_adj(), 
      fcr_ir = input$fcrir,
      fcr_ir_adj = fcrir_adj(), 
      fcr_dr = input$fcrdr, 
      fcr_dr_adj = fcrdr_adj(), 
      fcr_recon_total = fcr_recon(), 
      fcr_recon_total_adj = fcr_recon_adj(),
      fcr_recon_tp = input$fcrtp,
      fcr_recon_tp_adj = fcrtp_adj(), 
      fcr_recon_fp = input$fcrfp, 
      fcr_recon_fp_adj = fcrfp_adj(), 
      fcr_recon_tn = fcrtn(), 
      fcr_recon_tn_adj = fcrtn_adj(), 
      fcr_recon_fn = fcrfn(), 
      fcr_recon_fn_adj = fcrfn_adj(), 
      bnt30 = input$bnt,
      bnt30e = input$bnte,
      bnt30_adj = bnt_adj(), 
      denom_bcn = input$dimag,
      denom_bcn_adj = dimag_adj(), 
      comord = input$comord, 
      comord_adj = comord_adj(), 
      reppal = input$reppal,
      reppal_adj = reppal_adj(), 
      repfra = input$repfra, 
      repfra_adj = repfra_adj(),
      replog = input$replog, 
      replog_adj = replog_adj(), 
      flu_p = input$p,
      flu_p_adj = p_adj(),
      flu_p_rep = input$prep,
      flu_p_int = input$pint,
      flu_animales = input$ecat, 
      flu_animales_adj = ecat_adj(), 
      flu_animales_rep = input$ecatrep,
      flu_animales_int = input$ecatint,
      fas_f = input$f, 
      fas_a = input$a, 
      fas_s = input$s, 
      fas_total = fas(), 
      fas_total_adj = fas_adj(), 
      jaeger = input$jaeger,
      t15obj = input$`15obj`,
      t15obj_adj = t15obj_adj(), 
      fsl = input$fsl, 
      fsl_adj = fsl_adj(), 
      hooper = input$hooper, 
      hooper_adj = hooper_adj(), 
      jlo = input$jlo, 
      jlo_adj = jlo_adj(),
      read_clock = input$clockr, 
      read_clock_adj = clockr_adj(), 
      vosp_deciobj =input$deciobj, 
      vosp_deciobj_adj = deciobj_adj(), 
      vosp_silprog = input$silprog, 
      vosp_silprog_adj = silprog_adj(), 
      vosp_discpos = input$discpos, 
      vosp_discpos_adj = discpos_adj(), 
      vosp_locnum = input$locnum, 
      vosp_locnum_adj = locnum_adj(),  
      stroopp = input$stroopp,
      stroopp_errores = input$strooppe,
      stroopp_adj = stroopp_adj(),
      stroopc = input$stroopc,
      stroopc_errores = input$stroopce,
      stroopc_adj = stroopc_adj(), 
      strooppc = input$strooppc,
      strooppc_errores = input$strooppce,
      strooppc_adj = strooppc_adj(),
      tmta_seg = input$tmta, 
      tmta_errores = input$tmtae, 
      tmt_seg_adj = tmta_adj(), 
      tmtb_seg = input$tmtb, 
      tmtb_errores = input$tmtbe,
      tmtb_seg_adj = tmtb_adj(), 
      calculo_mental = input$calc, 
      calculo_mental_adj = calc_adj(), 
      semejanzas = input$simil, 
      smejanzas_adj = simil_adj(), 
      as_correctas = input$ascor, 
      as_omisiones = input$ascoro, 
      as_persever = input$ascorp, 
      sdmto_correctos = input$sdmta,
      sdmto_errores = input$sdmte, 
      sdmto_adj = sdmta_adj(), 
      tol_correctos = input$tolcor, 
      tol_correctos_adj = tolcor_adj(), 
      tol_movimientos = input$tolmov, 
      tol_movimientos_adj = tolmov_adj(), 
      tol_latencia = input$tollat, 
      tol_latencia_adj = tollat_adj(), 
      tol_ejecucion = input$toleje,
      tol_ejecucion_adj = toleje_adj(), 
      tol_resolucion = input$tolres, 
      tol_resolucion_adj = tolres_adj(),
      otros_tests = input$comments_tests, 
      colabora = input$colabora,
      conducta = paste(c(input$obervaciones), collapse = '|'),
      comments_cond = str_replace_all(input$comments_cond, "[^[:alnum:]]", " "),
      comments_leng = str_replace_all(input$comments_leng, "[^[:alnum:]]", " "),
      version_corrector = 1.1
    ) # data.frame
  }# reactive
  )# event

  #######################
  ###### RESUMEN #######
  #####################
  resum_df <- eventReactive(input$resumengo, {
    data.frame(funcion = c('Mem.Verbal',
                           'Mem.Verbal',
                           'Mem.Verbal',
                           'Mem.Visual',
                           'Mem.Visual',
                           'Mem.Visual',
                           'Atencion.Span',
                           'Atencion.Span',
                           'Atencion.Span',
                           'Atencion.Span',
                           'Praxias',
                           'Praxias',
                           'Praxias',
                           'Praxias',
                           'Praxias',
                           'Lenguaje',
                           'Lenguaje',
                           'Lenguaje',
                           'Lenguaje',
                           'Lenguaje',
                           'Fluencia.Verbal',
                           'Fluencia.Verbal',
                           'Fluencia.Verbal',
                           'Fc.Visoperceptivas',
                           'Fc.Visoperceptivas',
                           'Fc.Visoperceptivas',
                           'Fc.Visoperceptivas',
                           'Fc.Visoperceptivas',
                           'Fc.Visoespaciales',
                           'Fc.Visoespaciales',
                           'Fc.Visoespaciales',
                           'Fc.Visoespaciales',
                           'Fc.Visoconstructivas',
                           'Fc.Visoconstructivas',
                           'FFEE',
                           'FFEE',
                           'FFEE',
                           'FFEE',
                           'FFEE',
                           'FFEE',
                           'FFEE',
                           'FFEE'
                        ),
               puntadj = c(aptot_adj(), # mem.verbal
                           recuper_adj(),  # mem.verbal
                           recon_adj(), # mem.verbal
                           fcrdr_adj(), # mem.visual
                           fcrir_adj(), # mem.visual
                           fcr_recon_adj(), # mem.visual
                           digitsf_adj(), # atencion.span
                           stroopp_adj(), # atencion.span
                           stroopc_adj(), # atencion.span
                           tmta_adj(), # atencion.span
                           imitb_adj(), # prx
                           imitd_adj(), # prx
                           imiti_adj(), # prx
                           coord_adj(), # prx
                           secpos_adj(), # prx
                           dimag_adj(), # lenguaje
                           bnt_adj(), # lenguaje
                           comord_adj(), # lenguaje
                           reppal_adj(), # lenguaje
                           replog_adj(), # lenguaje
                           ecat_adj(), # fl.verbal
                           p_adj(), # fl.verbal
                           fas_adj(), # fl.verbal
                           fsl_adj(), # fc.visoperceptivas
                           t15obj_adj(), # fc.visoperceptivas
                           hooper_adj(), # fc.visoperceptivas
                           silprog_adj(), # fc.visoperceptivas 
                           deciobj_adj(), # fc.visoperceptivas
                           jlo_adj(), # fc.visoespaciales
                           clockr_adj(), # fc.visoespaciales 
                           locnum_adj(), # fc.visoespaciales
                           discpos_adj(), # fc.visoespaciales
                           cubos_adj(), # fc.visoconstructivas
                           fcrc_adj(), # fc.visoconstructivas
                           tmtb_adj(), # ffee
                           strooppc_adj(), # ffee
                           digitsb_adj(), # ffee
                           cm_adj(), # ffee
                           tolcor_adj(), # ffee
                           simil_adj(), # ffee
                           sdmta_adj(), # ffee
                           calc_adj() # ffee
                           )
               )
    })
  resum_df_grouped <- reactive({
    na.omit(data.frame(resum_df()%>%
                         group_by(funcion)%>%
                         summarise(mean = round_fun(mean(puntadj, na.rm = T)))
                       )
    )
  })
  
  ###################################################
  ############## Outputs & Renders #################
  #################################################
  
  #############################
  # Data-tables correcciones #
  ###########################
  output$correccion_avl_1 <- renderDataTable({ # AVL
    datatable(correccion_avl_1(), extensions = c('Responsive', 'Buttons'), rownames = F,
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#338DFF', 'color': '#fff'});",
                  "}"),
                dom = 'Bt',
                buttons = c('excel', 'pdf'),
                columnDefs = list(list(className = 'dt-center', targets = c(0,2,3)))
              )
    ) %>%
      formatStyle('Punt.T', color = styleInterval(c(37,60), c('red', 'normal', '#46D822'))) %>%
      formatStyle('Punt.T', fontWeight = styleInterval(c(37,60), c('bold','normal', 'bold')))
  })
  output$correccion_avl_2 <- renderDataTable({ # AVL calculos
    datatable(correccion_avl_2(), extensions = c('Responsive', 'Buttons'), rownames = F,
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#338DFF', 'color': '#fff'});",
                  "}"),
                dom = 'Bt',
                buttons = c('excel', 'pdf'),
                columnDefs = list(list(className = 'dt-center', targets = c(0,2,3)))
              ) 
    ) %>%
      formatStyle('Punt.T', color = styleInterval(c(37,60), c('red', 'normal', '#46D822'))) %>%
      formatStyle('Punt.T', fontWeight = styleInterval(c(37,60), c('bold','normal', 'bold')))
  })
  output$correccion_digprx <- renderDataTable({ # Digprx
    datatable(correccion_digprx(), extensions = c('Responsive', 'Buttons'), rownames = F,
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#338DFF', 'color': '#fff'});",
                  "}"),
                dom = 'Bt',
                buttons = c('excel', 'pdf'),
                columnDefs = list(list(className = 'dt-center', targets = c(0,2,3)))
              )
    )%>%
      formatStyle('Punt.T', color = styleInterval(c(37,60), c('red', 'normal', '#46D822'))) %>%
      formatStyle('Punt.T', fontWeight = styleInterval(c(37,60), c('bold','normal', 'bold')))
  })
  output$correccion_visoconst <- renderDataTable({ # Visoconst
    datatable(correccion_visoconst(), extensions = c('Responsive', 'Buttons'), rownames = F,
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#338DFF', 'color': '#fff'});",
                  "}"),
                dom = 'Bt',
                buttons = c('excel', 'pdf'),
                pageLength = 15,
                columnDefs = list(list(className = 'dt-center', targets = c(0,2,3)))
              )
    )%>% 
      formatStyle('Punt.T', color = styleInterval(c(37,60), c('red', 'normal', '#46D822'))) %>%
      formatStyle('Punt.T', fontWeight = styleInterval(c(37,60), c('bold','normal', 'bold')))
  })
  output$correccion_lenguaje <- renderDataTable({ # lenguaje
    datatable(correccion_lenguaje(), extensions = c('Responsive', 'Buttons'), rownames = F,
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#338DFF', 'color': '#fff'});",
                  "}"),
                dom = 'Bt',
                buttons = c('excel', 'pdf'),
                pageLength = 15,
                columnDefs = list(list(className = 'dt-center', targets = c(0,2,3)))
              )
    )%>%
      formatStyle('Punt.T', color = styleInterval(c(37,60), c('red', 'normal', '#46D822'))) %>%
      formatStyle('Punt.T', fontWeight = styleInterval(c(37,60), c('bold','normal', 'bold')))
  })
  output$correccion_visospatial <- renderDataTable({ # visospatial
    datatable(correccion_visospatial(), extensions = c('Responsive', 'Buttons'), rownames = F,
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#338DFF', 'color': '#fff'});",
                  "}"),
                dom = 'Bt',
                buttons = c('excel', 'pdf'),
                columnDefs = list(list(className = 'dt-center', targets = c(0,2,3)))
              )
    )%>%
      formatStyle('Punt.T', color = styleInterval(c(37,60), c('red', 'normal', '#46D822'))) %>%
      formatStyle('Punt.T', fontWeight = styleInterval(c(37,60), c('bold','normal', 'bold')))
  })
  output$correccion_ffee1 <- renderDataTable({ # FFEE-I
    datatable(correccion_ffee1(), extensions = c('Responsive', 'Buttons'), rownames = F,
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#338DFF', 'color': '#fff'});",
                  "}"),
                dom = 'Bt',
                buttons = c('excel', 'pdf'),
                columnDefs = list(list(className = 'dt-center', targets = c(0,2,3)))
              )
    )%>%
      formatStyle('Punt.T', color = styleInterval(c(37,60), c('red', 'normal','#46D822'))) %>%
      formatStyle('Punt.T', fontWeight = styleInterval(c(37,60), c('bold','normal', 'bold')))
  })
  output$correccion_ffee2 <- renderDataTable({ # FFEE-II
    datatable(correccion_ffee2(), extensions = c('Responsive', 'Buttons'), rownames = F,
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#338DFF', 'color': '#fff'});",
                  "}"),
                dom = 'Bt',
                buttons = c('excel', 'pdf'),
                columnDefs = list(list(className = 'dt-center', targets = c(0,2,3)))
              )
    )%>%
      formatStyle('Punt.T', color = styleInterval(c(37,60), c('red', 'normal','#46D822'))) %>%
      formatStyle('Punt.T', fontWeight = styleInterval(c(37,60), c('bold','normal', 'bold')))
  })
  
  ############
  # RESUMEN #
  ##########
  output$memverbinfo <- renderInfoBox({
    infoBox(
      title = 'Memoria Verbal', resum_df_grouped()[resum_df_grouped()[,1] == 'Mem.Verbal',2], icon = icon('fas fa-database'),
      color = 'blue'
    )
  })
  output$memvisuinfo <- renderInfoBox({
    infoBox(
      title = 'Memoria Visual', resum_df_grouped()[resum_df_grouped()[,1] == 'Mem.Visual',2], icon = icon('far fa-images'),
      color = 'blue'
    )
  })
  output$atencioninfo <- renderInfoBox({
    infoBox(
      title = 'Atencion y Span', resum_df_grouped()[resum_df_grouped()[,1] == 'Atencion.Span',2], icon = icon('fas fa-filter'),
      color = 'blue'
    )
  })
  output$prxinfo <- renderInfoBox({
    infoBox(
      title = 'Praxias', resum_df_grouped()[resum_df_grouped()[,1] == 'Praxias',2], icon = icon('fas fa-american-sign-language-interpreting'),
      color = 'blue'
    )
  })
  output$lenginfo <- renderInfoBox({
    infoBox(
      title = 'Lenguaje', resum_df_grouped()[resum_df_grouped()[,1] == 'Lenguaje',2], icon = icon('fas fa-comment'),
      color = 'blue'
    )
  })
  output$fluverbinfo <- renderInfoBox({
    infoBox(
      title = 'Fluencias Verbales', resum_df_grouped()[resum_df_grouped()[,1] == 'Fluencia.Verbal',2], icon = icon('fas fa-cat'),
      color = 'blue'
    )
  })
  output$visopercinfo <- renderInfoBox({
    infoBox(
      title = 'Fc.Visoperceptivas', resum_df_grouped()[resum_df_grouped()[,1] == 'Fc.Visoperceptivas',2], icon = icon('fas fa-eye'),
      color = 'blue'
    )
  })
  output$visoespinfo <- renderInfoBox({
    infoBox(
      title = 'Fc.Visoespaciales', resum_df_grouped()[resum_df_grouped()[,1] == 'Fc.Visoespaciales',2], icon = icon('fas fa-clock'),
      color = 'blue'
    )
  })
  output$constructinfo <- renderInfoBox({
    infoBox(
      title = 'Fc. Visoconstructivas', resum_df_grouped()[resum_df_grouped()[,1] == 'Fc.Visoconstructivas',2], icon = icon('fas fa-pencil-ruler'),
      color = 'blue'
    )
  })
  output$ffeeinfo <- renderInfoBox({
    infoBox(
      title = 'Fc.Ejecutivas', resum_df_grouped()[resum_df_grouped()[,1] == 'FFEE',2], icon = icon('fas fa-code-branch'),
      color = 'blue'
    )
  })
  
  output$resumenplot <- renderPlot({
    req(resum_df_grouped())
    ggplot(resum_df_grouped(), aes(resum_df_grouped()[,'funcion'], resum_df_grouped()[,'mean'], group = 1))+
      scale_fill_brewer(palette = 'Paired')+
      theme(panel.background = element_blank(),
            text = element_text(size = 14, colour = 'black'),
            axis.line = element_line(colour = 'black'),
            legend.position = 'none', 
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = 'black', size = 14), 
            axis.text.y = element_text(color = 'black', size = 14))+
      scale_y_continuous(limits = c(0, 150), breaks = seq(0,150,10))+
      geom_hline(yintercept = 37, linetype = 'dashed', size = 1, color = 'red')+
      geom_hline(yintercept = 20, linetype = 'dashed', size = 1.5, color = 'red')+
      geom_hline(yintercept = 63, linetype = 'dashed', size = 1, color = 'green')+
      geom_hline(yintercept = 80, linetype = 'dashed', size = 1.5, color = 'green')+
      geom_point(color = 'blue', size = 3)+
      geom_line(color = 'blue', size = 0.8)+
      ylab('Puntuacion T') + xlab('')+
      ggtitle('Resumen Puntuaciones')
  })
  
  output$download_data_np <- downloadHandler(
    filename = function() {
      paste(input$nhc, '_', input$fecha_np, '.csv', sep = '')
    },
    content = function(filename) {
      write.table(data_np(), filename, row.names = FALSE, sep = ',')
    }
  )
  
  
  #################
  # Download CSV #
  ###############
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0(input$nhc, '_', input$fecha_np, '.txt')
    },
    content = function(file) {
      write.csv(datos_csv(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$download_csv2, {
    write.table(datos_csv(), paste0('csv/',input$nhc, '_', input$fecha_np, '.txt'), row.names = F, sep = ',')
  })
  
  ############
  # Informe #
  ##########
 
      
  # Obejetos-tablas
  personal_data_rmd <- eventReactive(input$resumengo, {
    data.frame(Variable = c('NHC', 
                            'Neuropsicologo',
                            'Doctor Responsable', 
                            'Edad', 
                            'Sexo',
                            'Educacion',
                            'Dominancia', 
                            'Colaborador'
    ),
    Valor = c(input$nhc,
              input$neuropsicologa,
              input$doctor,
              input$edad, 
              input$sexo, 
              input$edu, 
              input$dominancia,
              input$colabora
    )
    )
  })
  
  avl_data_rmd <- eventReactive(input$resumengo, {
    data.frame(Test = c('A-1', 
                        'A-2',
                        'A-3', 
                        'A-4', 
                        'A-5',
                        'B-1',
                        'A-6',
                        'A-7',
                        'R-Verdaderos Positivos',
                        'R-Falsos Positivos',
                        'Aprendizaje total',
                        'Indice de Aprendizaje',
                        '% Recuperacion Informacion Almacenada', 
                        '% Recuperacion a Corto-Plazo',
                        '% Recuperacion a Largo-Plazo',
                        '% Eficiencia Recuperacion',
                        'Reconocimiento',
                        '% Sobrecarga Informacion'
    ),
    Punt.D = c(input$a1, 
               input$a2,
               input$a3, 
               input$a4, 
               input$a5,
               input$b,
               input$a6, 
               input$a7,
               input$r,
               fpr(),
               aptot(),
               apren(),
               recuper()*100, 
               stpret(), 
               ltperrat(),
               retriefi()*100,
               recon()*100, 
               infover()*100
               ),
    Punt.T = c(a1_adj(),
               a2_adj(),
               a3_adj(),
               a4_adj(),
               a5_adj(),
               b_adj(),
               a6_adj(),
               a7_adj(),
               r_adj(), 
               fpr_adj(),
               aptot_adj(),
               apren_adj(),
               recuper_adj(), 
               stpret_adj(),
               ltperrat_adj(),
               retriefi_adj(),
               recon_adj(),
               infover_adj()
               ),
    Test.Grupo = c(rep('Puntuaciones Ensayos', 10),
                  rep('Puntuaciones Derivadas', 8)
                  ),
    Funcion = rep('Memoria Audioverbal', 18),
    Indice = 1:18
    )
  })
  
  visoconst_data_rmd <- eventReactive(input$resumengo, {
    data.frame(Test = c('Reloj-Orden', 
                        'Cubos-WAIS',
                        'FCR-Copia',
                        'FCR-Tiempo'
    ),
    Punt.D = c(input$reloj,
               input$cubos,
               input$fcrc, 
               input$fcrt
    ),
    Punt.T = c(NA_real_, 
               cubos_adj(), 
               fcrc_adj(), 
               fcrt_adj()
    ),
    Test.Grupo = c('Reloj a la orden', 
                   'Cubos WAIS-III',
                   rep('Figura compleja de Rey', 2)),
    Funcion = c(NA_real_, rep('Funciones Visoconstructivas', 3)), 
    Indice = 1:4
    
    )
  })
  
  memvisual_data_rmd <- eventReactive(input$resumengo, {
    data.frame(Test = c('FCR-Recuerdo Immediato',
                        'FCR-Recuerdo Diferido',
                        'FCR-Reconocimiento', 
                        'FCR-Falsos Positivos'
    ),
    Punt.D = c(input$fcrir,
               input$fcrdr, 
               fcr_recon(),
               input$fcrfp
    ),
    Punt.T = c(fcrir_adj(),
               fcrdr_adj(),
               fcr_recon_adj(),
               fcrfp_adj()),
    Test.Grupo = c(rep('Figura compleja de Rey', 4)),
    Funcion = c(rep('Memoria Visual', 4)),
    Indice = 1:4
    )
  })
  
  lenguaje_data_rmd <- eventReactive(input$resumengo, {
    data.frame(Test = c('BNT-30 items', 
                        'BCN-14 items',
                        'Comprension ordenes',
                        'Repeticion palabras',
                        'Repeticion logotomas',
                        'Repeticion frases',
                        'Fluencia-P',
                        'P-Repeticiones',
                        'P-Intrusiones',
                        'Fluencia-Animales', 
                        'Animales Repeticiones', 
                        'Animales Intrusiones',
                        'FAS-Total'
                        ),
               
               Punt.D = c(input$bnt, 
                          input$dimag, 
                          input$comord, 
                          input$reppal, 
                          input$replog, 
                          input$repfra, 
                          input$p, 
                          input$prep,
                          input$pint,
                          input$ecat, 
                          input$ecatrep,
                          input$ecatint,
                          fas()
                          ),
               
               Punt.T = c(bnt_adj(), 
                          dimag_adj(), 
                          comord_adj(), 
                          reppal_adj(), 
                          repfra_adj(), 
                          replog_adj(), 
                          p_adj(),
                          NA_real_,
                          NA_real_,
                          ecat_adj(), 
                          NA_real_,
                          NA_real_,
                          fas_adj()
                          ),
               Test.Grupo = c(rep('Denominacion', 2), rep ('Comprension'), rep('Repeticion', 3), rep('Fluencias Verbales',7)),
               Funcion = c(rep('Lenguaje', 6), rep('FFEE', 7)),
               Indice = 1:13
               )
  })
  
  praxias_data_rmd <- eventReactive(input$resumengo, {
    data.frame(Test = c('Unilateral-Derecha', 
                        'Unilateral-Izquierda',
                        'Bilateral',
                        'Secuencias-Posturas',
                        'Coordinacion-Reciproca'
                        ),
               Punt.D = c(input$imitd,
                          input$imiti, 
                          input$imitb, 
                          secpos(), 
                          input$coord
                          ),
               Punt.T = c(imitd_adj(), 
                          imiti_adj(), 
                          imitb_adj(), 
                          secpos_adj(),
                          coord_adj()
                          ),
               Test.Grupo = c(rep('Praxias Ideomotoras', 5)),
               Funcion = c(rep('Praxias Ideomotoras', 5)),
               Indice = 1:5
               )
  })
  
  viso_data_rmd <- eventReactive(input$resumengo, {
    data.frame(Test = c('Jaeger', 
                        '15-Ojetos',
                        'Fig.Superp.-Luria',
                        'Hooper',
                        'JLO',
                        'Lectura Relojes', 
                        'VOSP-Deci.Objetos',
                        'VOSP-Siluetas.Prgr.',
                        'VOSP-Disc.Posiciones',
                        'VOSP-Loc.Numeros'
                        ),
               Punt.D = c(input$jaeger,
                          input$`15obj`,
                          input$fsl, 
                          input$hooper,
                          input$jlo,
                          input$clockr,
                          input$deciobj,
                          input$silprog, 
                          input$discpos, 
                          input$locnum
                          ),
               Punt.T = c(NA_real_, 
                          t15obj_adj(), 
                          fsl_adj(), 
                          hooper_adj(), 
                          jlo_adj(),
                          clockr_adj(), 
                          deciobj_adj(), 
                          silprog_adj(), 
                          discpos_adj(), 
                          locnum_adj()
                          ),
               Test.Grupo = c('Jaeger', rep('Funciones Visoperceptivas', 3), rep('Funciones Visoespaciales', 2), rep('VOSP', 4)), 
               Funcion = c(NA_real_, rep('Funciones Visoperceptivas', 3), rep('Funciones Visoespaciales', 2), 
                           rep('Funciones Visoperceptivas', 2), rep('Funciones Visoespaciales', 2)),
               Indice = 1:10
               )
  })
  
  ffee_data_rmd <- eventReactive(input$resumengo, {
    data.frame(Test = c('Digitos-Directos',
                        'Digitos-Inversos',
                        'Stroop-Palabra',
                        'Stroop-Palabra errores',
                        'Stroop-Color',
                        'Stroop-Color errores',
                        'Stroop-Palabra/Color',
                        'Stroop-Palabra/Color errores',
                        'TMT-A',
                        'TMT-A errores',
                        'TMT-B',
                        'TMT-B errores',
                        'Prueba As correctos',
                        'Prueba As omisiones',
                        'Prueba As perseveraciones',
                        'Control-Mental (WAIS)',
                        'SDMT Oral correctos',
                        'SDMT Oral errores',
                        'Calculo mental', 
                        'Semejanzas', 
                        'TOL-Correctos',
                        'TOL-Movimientos',
                        'TOL-Tiempo Latencia', 
                        'TOL-Tiempo Ejecucion', 
                        'TOL-Tiempo Resolucion'
                        
    ),
    Punt.D = c(input$digitsf, 
               input$digitsb, 
               input$stroopp,
               input$strooppe,
               input$stroopc, 
               input$stroopce,
               input$strooppc, 
               input$strooppce,
               input$tmta, 
               input$tmtae,
               input$tmtb,
               input$tmtbe,
               input$ascor,
               input$ascoro,
               input$ascorp,
               input$cm, 
               input$sdmta, 
               input$sdmte,
               input$calc, 
               input$simil, 
               input$tolcor, 
               input$tolmov, 
               input$tollat, 
               input$toleje, 
               input$tolres
    ),
    Punt.T = c(digitsf_adj(), 
               digitsb_adj(),
               stroopp_adj(),
               NA_real_,
               stroopc_adj(),
               NA_real_,
               strooppc_adj(), 
               NA_real_,
               tmta_adj(),
               NA_real_,
               tmtb_adj(),
               NA_real_,
               NA_real_,
               NA_real_,
               NA_real_,
               cm_adj(), 
               sdmta_adj(), 
               NA_real_,
               calc_adj(), 
               simil_adj(),
               tolcor_adj(), 
               tolmov_adj(), 
               tollat_adj(),
               toleje_adj(),
               tolres_adj()
    ),
    Test.Grupo = c(rep('Digitos WAIS-Span', 2),
                   rep('Stroop', 6),
                   rep('Trail Making Test', 4),
                   rep('Prueba de las As', 3),
                   rep('Otros Tests FFEE', 5),
                   rep('Tower of London', 5)), 
    Funcion = c(rep('FFEE', 25)),
    Indice = 1:25
    )
  })
  
  # Downloadhandler
  
  output$report <- downloadHandler(
    filename = function(){paste0(input$nhc, '_', format(Sys.Date(), '%d-%m-%y'), '_informe.html')},
    content = function(file) {
      # tempReport <- file.path(tempdir(), "report.Rmd")
      # file.copy("report.Rmd", tempReport, overwrite = TRUE)
      params <- list(plots = input$report_plots,
                     personal = personal_data_rmd(),
                     neuropsicologa = input$neuropsicologa,
                     date = as.Date(input$fecha_np),
                     avl = avl_data_rmd(),
                     mem_visual = memvisual_data_rmd(),
                     lenguaje = lenguaje_data_rmd(),
                     praxias = praxias_data_rmd(),
                     viso = viso_data_rmd(),
                     visoconst = visoconst_data_rmd(),
                     ffee = ffee_data_rmd(),
                     conducta = input$obervaciones,
                     comments_cond = input$comments_cond,
                     comments_leng = input$comments_leng
      )
      
      rmarkdown::render(input = 'report.Rmd',
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      }
    )
  
  observeEvent(input$report2, {
    params <- list(plots = input$report_plots,
                   personal = personal_data_rmd(),
                   neuropsicologa = input$neuropsicologa,
                   date = as.Date(input$fecha_np),
                   avl = avl_data_rmd(),
                   mem_visual = memvisual_data_rmd(),
                   lenguaje = lenguaje_data_rmd(),
                   praxias = praxias_data_rmd(),
                   viso = viso_data_rmd(),
                   visoconst = visoconst_data_rmd(),
                   ffee = ffee_data_rmd(),
                   conducta = input$obervaciones,
                   comments_cond = input$comments_cond,
                   comments_leng = input$comments_leng
    )
    rmarkdown::render(input = 'report.Rmd',
                      output_file = paste0('reports/', input$nhc, '_', format(Sys.Date(), '%d-%m-%y'), '_informe.html'),
                      params = params,
                      envir = new.env(parent = globalenv()))
  })

} # shinyserver