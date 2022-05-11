a1_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 6.6)/1.7
  else if (edad >= 40 & edad <= 49) output = (score - 6.6)/1.7
  else if (edad >= 50 & edad <= 59) output = (score - 6.2)/1.6
  else if (edad >= 60 & edad <= 69) output = (score - 5.9)/1.6
  else if (edad >= 70 & edad <= 84) output = (score - 5.5)/1.6 
  else if (edad >= 85) output = (score - 4.0)/1.5
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
  
}

a2_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 9.6)/2.1
  else if (edad >= 40 & edad <= 49) output = (score - 9.3)/1.9
  else if (edad >= 50 & edad <= 59) output = (score - 9.0)/1.9
  else if (edad >= 60 & edad <= 69) output = (score - 8.4)/2.0
  else if (edad >= 70 & edad <= 84) output = (score - 7.7)/2.1
  else if (edad >= 85) output = (score - 6.0)/1.8
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
  
}

a3_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 11.1)/2.1
  else if (edad >= 40 & edad <= 49) output = (score - 10.8)/2.1
  else if (edad >= 50 & edad <= 59) output = (score - 10.5)/1.9
  else if (edad >= 60 & edad <= 69) output = (score - 9.8)/2.3
  else if (edad >= 70 & edad <= 84) output = (score - 8.8)/2.1
  else if (edad >= 85) output = (score - 7.4)/2.2 

  return(ifelse((output*10+50) < 0, 0, output*10+50))
  
}

a4_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 12.0)/1.9
  else if (edad >= 40 & edad <= 49) output = (score - 11.7)/2.1 
  else if (edad >= 50 & edad <= 59) output = (score - 11.4)/1.9 
  else if (edad >= 60 & edad <= 69) output = (score - 10.9)/2.3
  else if (edad >= 70 & edad <= 84) output = (score - 9.8)/2.4 
  else if (edad >= 85) output = (score - 7.9)/2.4
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
  
}

a5_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 12.6)/1.9
  else if (edad >= 40 & edad <= 49) output = (score - 12.3)/1.9
  else if (edad >= 50 & edad <= 59) output = (score - 12.1)/2.1
  else if (edad >= 60 & edad <= 69) output = (score - 11.3)/2.3
  else if (edad >= 70 & edad <= 84) output = (score - 10.3)/2.4
  else if (edad >= 85) output = (score - 9.1)/2.3
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
  
}

b_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 6.6)/2.1
  else if (edad >= 40 & edad <= 49) output = (score - 6.1)/1.9
  else if (edad >= 50 & edad <= 59) output = (score - 5.7)/2.2
  else if (edad >= 60 & edad <= 69) output = (score - 5.1)/1.3
  else if (edad >= 70 & edad <= 84) output = (score - 3.9)/1.6 
  else if (edad >= 85) output = (score - 3.1)/1.4 
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
  
}

a6_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 11.2)/2.6
  else if (edad >= 40 & edad <= 49) output = (score - 10.4)/2.6
  else if (edad >= 50 & edad <= 59) output = (score - 9.9)/2.8
  else if (edad >= 60 & edad <= 69) output = (score - 9.3)/2.9
  else if (edad >= 70 & edad <= 84) output = (score - 8.1)/3.0
  else if (edad >= 85) output = (score - 6.2)/2.6
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
  
}

a7_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 11.0)/2.8
  else if (edad >= 40 & edad <= 49) output = (score - 10.2)/2.8
  else if (edad >= 50 & edad <= 59) output = (score - 9.9)/3.2
  else if (edad >= 60 & edad <= 69) output = (score - 8.8)/3.0
  else if (edad >= 70 & edad <= 84) output = (score - 7.0)/2.4
  else if (edad >= 85) output = (score - 5.4)/2.7
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
  
}

aptot_fun <- function(score, edad){
  if(is.na(score) | score > 75 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 53.0)/7.8
  else if (edad >= 40 & edad <= 49) output = (score - 51.1)/8.6
  else if (edad >= 50 & edad <= 59) output = (score - 47.6)/8.1 
  else if (edad >= 60 & edad <= 69) output = (score - 43.4)/7.7
  else if (edad >= 70) output = (score - 37.1)/7.5

  return(ifelse((output*10+50) < 0, 0, output*10+50))
}

r_fun <- function(score, edad){
  if(is.na(score) | score > 15 | score < 0 | is.na(edad)){
    return(NA_real_)
  }
  output <- c()
  if(edad < 40) output = (score - 14.2)/1.2 
  else if (edad >= 40 & edad <= 49) output = (score - 14.0)/1.4 
  else if (edad >= 50 & edad <= 59) output = (score - 13.9)/1.4
  else if (edad >= 60 & edad <= 69) output = (score - 13.5)/1.3
  else if (edad >= 70 & edad <= 84) output = (score - 13.3)/1.5
  else if (edad >= 85) output = (score - 12.3)/2.3
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
  
}



recuper_fun <- function(a7, r, fpr, edad, sexo){
  if(is.na(a7) | a7 > 15 | a7 < 0 | 
     is.na(r) | r > 15 | r < 0 |
     is.na(fpr) | fpr > 20 | fpr < 0 |
     is.na(edad) | is.na(sexo)){
    return(NA_real_)
  }
  
  fpr <- ifelse(a7 == 0 & r == 0 & fpr == 0, 0.1, fpr)
  r <- ifelse(a7 == 0 & r == 0, 0.1, r)
  a7 <- a7/15
  score <- a7/(0.033333*r - 0.025*fpr + 0.5)
  
  output <- c()
  
  if(sexo == 'Masculino'){
    if (edad < 20) output = (score - 0.8)/0.1
    else if (edad <= 29) output = (score - 0.8)/0.2
    else if (edad <= 39) output = (score - 0.8)/0.2
    else if (edad <= 49) output = (score - 0.8)/0.2
    else if (edad <= 59) output = (score - 0.7)/0.2
    else if (edad <= 69) output = (score - 0.6)/0.3
    else output = (score - 0.5)/0.2
  }
  
  if(sexo == 'Femenino'){
    if (edad < 20) output = (score - 0.8)/0.2
    else if(edad <= 29) output = (score - 0.8)/0.2
    else if(edad <= 39) output = (score - 0.9)/0.2
    else if(edad <= 49) output = (score - 0.8)/0.2
    else if(edad <= 59) output = (score - 0.8)/0.2
    else if(edad <= 69) output = (score - 0.8)/0.2
    else output = (score - 0.7)/0.1
  }
  return(ifelse((output*10+50) < 0, 0, output*10+50))
}

apren_fun <- function(a5, a1, edad){
  if(is.na(a5) | a5 > 15 | a5 < 0 | 
     is.na(a1) | a1 > 15 | a1 < 0 |
     is.na(edad)){
    return(NA_real_)
  }
  
  score <- a5 - a1
  output <- c()
  
  if (edad <= 29) output = (score - 6.0)/2.0
  else if (edad <= 49) output = (score - 5.3)/1.9
  else if (edad <= 65) output = (score - 5.7)/2.0
  else if (edad <= 70) output = (score - 5.6)/2.9
  else if (edad <= 75) output = (score - 5.2)/2.5
  else output = (score - 4.7)/2.7
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
}

recon_fun <- function(r, fpr, edad, sexo){
  if(is.na(r) | r > 15 | r < 0 |
     is.na(fpr) | fpr > 20 | fpr < 0 |
     is.na(edad) | is.na(sexo)){
    return(NA_real_)
  }
  
  fpr <- ifelse(r == 0 & fpr == 0, 0.1, fpr)
  r <- ifelse(r == 0, 0.1, r)
  
  score <- 0.5*(1+(r/15)-(fpr/20))
  output <- c()
  
  if(sexo == 'Masculino'){
    if( edad <= 29) output = (score - 0.90)/0.05
    else if(edad <= 39) output = (score - 0.92)/0.04
    else if(edad <= 49) output = (score - 0.92)/0.06
    else if(edad <= 59) output = (score - 0.90)/0.06
    else if(edad <= 69) output = (score - 0.82)/0.13
    else output = (score - 0.81)/0.10
  }
  
  if(sexo == 'Femenino'){
    if(edad <= 29) output = (score - 0.91)/0.09
    else if(edad <= 39) output = (score - 0.89)/0.08
    else if(edad <= 49) output = (score - 0.88)/0.07
    else if(edad <= 59) output = (score - 0.88)/0.08
    else if(edad <= 69) output = (score - 0.90)/0.06
    else output = (score - 0.84)/0.11
  }
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
}

fpr_fun <- function(fpr, edad, sexo){
  if(is.na(fpr) | fpr > 20 | fpr < 0 |
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
  
  return(ifelse((output*-10+50) < 0, 0, output*-10+50))
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
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
  
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
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
}  


## stprett i ltprrat

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
  if(edad < 50 & test == 'stpret') return(ifelse((((score-91.83)/13.01)*10+50)<0, 0, ((score-91.83)/13.01)*10+50))
  else if(edad < 50 & test == 'ltperrat') return(ifelse((((score-90.51)/15.47)*10+50)<0, 0, ((score-90.51)/15.47)*10+50))
  else{
    if(score > 100) score = 100
    score = round(score)
    grupos <- read.delim(paste0('C:\\Users\\Joan\\Desktop\\Implementation\\data\\', 'grupo_edad_stpret_ltprrat_avl.txt'))
    grupo <- groups[groups$edad==edad, 'grup']
    grid <- read.delim(paste0('C:\\Users\\Joan\\Desktop\\Implementation\\data\\', test, '.txt'))
    for(i in 1:nrow(grid)){
      if(score <= grid[i,grupo]){
        output = grid[i,grupo+1]
        break
      }
    }
    return(output)
  }
}

