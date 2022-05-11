# Comprensión de órdenes

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

# FAS
fas_fun <- function(score, escolaridad){
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
  
  grid <- read.delim('C:/Users/Joan/Desktop/Corrector NP UD/data/fas.txt')
  
  output = grid[grid$PD == score, 'PT']
  
  return(output)
}