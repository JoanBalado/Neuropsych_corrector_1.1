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
  
  return(ifelse((output*10+50)>=0, output*10+50, 0))
}

## Cálculo mental

calc_fun <- function(score, edad, educacion){
  if(is.na(score) | score < 0 | score > 10 | 
     is.na(edad) | edad > 100 | edad < 18 |
     is.na(educacion) | educacion > 20 | educacion < 0){
    return(NA_real_)
  }
  
  output <- c()
  
  if(edad < 50) output = (score - 9.17)/1.20
  else if(edad <= 70 & est <= 5) output = (score - 5.00)/2.02
  else if(edad <= 70 & est <= 12) output = (score - 8.40)/1.48
  else if(edad <= 70 & est > 12) output = (score - 9.77)/0.60
  else output = (score - 7.94)/3.10
  
  return(ifelse((output*10+50)>=0, output*10+50, 0))
}

## Simil

simil_fun <- function(score, edad, educacion){
  if(is.na(score) | score < 0 | score > 10 | 
     is.na(edad) | edad > 100 | edad < 18 |
     is.na(educacion) | educacion > 20 | educacion < 0){
    return(NA_real_)
  }
  
  grid <- read.delim(paste0('C:\\Users\\Joan\\Desktop\\Corrector NP UD\\data\\', 'simil.txt'))
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
  
  return(ifelse(output>=0, output, 0))
}
