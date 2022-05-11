## t 15 objetos

## fsl
fsl_fun <- function(score){
  if(is.na(score) | score > 14 | score < 0){
    return(NA_real_)
  }
  
  output = (score-12.82)/0.97
  
  return(ifelse((output*10+50) < 0, 0, round_fun(output*10+50)))
}

## hooper 
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
  
  grid <- read.delim(paste0('C:/Users/Joan/Desktop/Corrector NP UD/', directory, 'jlo', '.txt'))
  
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
  
  grid <- read.delim(paste0('C:/Users/Joan/Desktop/Corrector NP UD/', directory, 'clockr', '.txt'))
  grupos <- read.delim(paste0('C:/Users/Joan/Desktop/Corrector NP UD/', directory, 'grupo_edad_clockr', '.txt'))
  grupo <- grupos[grupos$edad == edad, 'grupo']
  
  output = grid[grid[,grupo] == score,grupo+1]
  
  return(100-output)
}