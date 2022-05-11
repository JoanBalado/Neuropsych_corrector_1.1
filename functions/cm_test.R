cm_fun <- function(score, edad, sexo){
  if(is.na(score) | score < 0 |
     is.na(sexo)){
    return(NA_real_)
  }
  
  output <- c()
  
  if(sexo == 'Masculino'){
    if(edad <=24) output = (score - 6.65)/2.25
    else if(edad <=34) output = (score - 6.67)/1.97
    else if(edad <=44) output = (score - 6.71)/2.06
    else if(edad <=54) output = (score - 6.44)/2.47
    else if(edad <=64) output = (score - 5.88)/2.58
    else if(edad <=69) output = (score - 5.37)/2.22
    else if(edad <=74) output = (score - 4.75)/2.21
    else output = (score - 4.40)/2.77
  }
  
  if(sexo == 'Femenino'){
    if(edad <=24) output = (score - 6.55)/2.05
    else if(edad <=34) output = (score - 6.81)/2.02
    else if(edad <=44) output = (score - 7.17)/1.90
    else if(edad <=54) output = (score - 6.63)/2.04
    else if(edad <=64) output = (score - 6.20)/2.31
    else if(edad <=69) output = (score - 5.04)/2.49
    else if(edad <=74) output = (score - 4.74)/2.58
    else output = (score - 4.38)/2.29
  }
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
  
}

## CM versió bona
T_fun <- function(x){
  if(is.na(x)) return(NA_real_)
  round((10/3)*x+(50/3), 2)
}

mcwmsiii_fun <- function(score, edad){
  if(is.na(score) | score < 0 | score > 40 | 
     is.na(edad) | edad < 18 | edad > 100){
    return(NA_real_)
  }
  
  output <- c()
  score <- as.integer(score)
  
  grupos_edad <- read.delim(paste0('C:\\Users\\Joan\\Desktop\\Corrector NP UD\\data\\', 'grupo_edad_mcwmsiii.txt'))
  grupo <- grupos_edad[grupos_edad$edad == edad, 'grupo']
  grid <- read.delim(paste0('C:\\Users\\Joan\\Desktop\\Corrector NP UD\\data\\', 'mcwmsiii.txt'))
  
  for(i in 1:nrow(grid)){
    if(score <= grid[i,grupo]){
      output = grid[i,grupo+1]
      break
    }
  }
  return(ifelse(T_fun(output)>=0, T_fun(output), 0))
}
  
  