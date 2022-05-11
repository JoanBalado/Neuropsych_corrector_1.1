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