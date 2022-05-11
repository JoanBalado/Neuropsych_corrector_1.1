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
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
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
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
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
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
}

secpos_fun <- function(secpos, edad, educacion){
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
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
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
  
  return(ifelse((output*10+50) < 0, 0, output*10+50))
}

