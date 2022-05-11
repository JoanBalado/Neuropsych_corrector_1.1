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
    return(ifelse(50 - output*10 < 0, 0, 50 - output*10))
  } else return(ifelse(output*10+50 < 0, 0, output*10+50))
}
