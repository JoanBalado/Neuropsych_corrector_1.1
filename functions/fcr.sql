
COMPUTE RTN = 12 - RFPO .
COMPUTE RFN = 12 - RTP .
COMPUTE fcrr = RTP + RTN .


IF (edad <= 19) FCRRT = (((fcrr - 21.74) / 1.48) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad <= 19) RTPT = 50 + (((rtp - 10.13) / 1.45) * 10) .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad <= 19) RFPOT = 50 - (((rfpo - 0.39) / 0.65) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad <= 19) RTNT = (((rtn - 11.61) / 0.65) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad <= 19) RFNT = 50 - (((rfn - 1.88) / 1.45) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .



IF (edad >= 20 & edad <=24) FCRRT = (((fcrr - 21.30) / 1.35) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 20 & edad <=24) RTPT = (((rtp - 9.82) / 1.28) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 20 & edad <=24) RFPOT = 50 - (((rfpo - 0.52) / 0.85) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 20 & edad <=24) RTNT = (((rtn - 11.48) / 0.85) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 20 & edad <=24) RFNT = 50 - (((rfn - 2.18) / 1.28) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .




IF (edad >= 25 & edad <=29) FCRRT = (((fcrr - 21.53) / 1.65) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 25 & edad <=29) RTPT = (((rtp - 9.91) / 1.57) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 25 & edad <=29) RFPOT = 50 - (((rfpo - 0.38) / 0.66) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 25 & edad <=29) RTNT = (((rtn - 11.62) / 0.66) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 25 & edad <=29) RFNT = 50 - (((rfn - 2.09) / 1.57) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .




IF (edad >= 30 & edad <=34) FCRRT = (((fcrr - 21.58) / 1.27) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 30 & edad <=34) RTPT = (((rtp - 10.05) / 1.54) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 30 & edad <=34) RFPOT = 50 - (((rfpo - 0.47) / 0.89) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 30 & edad <=34) RTNT = (((rtn - 11.53) / 0.89) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 30 & edad <=34) RFNT = 50 - (((rfn - 1.95) / 1.54) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .



IF (edad >= 35 & edad <=39) FCRRT = (((fcrr - 21.44) / 1.32) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 35 & edad <=39) RTPT = (((rtp - 9.84) / 1.43) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 35 & edad <=39) RFPOT = 50 - (((rfpo - 0.40) / 0.58) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 35 & edad <=39) RTNT = (((rtn - 11.61) / 0.58) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 35 & edad <=39) RFNT = 50 - (((rfn - 2.16) / 1.43) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .



IF (edad >= 40 & edad <=44) FCRRT = (((fcrr - 20.10) / 1.76) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 40 & edad <=44) RTPT = (((rtp - 8.86) / 1.79) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 40 & edad <=44) RFPOT = 50 - (((rfpo - 0.76) / 0.93) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 40 & edad <=44) RTNT = (((rtn - 11.24) / 0.93) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 40 & edad <=44) RFNT = 50 - (((rfn - 3.14) / 1.79) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .



IF (edad >= 45 & edad <=49) FCRRT = (((fcrr - 20.86) / 2.03) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 45 & edad <=49) RTPT = (((rtp - 9.49) / 2.13) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 45 & edad <=49) RFPOT = 50 - (((rfpo - 0.63) / 0.84) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 45 & edad <=49) RTNT = (((rtn - 11.37) / 0.84) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 45 & edad <=49) RFNT = 50 - (((rfn - 2.51) / 2.13) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .




IF (edad >= 50 & edad <=54) FCRRT = (((fcrr - 20.47) / 1.69) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 50 & edad <=54) RTPT = (((rtp - 9.59) / 1.54) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 50 & edad <=54) RFPOT = 50 - (((rfpo - 1.12) / 1.39) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 50 & edad <=54) RTNT = (((rtn - 10.88) / 1.39) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 50 & edad <=54) RFNT = 50 - (((rfn - 2.41) / 1.54) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .




IF (edad >= 50 & edad <=54) FCRRT = (((fcrr - 20.47) / 1.69) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 55 & edad <=59) RTPT = (((rtp - 8.67) / 1.52) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 55 & edad <=59) RFPOT = 50 - (((rfpo - 0.63) / 0.96) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 55 & edad <=59) RTNT = (((rtn - 11.37) / 0.96) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 55 & edad <=59) RFNT = 50 - (((rfn - 3.33) / 1.52) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .




IF (edad >= 60 & edad <=64) FCRRT = (((fcrr - 20.23) / 1.54) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 60 & edad <=64) RTPT = (((rtp - 8.87) / 1.69) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 60 & edad <=64) RFPOT = 50 - (((rfpo - 0.65) / 0.95) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 60 & edad <=64) RTNT = (((rtn - 11.36) / 0.95) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 60 & edad <=64) RFNT = 50 - (((rfn - 3.13) / 1.69) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .




IF (edad >= 65 & edad <=69) FCRRT = (((fcrr - 19.42) / 1.66) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 65 & edad <=69) RTPT = (((rtp - 8.73) / 1.88) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 65 & edad <=69) RFPOT = 50 - (((rfpo - 1.30) / 1.31) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 65 & edad <=69) RTNT = (((rtn - 10.70) / 1.31) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 65 & edad <=69) RFNT = 50 - (((rfn - 3.27) / 1.88) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .





IF (edad >= 70 & edad <=74) FCRRT = (((fcrr - 20.14) / 1.49) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 70 & edad <=74) RTPT = (((rtp - 8.76) / 1.65) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 70 & edad <=74) RFPOT = 50 - (((rfpo - 0.61) / 0.91) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 70 & edad <=74) RTNT = (((rtn - 11.39) / 0.91) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 70 & edad <=74) RFNT = 50 - (((rfn - 3.25) / 1.65) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .




IF (edad >= 70 & edad <=74) FCRRT = (((fcrr - 20.14) / 1.49) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 75 & edad <=79) RTPT = (((rtp - 9.35) / 1.19) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 75 & edad <=79) RFPOT = 50 - (((rfpo - 1.30) / 1.19) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 75 & edad <=79) RTNT = (((rtn - 10.70) / 1.19) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 75 & edad <=79) RFNT = 50 - (((rfn - 2.65) / 1.19) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .





IF (edad >= 80) FCRRT = (((fcrr - 19.33) / 1.45) * 10) + 50 .
VARIABLE LABELS FCRRT 'FIGURA COMPLEJA DE REY-RECOGNITION PUNTUACION T' .

IF (edad >= 80) RTPT = (((rtp - 8.93) / 1.28) * 10) + 50 .
VARIABLE LABELS RTPT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE POSITIVES PUNTUACIÓN T' .

IF (edad >= 80) RFPOT = 50 - (((rfpo - 1.60) / 1.06) * 10) .
VARIABLE LABELS RFPOT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE POSITIVES PUNTUACION T' .

IF (edad >= 80) RTNT = (((rtn - 10.40) / 1.06) * 10) + 50 .
VARIABLE LABELS RTNT 'FIGURA COMPLEJA DE REY-RECOGNITION TRUE NEGATIVES PUNTUACION T' .

IF (edad >= 80) RFNT = 50 - (((rfn - 3.07) / 1.28) * 10) .
VARIABLE LABELS RFNT 'FIGURA COMPLEJA DE REY-RECOGNITION FALSE NEGATIVES PUNTUACION T' .