IF (edad >=20 & edad <=24 & sexo=1) cmt = (((cm - 6.65)/2.25)*10)+50.
IF (edad >=25 & edad <=34 & sexo=1) cmt = (((cm - 6.67)/1.97)*10)+50.
IF (edad >=35 & edad <=44 & sexo=1) cmt = (((cm - 6.71)/2.06)*10)+50.
IF (edad >=45 & edad <=54 & sexo=1) cmt = (((cm - 6.44)/2.47)*10)+50.
IF (edad >=55 & edad <=64 & sexo=1) cmt = (((cm - 5.88)/2.58)*10)+50.
IF (edad >=65 & edad <=69 & sexo=1) cmt = (((cm - 5.37)/2.22)*10)+50.
IF (edad >=70 & edad <=74 & sexo=1) cmt = (((cm - 4.75)/2.21)*10)+50.
IF (edad >=75 & sexo=1) cmt = (((cm - 4.40)/2.77)*10)+50.

IF (edad >=20 & edad <=24 & sexo=2) cmt = (((cm - 6.55)/2.05)*10)+50.
IF (edad >=25 & edad <=34 & sexo=2) cmt = (((cm - 6.81)/2.02)*10)+50.
IF (edad >=35 & edad <=44 & sexo=2) cmt = (((cm - 7.17)/1.90)*10)+50.
IF (edad >=45 & edad <=54 & sexo=2) cmt = (((cm - 6.63)/2.04)*10)+50.
IF (edad >=55 & edad <=64 & sexo=2) cmt = (((cm - 6.20)/2.31)*10)+50.
IF (edad >=65 & edad <=69 & sexo=2) cmt = (((cm - 5.04)/2.49)*10)+50.
IF (edad >=70 & edad <=74 & sexo=2) cmt = (((cm - 4.74)/2.58)*10)+50.
IF (edad >=75 & sexo=2) cmt = (((cm - 4.38)/2.29)*10)+50.
VAR LABELS cmt 'Control mental puntuación T'.
