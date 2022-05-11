IF (edad <= 29 & est <= 10 & sexo = 1) bntt = 50 + (10 * ( (bnt30 - 22.09) / 3.42)).
IF (edad <= 29 & est > 10 & sexo = 1) bntt = 50 + (10 * ( (bnt30 - 22.52) / 3.28)).
IF (edad > 29 & edad <=39 & est <= 10 & sexo = 1) bntt = 50 + (10 * ( (bnt30 - 22.39) / 2.29)).
IF (edad > 29 & edad <= 39 & est > 10 & sexo = 1) bntt = 50 + (10 * ( (bnt30 - 26.56) / 2.68)).
IF (edad > 39 & edad <=49 & est <= 10 & sexo = 1) bntt = 50 + (10 * ( (bnt30 - 22.92) / 3.68)).
IF (edad > 39 & edad <= 49 & est > 10 & sexo = 1) bntt = 50 + (10 * ( (bnt30 - 27.82) / 1.94)).
IF (edad >= 50 & est <= 10 & sexo = 1) bntt = 50 + (10 * ( (bnt30 - 24.11) / 2.14)).
IF (edad >= 50 & est > 10 & sexo = 1) bntt = 50 + (10 * ( (bnt30 - 27.17) / 2.56)).

VARIABLE LABELS bntt 'Puntuación T de la versión de 30 del Boston Naming Test' .

IF (edad <= 29 & est <= 10 & sexo = 2) bntt = 50 + (10 * ( (bnt30 - 20.17) / 2.29)).
IF (edad <= 29 & est > 10 & sexo = 2) bntt = 50 + (10 * ( (bnt30 - 21.77) / 3.95)).
IF (edad > 29 & edad <=39 & est <= 10 & sexo = 2) bntt = 50 + (10 * ( (bnt30 - 21.59) / 2.96)).
IF (edad > 29 & edad <= 39 & est > 10 & sexo = 2) bntt = 50 + (10 * ( (bnt30 - 24.57) / 2.82)).
IF (edad > 39 & edad <=49 & est <= 10 & sexo = 2) bntt = 50 + (10 * ( (bnt30 - 21.81) / 2.29)).
IF (edad > 39 & edad <= 49 & est > 10 & sexo = 2) bntt = 50 + (10 * ( (bnt30 - 25.64) / 1.57)).
IF (edad >= 50 & est <= 10 & sexo = 2) bntt = 50 + (10 * ( (bnt30 - 20.52) / 4.12)).
IF (edad >= 50 & est > 10 & sexo = 2) bntt = 50 + (10 * ( (bnt30 - 24.15) / 2.85)).





IF (edad < 50) dimagt = (((dimag - 13.964)/0.187)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est <= 5) dimagt = (((dimag - 13.776)/0.798)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est >= 6 & est <= 12) dimagt = (((dimag - 13.975)/0.145)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est > 12) dimagt = (((dimag - 14)/0.5)* 10) + 50.
IF (edad > 70) dimagt = (((dimag - 13.729)/0.639)* 10) + 50.
VAR LABELS dimagt 'Denominación imágenes puntuación T'.