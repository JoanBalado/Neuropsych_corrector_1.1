
IF (edad < 50) imitdt = (((imitd - 10)/0.5)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est <= 5) imitdt = (((imitd - 10)/0.5)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est >= 6 & est <= 12) imitdt = (((imitd - 10)/0.5)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est > 12) imitdt = (((imitd - 10)/0.5)* 10) + 50.
IF (edad > 70) imitdt = (((imitd - 9.948)/0.394)* 10) + 50.
VAR LABELS imitdt 'Imitación derecha puntuación T'.

IF (edad < 50) imitit = (((imiti - 9.991)/0.095)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est <= 5) imitit = (((imiti - 9.766)/0.666)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est >= 6 & est <= 12) imitit = (((imiti - 9.978)/0.207)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est > 12) imitit = (((imiti - 10)/0.5)* 10) + 50.
IF (edad > 70) imitit = (((imiti - 9.914)/0.657)* 10) + 50.
VAR LABELS imitit 'Imitación izquierda puntuación T'.

IF (edad < 50) imitbt = (((imitb - 7.836)/0.447)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est <= 5) imitbt = (((imitb - 7.592)/0.864)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est >= 6 & est <= 12) imitbt = (((imitb - 7.935)/0.288)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est > 12) imitbt = (((imitb - 8)/0.5)* 10) + 50.
IF (edad > 70) imitbt = (((imitb - 7.085)/1.430)* 10) + 50.
VAR LABELS imitbt 'Imitación posturas bilateral puntuación T'.

IF (edad < 50) secdt = (((secd - 7.855)/0.446)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est <= 5) secdt = (((secd - 7.279)/1.533)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est >= 6 & est <= 12) secdt = (((secd - 7.617)/0.929)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est > 12) secdt = (((secd - 7.852)/0.602)* 10) + 50.
IF (edad > 70) secdt = (((secd - 6.649)/1.727)* 10) + 50.
VAR LABELS secdt 'Secuencias posturas derecha puntuación T'.

IF (edad < 50) coordt = (((coord - 4.00)/0.5)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est <= 5) coordt = (((coord - 3.88)/0.33)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est > 5 & est <= 12) coordt = (((coord - 3.89)/0.43)* 10) + 50.
IF (edad >= 50 & edad <= 70 & est > 12) coordt = (((coord - 4.00)/0.5)* 10) + 50.
IF (edad > 70) coordt = (((coord - 3.95)/0.22)* 10) + 50.
VAR LABELS coordt 'Coordinación recírpoca barna puntuación T'.
