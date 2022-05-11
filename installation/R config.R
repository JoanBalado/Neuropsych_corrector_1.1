#############################
#### Configuración R #######
###########################

librerias <- c('Hmisc', 
               'car', 
               'MASS',
               'nlme',
               'nortest',
               'foreign',
               'ggplot2',
               'dplyr',
               'stringr',
               'shiny',
               'shinythemes',
               'reshape2',
               'DT',
               'reactlog',
               'shinyalert',
               'shinydashboard'
)
new.packages <- librerias[!(librerias %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


##########################
##### Informes ##########
########################

librerias <- c('knitr',
               'kableExtra',
               'gridExtra',
               'repr'
)

new.packages <- librerias[!(librerias %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


