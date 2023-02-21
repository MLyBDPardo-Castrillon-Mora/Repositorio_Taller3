################################
##        Laura Pardo         ##
##      Juan Felipe Mora      ##
##    Fernando Castrillón     ##
##    Problem Set 2 BD&ML     ##
################################

#LIMPIAR ESPACIO DE TRABAJO
#=================================================================================

rm(list = ls())

#IMPORTAR PAQUETES
#=================================================================================
library(pacman)
p_load("tidyverse", "stargazer", "caret", "leaps", "Hmisc")

#IMPORTAR DATOS
#=================================================================================
#train_per <- read.csv("./Stores/train_personas.csv")
train_hog <- read.csv("./Stores/train_hogares.csv")
#test_per <- read.csv("./Stores/test_personas.csv")
#test_hog <- read.csv("./Stores/test_hogares.csv")

#MODIFICAR DATOS
#=================================================================================
  #Etiquetar datos
  y_train_h <- train_hog %>% select(Pobre) #Variable Respuesta
  x_train_h <- train_hog %>% select(-Ingtotug,-Ingpcug,-Dominio,
                                    -Fex_dpto,-Fex_c, -id,-Pobre)
  colnames(x_train_h)[2:7] = c("NCuartos","NCuartosDormir","ViviendaEs","CuotaAmort",
                               "ArriendoHipo","Arriendo")
  colnames(x_train_h)
  
  
  
