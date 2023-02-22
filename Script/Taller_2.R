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
test_hog <- read.csv("./Stores/test_hogares.csv")
colnames(train_hog)
#MODIFICAR DATOS
#=================================================================================
  #Etiquetar datos de entrenamiento

  colnames(train_hog)[4:9] = c("NCuartos","NCuartosDormir","ViviendaEs","CuotaAmort",
                               "ArriendoHipo","Arriendo")
  
  train_hog<-train_hog %>% mutate(Pobre=factor(Pobre,levels=c(1,0),labels=c("Sí","No"))) 
  
  y_train <- train_hog$Pobre
  y_train <- as.factor(y_train)
  x_train <- train_hog %>% select(-Pobre,-Fex_dpto,-Fex_c,-id)
  x_train <- as.matrix(x_train)
#MODELO
#==========================================================================================
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
  
ctrl<- trainControl(method = "cv",
                    number = 10,
                    summaryFunction = fiveStats, 
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = "final")

modelo <- train(y = y_train,
                x = x_train,
                method = "glmStepAIC",
                trControl = ctrl,
                maxvar = 10
                )
