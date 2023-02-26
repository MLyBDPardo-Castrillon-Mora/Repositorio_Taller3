################################
##        Laura Pardo         ##
##      Juan Felipe Mora      ##
##    Fernando Castrillón     ##
##    Problem Set 2 BD&ML     ##
################################


#LIMPIAR ESPACIO DE TRABAJO======================================================

rm(list = ls())


#IMPORTAR PAQUETES===============================================================
library(pacman)
p_load("tidyverse", "stargazer", "caret")

#IMPORTAR DATOS=================================================================================
train_per <- read.csv("./Stores/train_personas.csv")
train_hog <- read.csv("./Stores/train_hogares.csv")
test_per <- read.csv("./Stores/test_personas.csv")
test_hog <- read.csv("./Stores/test_hogares.csv")

#MODIFICAR DATOS==================================================================
#P6020 sexo
#P6040 Edad 
#P6090 Afiliado o no afiliado
#P6210s1 Escolaridad | Continua
#P6585s1 Auxilio | de alimentación Si o No
#P6585s3 Subsidio | Familiar Si o No
#P6920 Cotiza pensiones | Si o No o Pensionado
#P6050 Parentesco con jefe de Hogar | Categorica

# Seleccionar variables relevantes
train_per <- train_per %>% select(id,
  Estrato1, P6020, P6040, P6090, P6210s1,P6585s1, 
  P6585s3, P6920, P6050)

test_per <- test_per %>% select(id,
  P6020, P6040, P6090, P6210s1,P6585s1, 
  P6585s3, P6590, P6620, P6800, P6920, P7040, P7510s3, P6050)

# Arreglamos P6585s1 y P6585s3 con su media si el estrato es 'bajo'...
train_per$P6585s1[is.na(train_per$P6585s1) == "TRUE" & train_per$Estrato1>1] <- 0
train_per$P6585s3[is.na(train_per$P6585s3) == "TRUE" & train_per$Estrato1>1] <- 0

#Confirmamos estas variables
length(which(train_per$P6585s1 == "0"))
length(which(train_per$P6585s3 == "0"))

# Crear lista auxiliar de NAs
list_na_train <- colnames(train_per)[apply(train_per, 2, anyNA)]

# Media por Variable  
average_missing_train <- apply(train_per[,colnames(train_per) %in% list_na_train],
                               2,
                               mean,
                               na.rm =  TRUE)

average_missing_train
colSums(is.na(train_per)) %>% as.data.frame() #Confirmamos, no más NA
train_per <- train_per %>%
  mutate(P6090  = ifelse(is.na(P6090), average_missing_train[1], P6090),
         P6210s1  = ifelse(is.na(P6210s1), average_missing_train[2], P6210s1),
         P6585s1  = ifelse(is.na(P6585s1), average_missing_train[3], P6585s1),
         P6585s3  = ifelse(is.na(P6585s3), average_missing_train[4], P6585s3))
rm(average_missing_train, list_na_train)

# Filtramos Variables por datos con sentido
train_per <- filter(train_per, P6585s1 == 1 | P6585s1 == 2)
train_per <- filter(train_per, P6585s3 == 1 | P6585s3 == 2)
train_per <- filter(train_per, P6210s1 != 99)

# Etiquetas las variables con factor
#train_per<-train_per %>% mutate(P6090=factor(Pobre,levels=c(1,2),labels=c("Sí","No")),
 #                               P6585s1=factor(Pobre,levels=c(1,2),labels=c("Sí","No")),
  #                              P6585s3=factor(Pobre,levels=c(1,2),labels=c("Sí","No")),
   #                             P6920P=factor(Pobre,levels=c(1,2),labels=c("Sí","No")),
    #                            P6050=factor(Pobre,levels=c(1,2,3,4,5,6,7,8,9),
     #                                               labels=c("Jefx","Pareja","Hijx","Nietx","Otro","Empleadx","Pensionista","Trabajador(a)","Otros"))) 
#Etiquetamos variables de respuesta
#train_hog<-train_hog %>% mutate(Pobrefactor=factor(Pobre,levels=c(1,0),labels=c("Sí","No")))

#UNIÓN DE BASES DE DATOS==============================================================

# Agrupamiento por promedios
train_PerProm <- train_per %>%
            group_by(id) %>%
            dplyr::summarize(P6020 = mean(P6020),
                      P6040 = mean(P6040),
                      P6090 = mean(P6090),
                      P6210s1 = mean(P6210s1),
                      P6585s1 = mean(P6585s1),
                      P6585s3 = mean(P6585s3))

train_PerProm <- train_PerProm %>%
              mutate(P6020 = ifelse(P6020>=1.5,0,1),
                     P6090 = if_else(P6090>=1.5,0,1),
                     P6585s1 = if_else(P6585s1>=1.5,0,1),
                     P6585s3 = if_else(P6585s3>=1.5,0,1))

# Tomando Jefe de Hogar como Representante de hogar
train_PerJefe <- train_per %>% filter(train_per$P6050==1)

#UNIÓN
train_prom <- train_hog %>% inner_join(train_PerProm, by = c("id"))
train_Jefe <- train_hog %>% inner_join(train_PerJefe, by = c("id"))


#BALANCEO DE DATOS========================================================================
set.seed(1000)
train_prom <- train_prom %>%
  mutate(pobre_l = train_prom$Pobre == 1)

dumificador <- dummyVars(formula = ~ .+ I(as.numeric(Nper)^2), 
                         data = train_prom, fullRank = T)

train_prom_db <- predict(dumificador, newdata=train_prom)
train_prom_db <- as.data.frame(train_prom_db)

#glimpse(train_prom_db) #despliegue

#prueba <-lm(formula=pobre_lTRUE ~ ., data=data.frame(train_prom_db)) #modelo lineal

#Guardamos las variables con coeficentes omitidos
#multicolinealidad <- names(prueba$coefficients[is.na(prueba$coefficients)])

#train_prom_db <- train_prom_db %>% 
 # data.frame() %>%
  #select(-all_of(multicolinealidad))


#Rescalamos variables numéricas
####variables_numericas <- c("VECTOR DE VARIABLES NUMERICAS")
####escalador <- preProcess(train_prom[, variables_numericas],method = c("center", "scale"))
####train_prom[, variables_numericas] <- predict(escalador, train_prom[, variables_numericas])
#^^^^^^^^^^^^^^^^^^^^^^^^ NO OLVIDAR ESTO ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

train_prom2 <- train_prom #Copia base

train_prom <- as.tibble(train_prom) #Paso a tibble
train_prom2 <- as.tibble(train_prom2) #Paso a tibble

train_prom2 <- train_prom2 %>% mutate(pobre_l = factor(pobre_l))

x <- train_prom2 %>% select(pobre_l)

train_prom2_rsam <- upSample(x = select(train_prom2, -Pobre, -pobre_l), 
                           y = train_prom2$pobre_l, list = F, yname = "PobreTRUE")

train_prom2_rsam <- train_prom2_rsam %>% mutate(PobreTrue = factor(PobreTRUE,levels=c(T,F),labels=c("Sí","No")))
x_train <- train_prom2_rsam %>% select(-PobreTRUE,-PobreTrue)
y_train <- train_prom2_rsam %>% select(PobreTrue)
#MODELO==================================================================================
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats, 
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = "final")

modelo <- train(PobreTrue ~ Clase + P5000 + P5010+P5090+Nper+Npersug+Ingtotug+Li+Lp+Indigente,
                data = train_prom2_rsam,
                method = "glmStepAIC",
                trControl = ctrl,
                maxvar = 5
                )
