
# LIBRERIAS ====================================================================
library(dplyr)
library(e1071)
library(MASS)

# DATOS ========================================================================

# Base de datos ----------------------------------------------------------------

# Importacion de datos
train_hogares <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/train_hogares.csv")
test_hogares <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/test_hogares.csv")
train_personas <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/train_personas.csv")
test_personas <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/test_personas.csv")

# Union de las bases de datos
train_merge <- merge(train_personas,train_hogares,by='id')
test_merge <- merge(test_personas,test_hogares,by='id')

# Seleccion de datos -----------------------------------------------------------


# Var Discretas
train_personas$Clase
train_personas$sexo
train_personas$P6020
train_personas$Estrato1
train_personas$P6040
train_personas$P6050
train_personas$P6090
train_personas$P6210
train_personas$P6100
train_personas$P6585s1
train_personas$P6585s3
train_personas$P6590
train_personas$P6585s4
train_personas$P6610
train_personas$P6620
train_personas$P6870
train_personas$P6920
train_personas$P7040
train_personas$P7090
train_personas$P7050
train_personas$P7495
train_personas$Pet
train_personas$Des
train_personas$Oc
train_personas$Ina

# Var Continaus
train_personas$P5130
train_personas$Nper
train_personas$P6040
train_personas$P6210s1
train_personas$P6500
train_personas$P6510s1
train_personas$P6580s1
train_personas$P6800
train_personas$P7422s1
train_personas$P7500s1a1
train_personas$Ingtotob
train_personas$Ingtot

# Base de datos - aux_RC
train_RC <- train_merge %>% dplyr::select(
  Estrato1, Pobre, P5000, P5010, Nper, P6020, P6040, P6090, P6210s1,P6585s1, 
  P6585s3, P6590, P6620, P6800, P6920, P7040, P7510s3, P6050)

test_RC <- test_merge %>% dplyr::select(
  P5000, P5010, Nper, P6020, P6040, P6090, P6210s1,P6585s1, 
  P6585s3, P6590, P6620, P6800, P6920, P7040, P7510s3, P6050)

# Limpieza de datos ------------------------------------------------------------

# Contar NAs en la base de datos
colSums(is.na(train_RC)) %>% as.data.frame()

# Eliminar columnas con DEMASIADOS NAs
train_RC <- train_RC %>% dplyr::select(-P6590, -P6620, -P6800, -P7040, -P7510s3)

# Arreglamos P6585s1 y P6585s3 con su media si el estrato es 'bajo'...
train_RC$P6585s1[is.na(train_RC$P6585s1) == "TRUE" & train_RC$Estrato1>1] <- 0
train_RC$P6585s3[is.na(train_RC$P6585s3) == "TRUE" & train_RC$Estrato1>1] <- 0

length(which(train_RC$P6585s1 == "0"))
length(which(train_RC$P6585s3 == "0"))

# Crear lista auxiliar de NAs
list_na_train <- colnames(train_RC)[apply(train_RC, 2, anyNA)]

# Media por Variable
average_missing_train <- apply(train_RC[,colnames(train_RC) %in% list_na_train],
                         2,
                         mean,
                         na.rm =  TRUE)
average_missing_train

# Reemplazamos medias en los NAs - Creamos nueva base de datos
train_prueba <- train_RC %>%
  mutate(P6090_n  = ifelse(is.na(P6090), average_missing_train[1], P6090),
         P6210s1_n  = ifelse(is.na(P6210s1), average_missing_train[2], P6210s1),
         P6585s1_n  = ifelse(is.na(P6585s1), average_missing_train[3], P6585s1),
         P6585s3_n  = ifelse(is.na(P6585s3), average_missing_train[4], P6585s3),
         P6920_n = ifelse(is.na(P6920), average_missing_train[5], P6920))

# Checkear NAs en variables con medias
colSums(is.na(train_prueba)) %>% as.data.frame()

#MODELOS =======================================================================

# LOGIT ------------------------------------------------------------------------

# Modelo
glm.fits <- glm(Pobre ~ P5000 + P5010 + Nper + P6020 + P6040 + P6090_n + P6210s1_n + P6585s1_n + P6585s3_n + P6920_n + P6050,
                data = train_prueba, family = "binomial")
summary(glm.fits)

# Train check
glm.probs <- predict(glm.fits, type = "response")
glm.pred <- rep("No-pobre", length(fitted(glm.fits)))
glm.pred[glm.probs > 0.4] = "Pobre"
table(glm.pred, glm.fits$y)

# LDA --------------------------------------------------------------------------

#Modelo de prueba

lda.fit <- lda(
  Pobre ~ P5000 + Nper + P6210s1 + Ingtot + P6050,
  data = train_merge)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, train_merge)
lda.class <- lda.pred$class
table(lda.class, test_merge$Pobre)
(338454+16400)/(338454+16400+83371+9434)

# Modelo 1
lda.fit <- lda(
  Pobre ~ P5000 + P5010 + Nper + P6020 + P6040 + P6090_n + P6210s1_n + P6585s1_n + P6585s3_n + P6920_n + P6050,
  data = train_prueba)
lda.fit
lda.pred <- predict(lda.fit, train_prueba)
lda.class <- lda.pred$class
table(lda.class, train_prueba$Pobre)
(23803+602)/(23803+602+1106+780)

# Modelo 2
lda.fit <- lda(
  Pobre ~ P5000 + Nper + P6210s1_n + P6050, data = train_prueba)
lda.fit
lda.pred <- predict(lda.fit, train_prueba)
lda.class <- lda.pred$class
table(lda.class, train_prueba$Pobre)
(23803+602)/(23803+602+1106+780)

# QDA

qda.fit <- qda(
  Pobre ~ P5000 + Nper + P6210s1_n + P6050,
  data = train_merge)
qda.fit
qda.pred <- predict(qda.fit, train_merge)
qda.class <- qda.pred$class
table(qda.class, train_merge$Pobre)
(266551+68670)/(266551+68670+31101+81337)

# Naive Bayes

nb.fit <- naiveBayes(
  Pobre ~ P5000 + Nper + P6210s1 + Ingtot + P6050,
  data = train_merge)
nb.class <- predict(nb.fit, train_merge)
table(nb.class, train_merge$Pobre)
(294005+81355)/(294005+81355+55166+112583)







