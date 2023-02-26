
# LIBRERIAS ====================================================================
library(dplyr)
library(e1071)
library(MASS)
library(pacman)
library(caret)
library(tidyverse)
library(ISLR2)
library(leaps)
library(fastDummies)
library (DescTools)

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
  Ingtotug, Lp, Pobre, Clase.x, Depto.x, P6020, P6040, P6050,
  P6090, P6100, P6210, P6240, P6430, P6920, P7040, P7495, P7505, 
  P5000, P5010, P5090, P5130, Nper, Npersug)

test_RC <- test_merge %>% dplyr::select(
  Clase.x, Depto.x, P6020, P6040, P6050,
  P6090, P6100, P6210, P6240, P6430, P6920, P7040, P7495, P7505, 
  P5000, P5010, P5090, P5130, Nper, Npersug)

# Factor Variables
cols_f <- c("Clase.x", "Depto.x", "P6090", "P6100", "P6020", "P6210", "P6240", "P6430", "P6920", "P7040", "P7495", "P7505", "P5090")
train_RC[,cols_f] <- data.frame(apply(train_RC[cols_f], 2, as.factor))
test_RC[,cols_f] <- data.frame(apply(test_RC[cols_f], 2, as.factor))

# Limpieza de datos ------------------------------------------------------------

# 1. Datos de entrenamiento ____________________________________________________

# Contar NAs en la base de datos
colSums(is.na(train_RC)) %>% as.data.frame()

# Crear lista auxiliar de NAs - Dummys
train_list_d <- c("P6090", "P6100", "P6020", "P6210", "P6240", "P6430" , "P6920", "P7040", "P7495", "P7505", "P5090")

# Media por Variable
for (i in 1:ncol(train_RC)){
  mod_val <- Mode(na.omit(train_RC[, i]))
  cat(i, ":", mod_val, "\n")
}
# Mediana por Variable
median(na.omit(train_RC$P5130))

# Vector de reemplazo

mode_d <- c(1, 1, 2, 3, 1, 4, 2, 2, 2, 2, 1, median(na.omit(train_RC$P5130)))

# Reemplazamos modas en los NAs - Creamos nueva base de datos
train_db <- train_RC %>%
  dplyr::mutate(P6090 = ifelse(is.na(P6090), mode_d[1], P6090),
                P6100 = ifelse(is.na(P6100), mode_d[2], P6100),
                P6020 = ifelse(is.na(P6020), mode_d[3], P6020),
                P6210 = ifelse(is.na(P6210), mode_d[4], P6210),
                P6240 = ifelse(is.na(P6240), mode_d[5], P6240),
                P6430 = ifelse(is.na(P6430), mode_d[6], P6430),
                P6920 = ifelse(is.na(P6920), mode_d[7], P6920),
                P7040 = ifelse(is.na(P7040), mode_d[8], P7040),
                P7495 = ifelse(is.na(P7495), mode_d[9], P7495),
                P7505 = ifelse(is.na(P7505), mode_d[10], P7505),
                P5090 = ifelse(is.na(P5090), mode_d[11], P5090),
                P5130 = ifelse(is.na(P5130), mode_d[12], P5130))

# Checkear NAs en variables con medias
colSums(is.na(train_db)) %>% as.data.frame()

# Crear Dummys
train_db <- fastDummies::dummy_cols(train_db)

# Clasificador por hogares
home_class <- train_db[train_db$P6050=="1",]
colSums(is.na(home_class)) %>% as.data.frame()

# 2. Datos de prueba ___________________________________________________________

# Contar NAs en la base de datos
colSums(is.na(test_RC)) %>% as.data.frame()

# Crear lista auxiliar de NAs - Dummys
test_list_d <- c("P6090", "P6100", "P6020", "P6210", "P6240", "P6430" , "P6920", "P7040", "P7495", "P7505", "P5090")

# Media por Variable
for (i in 1:ncol(test_RC)){
  mod_val <- Mode(na.omit(test_RC[, i]))
  cat(i, ":", mod_val, "\n")
}
# Mediana por Variable
median(na.omit(test_RC$P5130))

# Vector de reemplazo

mode_d <- c(1, 3, 2, 3, 1, 4, 2, 2, 2, 2, 1, median(na.omit(test_RC$P5130)))

# Reemplazamos modas en los NAs - Creamos nueva base de datos
test_db <- test_RC %>%
  dplyr::mutate(P6090 = ifelse(is.na(P6090), mode_d[1], P6090),
                P6100 = ifelse(is.na(P6100), mode_d[2], P6100),
                P6020 = ifelse(is.na(P6020), mode_d[3], P6020),
                P6210 = ifelse(is.na(P6210), mode_d[4], P6210),
                P6240 = ifelse(is.na(P6240), mode_d[5], P6240),
                P6430 = ifelse(is.na(P6430), mode_d[6], P6430),
                P6920 = ifelse(is.na(P6920), mode_d[7], P6920),
                P7040 = ifelse(is.na(P7040), mode_d[8], P7040),
                P7495 = ifelse(is.na(P7495), mode_d[9], P7495),
                P7505 = ifelse(is.na(P7505), mode_d[10], P7505),
                P5090 = ifelse(is.na(P5090), mode_d[11], P5090),
                P5130 = ifelse(is.na(P5130), mode_d[12], P5130))

# Checkear NAs en variables con medias
colSums(is.na(test_db)) %>% as.data.frame()

# Crear Dummys
test_db <- fastDummies::dummy_cols(test_db)

# Clasificador por hogares
home_class_test <- test_db[test_db$P6050=="1",]

#MODELOS =======================================================================

# LOGIT ------------------------------------------------------------------------

# Modelo
glm.fits <- glm(Pobre ~ Clase.x + P6020_2 + P6040 + I(P6040^2) + P6090_2 + P6100_2 
                + P6100_3 + P6210_2 + P6210_3 + P6210_4 + P6210_5 + P6240_2 + P6240_3
                + P6240_4 + P6240_5 + P6240_6 + P6430_1 + P6430_2 + P6430_3 + P6430_4 
                + P6430_5 + P6430_7 + P6430_8 + P6920_2 + P7040_1 + P7495_2 + P7505_1 
                + P5000 + P5010 + Nper + Npersug + P5090_2 + P5090_3 + P5090_4 
                + P5090_5 + P5090_6 + P5130,
                data = home_class, family = "binomial")

summary(glm.fits)

# Train check
glm.probs <- predict(glm.fits, type = "response", newdata = home_class)
glm.pred <- rep("No-pobre", length(fitted(glm.fits)))
glm.pred[glm.probs > 0.5] = "Pobre"
table(glm.pred, glm.fits$y)

# Test check
glm.probs <- predict(glm.fits, type = "response", newdata = home_class_test)
glm.pred <- rep(0, 66168)
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, glm.fits$y)
export <- cbind.data.frame(test_hogares$id, glm.pred)
colnames(export)[1] <- "id"
colnames(export)[2] <- "pobre"
write.csv(export, "/Users/mora/downloads/testing_kaggle.csv", row.names=FALSE)

# CARET-Logit

home_class$Pobre = as.factor(home_class$Pobre)

levels(home_class$Pobre) <- c("No.pobre", "Pobre")

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

mylogit_caret <- train(Pobre ~ Clase.x + P6020_2 + P6040 + I(P6040^2) + P6090_2 + P6210_1
                       + P6210_2 + P6210_3 + P6210_4+ P6210_5 + P6240_2+ P6240_3+ P6240_4
                       + P6240_5 + P6240_6 + P7495_2 + P7505_1 + P5000 + P5010 + Nper 
                       + Npersug + P5090_2 + P5090_3 + P5090_4 + P5090_5 + P5090_6, 
                       data = home_class, 
                       method = "glm",
                       trControl = ctrl,
                       family = "binomial", 
                       metric = 'ROC')
mylogit_caret

# Upsample
logit_us <- upSample(x = home_class, y = home_class$Pobre, yname = "Pobre")
table(logit_us$Pobre)

logit_us_caret <- train(Pobre ~ Clase.x + P6020_2 + P6040 + I(P6040^2) + P6090_2 + P6210_1
                        + P6210_2 + P6210_3 + P6210_4+ P6210_5 + P6240_2+ P6240_3+ P6240_4
                        + P6240_5+ P6240_6 + P7495_2 + P7505_1 + P5000 + P5010 + Nper 
                        + Npersug + P5090_2 + P5090_3 + P5090_4 + P5090_5 + P5090_6, 
                       data = logit_us, 
                       method = "glm",
                       trControl = ctrl,
                       family = "binomial", 
                       metric = 'Acc')
logit_us_caret


# LDA --------------------------------------------------------------------------

#Modelo de prueba

lda.fit <- lda(
  Pobre ~ Clase.x + P6020_2 + P6040 + I(P6040^2) + P6090_2 + P6210_1
  + P6210_2 + P6210_3 + P6210_4+ P6210_5 + P6240_2+ P6240_3+ P6240_4
  + P6240_5+ P6240_6 + P6430_1 + P6430_2 + P6430_3 + P6430_4 + P6430_5 
  + P6430_7+ P6430_8 + P7495_2 + P7505_1 + P5000 + P5010 + Nper 
  + Npersug + P5090_2 + P5090_3 + P5090_4 + P5090_5 + P5090_6 + P5130,
  data = home_class)
lda.fit
lda.pred <- predict(lda.fit, home_class)
lda.class <- lda.pred$class
table(lda.class, home_class$Pobre)
(338454+16400)/(338454+16400+83371+9434)

# Modelo 1
lda.fit <- lda(
  Pobre ~ P5000 + I(P5000^2)+ P5010 + I(P5010^2) + Nper + I(Nper^2) + 
    P6020 + P6040 + I(P6040^2) + P6090_n + P6210_n + P6210s1_n
    + P6585s1_n + P6585s3_n + P6920_n + P7495_n,
  data = home_class)
lda.fit
lda.pred <- predict(lda.fit, home_class)
lda.class <- lda.pred$class
table(lda.class, home_class$Pobre)
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

# PREDICCION DE INGRESO ========================================================

reg1 <- lm(Ingtot ~ Clase.x + P6020_2 + P6040 + I(P6040^2) + P6090_2 + P6100_2 
           + P6100_3 + P6210_2 + P6210_3 + P6210_4 + P6210_5 + P6240_2 + P6240_3
           + P6240_4 + P6240_5 + P6240_6 + P6430_1 + P6430_2 + P6430_3 + P6430_4 
           + P6430_5 + P6430_7 + P6430_8 + P6920_2 + P7040_1 + P7495_2 + P7505_1 
           + P5000 + P5010 + Nper + Npersug + P5090_2 + P5090_3 + P5090_4 
           + P5090_5 + P5090_6 + P5130,
           data = home_class)
summary(reg1)

reg.pred <- predict(reg1, home_class)
poor.pred <- rep("No-pobre", 164960)
compare.df <- cbind.data.frame(home_class$Pobre , home_class$Lp, reg.pred)
compare.df$poor.pred <- ifelse(home_class$Lp>reg.pred, 1, 0)
table(compare.df$poor.pred, home_class$Pobre)



