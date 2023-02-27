# LIBRERIAS ====================================================================
library(caret)
library(DescTools)
library(dplyr)
library(e1071)
library(fastDummies)
library(ISLR2)
library(leaps)
library(MASS)
library(pacman)
library(randomForest)
library(rpart)
library(smotefamily)
library(tidyverse)


# DATOS ========================================================================
# Base de datos ----------------------------------------------------------------

# Semilla
set.seed(4040)

# Limite de memoria
memory.limit(100000)

# Importacion de datos
db.h <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/train_hogares.csv")
ka.h <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/test_hogares.csv")
db.p <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/train_personas.csv")
ka.p <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/test_personas.csv")

# Union de las bases de datos
db <- merge(db.p, db.h, by='id')
kaggle <- merge(ka.p, ka.h, by='id')

# Split de datos

db$obs <- 1:nrow(db)
indices <- createDataPartition(y = db$obs, p = 0.8, list = FALSE)
train <- db[indices, ]
dev_set <- db[-indices, ]

indices_1 <- createDataPartition(y = dev_set$obs, p = 0.5, list = FALSE)
test <- dev_set[indices_1, ]
dev_set <- dev_set[-indices_1, ]

# Seleccion de datos -----------------------------------------------------------

# Seleccion de variables

train <- dplyr::select(train, id, P5000, P5010, Nper, P5130, P5140, Npersug, Ingtotug, 
                Lp, Pobre, Clase.x, P5090, Estrato1, P6020, P6040, P6090, P7495,
                P7510s1a1, P6050, P6100, P6210, P6210s1)

dev_set <- dplyr::select(dev_set, id, P5000, P5010, Nper, P5130, P5140, Npersug, Ingtotug, 
                       Lp, Pobre, Clase.x, P5090, Estrato1, P6020, P6040, P6090, P7495,
                       P7510s1a1, P6050, P6100, P6210, P6210s1)

test <- dplyr::select(test, id, P5000, P5010, Nper, P5130, P5140, Npersug, Ingtotug, 
                       Lp, Pobre, Clase.x, P5090, Estrato1, P6020, P6040, P6090, P7495,
                       P7510s1a1, P6050, P6100, P6210, P6210s1)

# Nombres legibles...
colnames(train) <- c("id", "rooms", "bedrooms", "nper", "ayuda_1", "ayuda_2", 
                     "npersug", "ing", "lp", "poor", "clase", "prop", "estrato",
                     "sex", "age", "health", "rent_rec", "help", "parent", 
                     "health_aff", "educ", "grade")

colnames(test) <- c("id", "rooms", "bedrooms", "nper", "ayuda_1", "ayuda_2", 
                    "npersug", "ing", "lp", "poor", "clase", "prop", "estrato",
                    "sex", "age", "health", "rent_rec", "help", "parent", 
                    "health_aff", "educ", "grade")

colnames(dev_set) <- c("id", "rooms", "bedrooms", "nper", "ayuda_1", "ayuda_2", 
                       "npersug", "ing", "lp", "poor", "clase", "prop", "estrato",
                       "sex", "age", "health", "rent_rec", "help", "parent", 
                       "health_aff", "educ", "grade")

# Transformaciones -------------------------------------------------------------

# Crear variable "rent_est"
train$ayuda_1 <- ifelse(is.na(train$ayuda_1), 0, train$ayuda_1)
train$ayuda_2 <- ifelse(is.na(train$ayuda_2), 0, train$ayuda_2)
train$rent_est <- train$ayuda_1 + train$ayuda_2
train <- dplyr::select(train, -ayuda_1, -ayuda_2)

# Repetir para test
test$ayuda_1 <- ifelse(is.na(test$ayuda_1), 0, test$ayuda_1)
test$ayuda_2 <- ifelse(is.na(test$ayuda_2), 0, test$ayuda_2)
test$rent_est <- test$ayuda_1 + test$ayuda_2
test <- dplyr::select(test, -ayuda_1, -ayuda_2)

# Repetir para dev_set
dev_set$ayuda_1 <- ifelse(is.na(dev_set$ayuda_1), 0, dev_set$ayuda_1)
dev_set$ayuda_2 <- ifelse(is.na(dev_set$ayuda_2), 0, dev_set$ayuda_2)
dev_set$rent_est <- dev_set$ayuda_1 + dev_set$ayuda_2
dev_set <- dplyr::select(dev_set, -ayuda_1, -ayuda_2)

# Resolver NAs -----------------------------------------------------------------

# TRAIN

# Contar NAs
colSums(is.na(train)) %>% as.data.frame()

# Imputacion por Arboles de Decision

# I. Health
aux <- subset(train, select = c("health", "estrato", "age", "nper", "ing"))
aux$health <- ifelse(aux$health == "9", NA, aux$health)
tree_model <- rpart(health ~ ., data = aux, method = "class")
aux$health[is.na(aux$health)] <- predict(tree_model, newdata = aux[is.na(aux$health), ], type = "class")
aux$health <- ifelse(aux$health == "1", 0, 1)
train$health <- aux$health

# II. Help
aux <- subset(train, select = c("help", "estrato", "age", "nper", "ing"))
tree_model <- rpart(help ~ ., data = aux)
aux$help[is.na(aux$help)] <- predict(tree_model, newdata = aux[is.na(aux$help), ])
train$help <- aux$help

# III. Educ
aux <- subset(train, select = c("educ", "estrato", "age", "nper", "ing"))
tree_model <- rpart(educ ~ ., data = aux, method = "class")
aux$educ[is.na(aux$educ)] <- predict(tree_model, newdata = aux[is.na(aux$educ), ], type = "class")
train$educ <- aux$educ

# IV. Grade
aux <- subset(train, select = c("grade", "estrato", "age", "nper", "ing"))
tree_model <- rpart(grade ~ ., data = aux)
aux$grade[is.na(aux$grade)] <- predict(tree_model, newdata = aux[is.na(aux$grade), ])
train$grade <- aux$grade

# V. Health_aff
aux <- subset(train, select = c("health_aff", "estrato", "age", "nper", "ing"))
tree_model <- rpart(health_aff ~ ., data = aux, method = "class")
aux$health_aff[is.na(aux$health_aff)] <- predict(tree_model, newdata = aux[is.na(aux$health_aff), ], type = "class")
train$health_aff <- aux$health_aff

# Check - Conteo de NAs
colSums(is.na(train)) %>% as.data.frame()

# TEST

# Contar NAs
colSums(is.na(test)) %>% as.data.frame()

# Imputacion por Arboles de Decision

# I. Health
aux <- subset(test, select = c("health", "estrato", "age", "nper", "ing"))
aux$health <- ifelse(aux$health == "9", NA, aux$health)
tree_model <- rpart(health ~ ., data = aux, method = "class")
aux$health[is.na(aux$health)] <- predict(tree_model, newdata = aux[is.na(aux$health), ], type = "class")
aux$health <- ifelse(aux$health == "1", 0, 1)
test$health <- aux$health

# II. Help
aux <- subset(test, select = c("help", "estrato", "age", "nper", "ing"))
tree_model <- rpart(help ~ ., data = aux)
aux$help[is.na(aux$help)] <- predict(tree_model, newdata = aux[is.na(aux$help), ])
test$help <- aux$help

# III. Educ
aux <- subset(test, select = c("educ", "estrato", "age", "nper", "ing"))
tree_model <- rpart(educ ~ ., data = aux, method = "class")
aux$educ[is.na(aux$educ)] <- predict(tree_model, newdata = aux[is.na(aux$educ), ], type = "class")
test$educ <- aux$educ

# IV. Grade
aux <- subset(test, select = c("grade", "estrato", "age", "nper", "ing"))
tree_model <- rpart(grade ~ ., data = aux)
aux$grade[is.na(aux$grade)] <- predict(tree_model, newdata = aux[is.na(aux$grade), ])
test$grade <- aux$grade

# V. Health_aff
aux <- subset(test, select = c("health_aff", "estrato", "age", "nper", "ing"))
tree_model <- rpart(health_aff ~ ., data = aux, method = "class")
aux$health_aff[is.na(aux$health_aff)] <- predict(tree_model, newdata = aux[is.na(aux$health_aff), ], type = "class")
test$health_aff <- aux$health_aff

# Check - Conteo de NAs
colSums(is.na(test)) %>% as.data.frame()

# DUMMIES  =================================================================

# Definir variables categoricas
cols_f <- c("prop", "parent", "health_aff", "educ")
train[, cols_f] <- data.frame(apply(train[cols_f], 2, as.factor))
test[, cols_f] <- data.frame(apply(test[cols_f], 2, as.factor))
dev_set[, cols_f] <- data.frame(apply(dev_set[cols_f], 2, as.factor))

# Crear Dummies
train <- dummy_cols(train, select_columns = cols_f)
test <- dummy_cols(test, select_columns = cols_f)
dev_set <- dummy_cols(dev_set, select_columns = cols_f)

# AGRUPAR HOGARES ==============================================================

# Clasificador por hogares
train_h <- train[train$parent=="1",]
test_h <- test[test$parent=="1",]
dev_set_h <- dev_set[dev_set$parent=="1",]


# BALANCEO DE DATOS ============================================================

# Histograma - Desbalance
hist(train$poor, col="magenta")
prop.table(table(train$poor))

# Preparar datos para balanceo
aux <- train[, 2:46]

# 1. UnderSampling

# Undersample
undersample <- downSample(x = aux[, -ncol(aux)], y = as.factor(aux$poor))

# Revisar correcion de balance
hist(as.numeric(undersample$poor), col="coral", main="Balance de Clases: UnderSample", xlab="Pobre", ylab="Frecuencia")
prop.table(table(undersample$poor))

#2. SMOTE

# SMOTE 
aux[, cols_f] <- data.frame(apply(aux[cols_f], 2, as.numeric))
smote <- SMOTE(aux[, -7], aux$poor)
smote <- smote$data
names(smote)[names(smote) == "class"] <- "poor"


# Revisar correcion de balance
hist(as.numeric(smote$poor), col="coral", main="Balance de Clases: SMOTE", xlab = "Pobre", ylab = "Frecuencia")
prop.table(table(smote$poor))

#3. Undersampling para hogares

aux <- train_h[, 2:46]
under_h <- downSample(x = aux[, -ncol(aux)], y = as.factor(aux$poor))
under_h <- dplyr::select(under_h, -"Class")

# Revisar correcion de balance
hist(as.numeric(undersample_h$poor), col="coral")
prop.table(table(undersample_h$poor))

# MODELOS INGENUOS =============================================================

# Modelo 1: Logit (clasificacion directa) --------------------------------------


# Estimacion 1.1: undersampling
model.1 <- glm(as.numeric(poor) ~ clase + sex + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7 + parent_8 + parent_9
               + rooms + bedrooms + prop_2 + prop_3 + prop_4 + prop_5 + prop_6
               + nper,
               data = undersample, family = "binomial")

sum_m1 <- summary(model.1)

# Prediccion en train
m1_probs <- predict(model.1, type = "response", newdata = test)

# Matriz de confusion
m1_pred <- rep(0, length(fitted(model.1)))
m1_pred[m1_probs > 0.5] = 1
m1_acc <- table(m1_pred, model.1$y)

# Accuracy - 50%
confusionMatrix(m1_acc)

# Estimacion 1.2: SMOTE
model.1 <- glm(as.numeric(poor) ~ clase + sex + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7 + parent_8 + parent_9
               + rooms + bedrooms + prop_2 + prop_3 + prop_4 + prop_5 + prop_6
               + nper,
               data = smote, family = "binomial")

sum_m1 <- summary(model.1)

# Prediccion en train
m1_probs <- predict(model.1, type = "response", newdata = test)

# Matriz de confusion
m1_pred <- rep(0, length(fitted(model.1)))
m1_pred[m1_probs > 0.5] = 1
m1_acc <- table(m1_pred, model.1$y)

# Accuracy - 54.86%
confusionMatrix(m1_acc)

# Modelo 2: Regresion Lineal (prediccion de ingreso) ---------------------------

# Estimacion 2.1: undersampling
model.2 <- lm(ing ~ clase + sex + parent_2 + parent_3 + parent_4
              + parent_5 + parent_6 + parent_7 + parent_8 + parent_9
              + rooms + bedrooms + prop_2 + prop_3 + prop_4 + prop_5 + prop_6
              + nper,
              data = undersample)

sum_m2 <- summary(model.2)

# Prediccion en train
m2_probs <- predict(model.2, newdata = test)

# Matriz de confusion
m2_pred <- rep(0, length(m2_probs))
m2_pred[m2_probs < test$lp] <- 1
m2_acc <- table(m2_pred, test$poor)

# Accuracy - 75.1%
confusionMatrix(m2_acc)

# Estimacion 2.2: SMOTE
model.2 <- lm(ing ~ clase + sex + parent_2 + parent_3 + parent_4
              + parent_5 + parent_6 + parent_7 + parent_8 + parent_9
              + rooms + bedrooms + prop_2 + prop_3 + prop_4 + prop_5 + prop_6
              + nper,
              data = smote)

sum_m2 <- summary(model.2)

# Prediccion en train
m2_probs <- predict(model.2, newdata = test)

# Matriz de confusion
m2_pred <- rep(0, length(m2_probs))
m2_pred[m2_probs < test$lp] <- 1
m2_acc <- table(m2_pred, test$poor)

# Accuracy - 75.14%
confusionMatrix(m2_acc)


# MODELOS ======================================================================

# Modelo 3: Logit (clasificacion directa) --------------------------------------
# Estimacion 3.0: ingenuo
model.3 <- glm(poor ~ rooms + bedrooms + nper + npersug + clase
               + sex + age + health + rent_rec + help + grade + prop_2 + prop_3
               + prop_4 + prop_5 + prop_6 + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7 + parent_8 + parent_9 
               + health_aff_2 + health_aff_3 + health_aff_9 + educ_1 + educ_2
               + educ_3 + educ_4 + educ_5 + educ_9, 
               data = train, family = "binomial")

sum_m3 <- summary(model.3)

# Prediccion en train
m3_probs <- predict(model.3, type = "response", newdata = test)

# Matriz de confusion
m3_pred <- rep(0, length(fitted(model.3)))
m3_pred[m3_probs > 0.5] = 1
m3_acc <- table(m3_pred, model.3$y)

# Accuracy - 68.8%
confusionMatrix(m3_acc)

# Estimacion 3.1: undersampling
model.3 <- glm(as.numeric(poor) ~ rooms + bedrooms + nper + npersug + clase
               + sex + age + health + rent_rec + help + grade + prop_2 + prop_3
               + prop_4 + prop_5 + prop_6 + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7 + parent_8 + parent_9 
               + health_aff_2 + health_aff_3 + health_aff_9 + educ_1 + educ_2
               + educ_3 + educ_4 + educ_5, 
               data = undersample, family = "binomial")

sum_m3 <- summary(model.3)

# Prediccion en train
m3_probs <- predict(model.3, type = "response", newdata = test)

# Matriz de confusion
m3_pred <- rep(0, length(fitted(model.3)))
m3_pred[m3_probs > 0.5] = 1
m3_acc <- table(m3_pred, model.3$y)

# Accuracy - 50.01%
confusionMatrix(m3_acc)

# Estimacion 3.2: SMOTE
model.3 <- glm(as.numeric(poor) ~ rooms + bedrooms + nper + npersug + clase
               + sex + age + health + rent_rec + help + grade + prop_2 + prop_3
               + prop_4 + prop_5 + prop_6 + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7, parent_8, parent_9 
               + health_aff_2 + health_aff_3 + health_aff_9 + educ_1 + educ_2
               + educ_3 + educ_4 + educ_5 + educ_9, 
               data = smote, family = "binomial")

sum_m3 <- summary(model.3)

# Prediccion en train
m3_probs <- predict(model.3, type = "response", newdata = test)

# Matriz de confusion
m3_pred <- rep(0, length(fitted(model.3)))
m3_pred[m3_probs > 0.5] = 1
m3_acc <- table(m3_pred, model.3$y)

# Accuracy - 50.01%
confusionMatrix(m3_acc)

# Modelo 4: LDA (clasificicacion directa) --------------------------------------
#Estimacion 4.0: ingenuo

model.4 <- lda(as.numeric(poor) ~ rooms + bedrooms + nper + npersug + clase
                   + sex + age + rent_rec + help + grade + prop_2 + prop_3
                   + prop_4 + prop_5 + prop_6 + parent_2 + parent_3 + parent_4
                   + parent_5 + parent_6 + parent_7 + parent_8 + parent_9 
                   + health_aff_2 + health_aff_3 + health_aff_9 + educ_1 + educ_2
                   + educ_3 + educ_4 + educ_5 + educ_9, 
                   data = train)
model.4
model.4.pred <- predict(model.4, test)
model.4.class <- model.4.pred$class
m4_acc <- table(model.4.class, test$poor)

# Accuracy - 81.08%
confusionMatrix(m4_acc)

#Estimacion 4.1: Undersample

model.4 <- lda(as.numeric(poor) ~ rooms + bedrooms + nper + npersug + clase
               + sex + age + rent_rec + help + grade + prop_2 + prop_3
               + prop_4 + prop_5 + prop_6 + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7 + parent_8 + parent_9 
               + health_aff_2 + health_aff_3 + health_aff_9 + educ_1 + educ_2
               + educ_3 + educ_4 + educ_5, 
               data = undersample)
model.4
model.4.pred <- predict(model.4, test)
model.4.class <- model.4.pred$class
m4_acc <- table(model.4.class, test$poor)

# Accuracy - 71.46%
confusionMatrix(m4_acc)

#Estimacion 4.2: SMOTE

model.4 <- lda(as.numeric(poor) ~ rooms + bedrooms + nper + npersug + clase
               + sex + age + rent_rec + help + grade + prop_2 + prop_3
               + prop_4 + prop_5 + prop_6 + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7 + parent_8 + parent_9 
               + health_aff_2 + health_aff_3 + health_aff_9 + educ_1 + educ_2
               + educ_3 + educ_4 + educ_5, 
               data = smote)
model.4
model.4.pred <- predict(model.4, test)
model.4.class <- model.4.pred$class
m4_acc <- table(model.4.class, test$poor)

# Accuracy - 77.08%
confusionMatrix(m4_acc)


# Modelo 5: Regresion Lineal (prediccion de ingreso) ---------------------------

# Estimacion 5.0: ingenuo 
model.5 <- lm(ing ~ rooms + bedrooms + nper + npersug + clase
              + sex + age + rent_rec + help + grade + prop_2 + prop_3
              + prop_4 + prop_5 + prop_6 + parent_2 + parent_3 + parent_4
              + parent_5 + parent_6 + parent_7 + parent_8 + parent_9 
              + health_aff_2 + health_aff_3 + health_aff_9 + educ_1 + educ_2
              + educ_3 + educ_4 + educ_5, 
              data = train)

sum_m5 <- summary(model.5)

# Prediccion en train
m5_probs <- predict(model.5, newdata = test)

# Matriz de confusion
m5_pred <- rep(0, length(m5_probs))
m5_pred[m5_probs < test$lp] <- 1
m5_acc <- table(m5_pred, test$poor)

# Accuracy - 74.98%
confusionMatrix(m5_acc)

# Estimacion 5.1: Undersampling 
model.5 <- lm(ing ~ rooms + bedrooms + nper + npersug + clase
              + sex + age + rent_rec + help + grade + prop_2 + prop_3
              + prop_4 + prop_5 + prop_6 + parent_2 + parent_3 + parent_4
              + parent_5 + parent_6 + parent_7 + parent_8 + parent_9 
              + health_aff_2 + health_aff_3 + health_aff_9 + educ_1 + educ_2
              + educ_3 + educ_4 + educ_5, 
              data = undersample)

sum_m5 <- summary(model.5)

# Prediccion en train
m5_probs <- predict(model.5, newdata = test)

# Matriz de confusion
m5_pred <- rep(0, length(m5_probs))
m5_pred[m5_probs < test$lp] <- 1
m5_acc <- table(m5_pred, test$poor)

# Accuracy - 74.13%
confusionMatrix(m5_acc)

# Estimacion 5.2: SMOTE
model.5 <- lm(ing ~ rooms + bedrooms + nper + npersug + clase
              + sex + age + rent_rec + help + grade + prop_2 + prop_3
              + prop_4 + prop_5 + prop_6 + parent_2 + parent_3 + parent_4
              + parent_5 + parent_6 + parent_7 + parent_8 + parent_9 
              + health_aff_2 + health_aff_3 + health_aff_9 + educ_1 + educ_2
              + educ_3 + educ_4 + educ_5, 
              data = smote)

sum_m5 <- summary(model.5)

# Prediccion en train
m5_probs <- predict(model.5, newdata = test)

# Matriz de confusion
m5_pred <- rep(0, length(m5_probs))
m5_pred[m5_probs < test$lp] <- 1
m5_acc <- table(m5_pred, test$poor)

# Accuracy - 74.14%
confusionMatrix(m5_acc)


# Modelo 6: Random Forest ------------------------------------------------------]

# Estimacion 6.0: Ingenuo
under_h <- dplyr::select(under_h, -"poor", -"rent_rec", -"estrato", -"help")
under_h <- dplyr::select(under_h, -"parent_2", -"parent_3", -"parent_4", -"parent_5", -"parent_6", -"parent_7",-"parent_8",-"parent_9")
model.6 <- randomForest(ing ~ ., data = train_h, ntree = 2)
m6_probs <- predict(model.6, newdata = test_h)

# Matriz de confusion
m6_pred <- rep(0, length(m6_probs))
m6_pred[m6_probs < test_h$lp] <- 1
m6_acc <- table(m6_pred, test_h$poor)

# Accuracy - 83.14%
confusionMatrix(m6_acc)

# Estimacion 6.1: Undersampling
model.6 <- randomForest(ing ~ ., data = under_h, ntree = 4)
m6_probs <- predict(model.6, newdata = test_h)

# Matriz de confusion
m6_pred <- rep(0, length(m6_probs))
m6_pred[m6_probs < test_h$lp] <- 1
m6_acc <- table(m6_pred, test_h$poor)

# Accuracy - 83.14%
confusionMatrix(m6_acc)

# Estimacion 6.2: Undersampling - 16 trees
model.6 <- randomForest(ing ~ ., data = under_h, ntree = 16)
m6_probs <- predict(model.6, newdata = test_h)

# Matriz de confusion
m6_pred <- rep(0, length(m6_probs))
m6_pred[m6_probs < test_h$lp] <- 1
m6_acc <- table(m6_pred, test_h$poor)

# Accuracy - 83.14%
confusionMatrix(m6_acc)

# Estimacion 6.2: Undersampling - 64 trees
model.6 <- randomForest(ing ~ ., data = under_h, ntree = 64)
m6_probs <- predict(model.6, newdata = test_h)

# Matriz de confusion
m6_pred <- rep(0, length(m6_probs))
m6_pred[m6_probs < test_h$lp] <- 1
m6_acc <- table(m6_pred, test_h$poor)

# Accuracy - 83.14%
confusionMatrix(m6_acc)


# EXPORT =======================================================================

# Resolver NAs de Kaggle

kaggle <- dplyr::select(kaggle, id, P5000, P5010, Nper, P5130, P5140, Npersug,  
                      Lp, Clase.x, P5090, P6020, P6040, P6090, P7495,
                      P6050, P6100, P6210, P6210s1)

colnames(kaggle) <- c("id", "rooms", "bedrooms", "nper", "ayuda_1", "ayuda_2", 
                       "npersug", "lp", "clase", "prop", "sex", "age", "health",
                       "rent_rec", "parent", "health_aff", "educ", "grade")

kaggle_h <- kaggle[kaggle$parent=="1",]

# Crear variable "rent_est"
kaggle_h$ayuda_1 <- ifelse(is.na(kaggle_h$ayuda_1), 0, kaggle_h$ayuda_1)
kaggle_h$ayuda_2 <- ifelse(is.na(kaggle_h$ayuda_2), 0, kaggle_h$ayuda_2)
kaggle_h$rent_est <- kaggle_h$ayuda_1 + kaggle_h$ayuda_2
kaggle_h <- dplyr::select(kaggle_h, -ayuda_1, -ayuda_2)

# Contar NAs
colSums(is.na(kaggle_h)) %>% as.data.frame()

# Imputacion por Arboles de Decision

# I. Health
aux <- subset(kaggle_h, select = c("health", "age", "nper"))
aux$health <- ifelse(aux$health == "9", NA, aux$health)
tree_model <- rpart(health ~ ., data = aux, method = "class")
aux$health[is.na(aux$health)] <- predict(tree_model, newdata = aux[is.na(aux$health), ], type = "class")
aux$health <- ifelse(aux$health == "1", 0, 1)
kaggle_h$health <- aux$health

# II. Grade
aux <- subset(kaggle_h, select = c("grade", "age", "nper"))
tree_model <- rpart(grade ~ ., data = aux)
aux$grade[is.na(aux$grade)] <- predict(tree_model, newdata = aux[is.na(aux$grade), ])
kaggle_h$grade <- aux$grade

# III. Health_aff
aux <- subset(kaggle_h, select = c("health_aff", "age", "nper"))
tree_model <- rpart(health_aff ~ ., data = aux, method = "class")
aux$health_aff[is.na(aux$health_aff)] <- predict(tree_model, newdata = aux[is.na(aux$health_aff), ], type = "class")
kaggle_h$health_aff <- aux$health_aff

# Check - Conteo de NAs
colSums(is.na(kaggle_h)) %>% as.data.frame()

# Dummies
kaggle_h[, cols_f] <- data.frame(apply(kaggle_h[cols_f], 2, as.factor))

# Crear Dummies
kaggle_h <- dummy_cols(kaggle_h, select_columns = cols_f)

# Estimacion 6.2: Undersampling - 16 trees
model.6 <- randomForest(ing ~ ., data = under_h, ntree = 64)
m6_probs <- predict(model.6, newdata = kaggle_h)

# Matriz de confusion
m6_pred <- rep(0, length(m6_probs))
m6_pred[m6_probs < kaggle_h$lp] <- 1

export <- cbind(kaggle_h$id, m6_pred)
export <- as.data.frame(export)
colnames(export) <- c("id", "pobre")
write.csv(export, "C:\\Users\\Usuario\\Downloads\\forest.csv", row.names=FALSE)
