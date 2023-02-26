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
library(rpart)
library(smotefamily)
library(tidyverse)


# DATOS ========================================================================
# Base de datos ----------------------------------------------------------------

# Semilla
set.seed(4040)

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
train$rent_rec <- train$ayuda_1 + train$ayuda_2
train <- dplyr::select(train, -ayuda_1, -ayuda_2)

# Resolver NAs -----------------------------------------------------------------

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

# DUMMIES  =================================================================

# Definir variables categoricas
cols_f <- c("prop", "parent", "health_aff", "educ")
train[, cols_f] <- data.frame(apply(train[cols_f], 2, as.factor))
test[, cols_f] <- data.frame(apply(test[cols_f], 2, as.factor))

# Crear Dummies
train <- dummy_cols(train, select_columns = cols_f)


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
hist(as.numeric(undersample$poor), col="coral")
prop.table(table(undersample$poor))

#2. SMOTE

# SMOTE 
aux[, cols_f] <- data.frame(apply(aux[cols_f], 2, as.numeric))
smote <- SMOTE(aux[, -7], aux$poor)
smote <- smote$data
names(smote)[names(smote) == "class"] <- "poor"


# Revisar correcion de balance
hist(as.numeric(smote$poor), col="coral")
prop.table(table(smote$poor))


# MODELOS INGENUOS =============================================================

# Modelo 1: Logit (clasificacion directa) --------------------------------------


# Estimacion 1.1: undersampling
model.1 <- glm(as.numeric(pobre) ~ clase_2 + sex_2 + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7 + parent_8 + parent_9
               + parent_3 + rooms + bedrooms + property_2 + property_3
               + property_4 + property_5 + property_6 + nper,
               data = smote, family = "binomial")

sum_m1 <- summary(model.1)

# Prediccion en train
m1_probs <- predict(model.1, type = "response", newdata = test_nan)

# Matriz de confusion
m1_pred <- rep(0, length(fitted(model.1)))
m1_pred[m1_probs > 0.5] = 1
m1_acc <- table(m1_pred, model.1$y)

# Accuracy - 50.08%
confusionMatrix(m1_acc)

# Estimacion 1.2: SMOTE
model.1 <- glm(as.numeric(pobre) ~ clase_2 + sex_2 + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7 + parent_8 + parent_9
               + parent_3 + rooms + bedrooms + property_2 + property_3
               + property_4 + property_5 + property_6 + nper,
               data = smote, family = "binomial")

sum_m1 <- summary(model.1)

# Prediccion en train
m1_probs <- predict(model.1, type = "response", newdata = test_nan)

# Matriz de confusion
m1_pred <- rep(0, length(fitted(model.1)))
m1_pred[m1_probs > 0.5] = 1
m1_acc <- table(m1_pred, model.1$y)

# Accuracy - 54.84%
confusionMatrix(m1_acc)

# Modelo 2: Regresion Lineal (prediccion de ingreso) ---------------------------

# Estimacion 2.1: undersampling
model.2 <- lm(ingtotug ~ clase_2 + sex_2 + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7 + parent_8 + parent_9
               + parent_3 + rooms + bedrooms + property_2 + property_3
               + property_4 + property_5 + property_6 + nper,
               data = undersample)

sum_m2 <- summary(model.2)

# Prediccion en train
m2_probs <- predict(model.2, newdata = test_nan)

# Matriz de confusion
m2_pred <- rep(0, length(m2_probs))
m2_pred[m2_probs < test_nan$lp] <- 1
m2_acc <- table(m2_pred, test_nan$pobre)

# Accuracy - 74.98%
confusionMatrix(m2_acc)

# Estimacion 2.2: SMOTE
model.2 <- lm(ingtotug ~ clase_2 + sex_2 + parent_2 + parent_3 + parent_4
              + parent_5 + parent_6 + parent_7 + parent_8 + parent_9
              + parent_3 + rooms + bedrooms + property_2 + property_3
              + property_4 + property_5 + property_6 + nper,
              data = smote)

sum_m2 <- summary(model.2)

# Prediccion en train
m2_probs <- predict(model.2, newdata = test_nan)

# Matriz de confusion
m2_pred <- rep(0, length(m2_probs))
m2_pred[m2_probs < test_nan$lp] <- 1
m2_acc <- table(m2_pred, test_nan$pobre)

# Accuracy - 75.05%
confusionMatrix(m2_acc)

# Subset Selection
sub_sel <- regsubsets(as.factor(class)~., data = tr_b, nvmax = 15)
sum.sub_sel <- summary(sub_sel)

# Metricas
Adj.R2 <- which.max(sum.sub_sel$adjr2)
CP <- which.min(sum.sub_sel$cp)
BIC <- which.min(sum.sub_sel$bic)
data.frame(Adj.R2, CP, BIC)

as.data.frame(coef(sub_sel, 12))

# Comparacion
lm.0 <- glm(as.numeric(class)~., data = tr_b, family="binomial")
lm.probs.0 <- predict(lm.0, type="response")
lm.pred.0 <- rep(0, length(lm.probs.0))
lm.pred.0[lm.probs.0 > 0.5] <- 1
table(lm.pred.0, tr_b$class)

confusionMatrix(data = as.factor(lm.pred.0), 
                reference = as.factor(tr_b$class), 
                positive="1")

lm.1 <- glm(as.numeric(class) ~ clase + age + parent + rooms + bedrooms 
            + property + nper + npersug + ingtotug + npobres + sex_2, data=tr_b, 
            family = "binomial")
lm.probs.1 <- predict(lm.1, type="response")
lm.pred.1<- rep(0, length(lm.probs.1))
lm.pred.1[lm.probs.1 > 0.5] <- 1
table(lm.pred.1, tr_b$class)

# Modelo de Ignacio
levels(tr_b$class) <- c("no_pobre", "pobre")
model.1.smote <- train(as.factor(class) ~ as.factor(clase_2) + as.factor(sex_2) + rooms + bedrooms + as.factor(property_2) + as.factor(property_3) + as.factor(property_4) + as.factor(property_5) + as.factor(property_6) + nper,
                            data = tr_b, 
                            method = "glmnet",
                            trControl = ctrl,
                            family = "binomial", 
                            metric = "ROC",
)

mylogit_lasso_smote







