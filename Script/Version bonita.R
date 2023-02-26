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
library(smotefamily)
library(tidyverse)


# DATOS ========================================================================
# Base de datos ----------------------------------------------------------------

# Importacion de datos
tr.h <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/train_hogares.csv")
t.h <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/test_hogares.csv")
tr.p <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/train_personas.csv")
t.p <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/test_personas.csv")

# Union de las bases de datos
train <- merge(tr.p,tr.h,by='id')
test <- merge(test_personas,test_hogares,by='id')

# Seleccion de datos -----------------------------------------------------------

# Preservar variables con valores completos
cols_preserve <- c("id", "Clase.x", "P6020", "P6040", "P6050", "P5000", "P5010",
              "P5090", "Nper", "Npersug", "Ingtotug", "Lp", "Pobre", "Npobres")
tr <- train[,cols_preserve] 

# Definir variables categoricas con 0 NAs
cols_nan_cat <- c("Clase.x", "P6020", "P6050", "P5090", "Pobre")
tr[,cols_nan_cat] <- data.frame(apply(tr[cols_nan_cat], 2, as.factor))

# Nombres legibles...
colnames(tr) <- c("id", "clase", "sex", "age", "parent", "rooms", "bedrooms", 
            "property", "nper", "npersug", "ingtotug", "lp", "pobre", "npobres")
cols_nan_cat <- c("clase", "sex", "parent", "property", "pobre")

# Dummy Variables
tr <- fastDummies::dummy_cols(tr, select_columns=cols_nan_cat)
tr$pobre <- as.numeric(as.character(tr$pobre))


# MODELOS INGENUOS =============================================================

# Modelo 1: Logit (clasificacion directa) --------------------------------------
# Estimacion
model.1 <- glm(pobre ~ clase_2 + sex_2 + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7 + parent_8 + parent_9
               + parent_3 + rooms + bedrooms + property_2 + property_3
               + property_4 + property_5 + property_6 + nper,
               data = tr, family = "binomial")

sum_m1 <- summary(model.1)

# Prediccion en train
m1_probs <- predict(model.1, type = "response")

# Matriz de confusion
m1_pred <- rep(0, length(fitted(model.1)))
m1_pred[m1_probs > 0.5] = 1
m1_acc <- table(m1_pred, model.1$y)

# Accuracy - 77.56%
confusionMatrix(m1_acc)

# Modelo 2: Regresion Lineal (prediccion de ingreso) ---------------------------
# Estimacion
model.2 <- lm(ingtotug ~ clase_2 + sex_2 + parent_2 + parent_3 + parent_4
               + parent_5 + parent_6 + parent_7 + parent_8 + parent_9
               + parent_3 + rooms + bedrooms + property_2 + property_3
               + property_4 + property_5 + property_6 + nper,
               data = tr)

sum_m2 <- summary(model.2)

# Prediccion en train
m2_pred <- predict(model.2)

# Matriz de confusion
m2_poor <- rep(0, length(fitted(model.2)))
m2_c <- cbind.data.frame(tr$pobre , tr$lp, m2_pred, m2_poor)
m2_c$m2_poor <- ifelse(tr$lp>m2_pred, 1, 0)
m2_acc <- table(m2_c[,4], m2_c[,1])

# Accuracy - 74.94%
confusionMatrix(m2_acc)


# BALANCEO DE DATOS ============================================================

# Histograma - Desbalance
hist(tr$pobre, col="blue")

# SMOTE 
aux <- tr[,2:35]
col_numer <- c("clase", "sex", "parent", "property", "pobre")
aux[,col_numer] <- data.frame(apply(aux[col_numer], 2, as.numeric))
smote <- SMOTE(aux[,-12], aux$pobre)
tr_b <- smote$data

# Revisar correcion de balance
hist(as.numeric(tr_b$class), col="coral")
prop.table(table(tr_b$class))


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







