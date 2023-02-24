#Limpiamos el espacio de trabajo
rm(list = ls())

#Requerimos los paquetes que vamos a usar
library(pacman)
library(boot)
p_load(rvest, tidyverse)

#Obtenemos los datos

db_test_personas<- read.csv("C:/BigDataLaura/Repositorio_Taller2/Files and Data/test_personas.csv")
db_train_personas<- read.csv("C:/BigDataLaura/Repositorio_Taller2/Files and Data/train_personas.csv")
db_test_hogares<- read.csv("C:/BigDataLaura/Repositorio_Taller2/Files and Data/train_hogares.csv")
db_train_hogares

glimpse(db_train_hogares)

glimpse(db_test_hogares)
summary (db_test_hogares$Dominio)
db_test_hogares$Dominio


dic_pobre = c("Pobres", "No Probres")
db_test_hogares <- db_test_hogares %>%
  mutate(Pobre = factor(db_test_hogares$Pobre, 
                          levels = c(0, 1),
                          labels = dic_pobre))
prop.table(table(db_test_hogares$Pobre))

db_test_hogares <- db_test_hogares %>%
  mutate(pobre_l = db_test_hogares$Pobre == "Pobres")
class(db_test_hogares$pobre_l)


ggplot(db_test_hogares, aes(x = db_test_hogares$Pobr)) +
  geom_bar(fill = "purple") +
  theme_bw() +
  labs(title = "¿Con qué frecuencia se encontraron hogares categorizados como Pobres o No Pobres?",
       x = "",
       y = "frecuencia") +
  coord_flip()

#class(db_test_hogares$Pobr)

#*********************** Function LogLikelihood  ***************************

LogLikelihood <- function (y_pred, y_true) {
  # Número cercano a cercano para evitar log(0) - indeterminación
  eps <- 1e-15 
  # Si la probabilidad predicha es 0, agregale eps
  # Si la probabilidad predicha es 1, restele eps
  y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
  # Pasamos de booleano a numerico
  y_true <- y_true + 0
  LogLoss <- sum(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
  return(LogLoss)
}

#*********************** Fin Function LogLikelihood  ***************************

#************ Vista general de Verosimilitud   ********************
y_hat <- seq(0.001, 0.999, length.out = 100)
y_hat

l <- c()
for (i in 1:100) {
  li <- LogLikelihood(y_pred = y_hat[i], y_true = 1)
  l <- c(l, li)
}

plot_f <- data.frame(y_hat = y_hat, log_likelihood = l)
plot_f
ggplot(plot_f, aes(x = y_hat, y = log_likelihood)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  theme_bw() +
  labs(x = "Valor predicho", y = "Log-verosimilitud")

#***************************Modelo Verosimilitud con 75% de las personas pobres **********************

nrow (db_test_hogares)
n_hogares <- nrow (db_test_hogares)
n_hogares
y_hat_pobres <- rep(0, n_hogares)
db_test_hogares_num <-as.numeric (db_test_hogares$Pobre)
l_pobres <- LogLikelihood(y_pred = y_hat_pobres, y_true = db_test_hogares_num )


y_hat_noprobres <- rep(1, n_hogares)
l_nopobres <- LogLikelihood(y_pred = y_hat_noprobres, y_true = db_test_hogares_num)
#class(db_test_hogares_num)

# Log-verosimilitud cuando el 75% de las personas es Pobres seleccionado al azar

y_hat_pobres75 <- c(rep(0, round(n_hogares*0.75, 0)), rep(1, round(n_hogares*0.25, 0)))

# Corremos 10000 simulaciones

set.seed(666)
l_pobres75 <- c()
db_test_hogares_num <-as.numeric (db_test_hogares$pobre_l)
n <- 100
for (i in 1:n) {          # Cambiar a 10.000 para la entrega final
  y_hat_pobres75_i <- sample(y_hat_pobres75, n_hogares)
  l_pobres75_i <- LogLikelihood(y_pred = y_hat_pobres75_i, 
                                y_true = db_test_hogares_num)
  l_pobres75 <- c(l_pobres75,l_pobres75_i) 
  if (i == n){
     print (paste("**** Se han simulado [ i=", i, "] veces"))
  }
}


# Log-verosimilitud cuando el 100% de las predicciones es correcta
l_maximo <- LogLikelihood(y_pred = db_test_hogares$pobre_l, y_true = db_test_hogares$pobre_l)

ggplot() +
  geom_histogram(aes(x = l_pobres75), fill = "darkblue") + theme_bw() +
  geom_vline(aes(xintercept = l_pobres, 
                 color = "100% Pobres"), linetype = "dashed") +
  geom_vline(aes(xintercept = l_nopobres, 
                 color = "100% No Pobres"), linetype = "dashed") +
  geom_vline(aes(xintercept = l_maximo, 
                 color = "Máxima Log-verosimilitud"), 
             linetype = "dashed") +
  labs(x = "Log-verosimilitud", y = "Frecuencia") +
  scale_color_manual(name = "Escenario", 
                     values = c("100% No Pobres" = "red", 
                                "100% Pobres" = "blue",
                                "Máxima Log-verosimilitud" = "green"))

#***************************Fin Modelo Verosimilitud con 75% de las personas pobres **********************

# OJO: siempre es mejor las variables y tener claridad, cuales son continuas, dicotomas o categoricas

db_test_hogares <- db_test_hogares %>%
  mutate(Ingtotug = factor(db_test_hogares$Ingtotug), 
         Nper = factor(db_test_hogares$Nper),
         Depto = factor(db_test_hogares$Depto))

glimpse(db_test_hogares)

# Dummyficamos ANTES de partir la base en train/test utilizando Caret
# Adicionalmente, reamos las variables edad y años de casados al cuadrado

db_test_hogShort <- db_test_hogares %>%select (id,Nper, Depto, Ingtotug,Pobre)
db_test_hogShort <- db_test_hogShort[1:1000,]
dumificador <- dummyVars(formula = ~ . + I(as.numeric(Nper)^2) + I(Ingtotug) *  I(Depto)- Pobre-1, 
                         data = db_test_hogShort, fullRank = T)
db<- predict (dumificador, newdata=db_test_hogShort)

# resto -1, para quitar el intecepto, que no me tenga en cuenta affaris, el Fullrank, elimine varibales  multicolinealidad 

db <- as.data.frame(db)
glimpse(db)
db

prueba <-lm(formula=PobreTRUE ~ ., data=data.frame(db))
summary(prueba)
glimpse(prueba)
prueba$coefficients

multicolinealidad <- names(prueba$coefficients[is.na(prueba$coefficients)])
db <- db %>% 
  data.frame() %>%
  select(-multicolinealidad)

# Eliminamos columna por multicolinealidad
# db$gender.male <- NULL

# Dividimos train/test (70/30) usando caret

set.seed(666)
trainIndex <- createDataPartition(db$PobreTRUE, p = 0.7, list = TRUE)
trainIndex$Resample
train <- db[trainIndex$Resample,]
test <- db[-trainIndex$Resample,]
# db$gender.male <- NULL
#db <- db %>%
#  select(-gender.male)

# Analizamos que tan bien quedó partida la base OJOOOOOOO siempre hay que hacerlo
prop.table(table(db$PobreTRUE))


# Estandarizamos DESPUÉS de partir la base en train/test
train_s <- train
test_s <- test
# igual dejamos train y test  en backup, para que no se nos estalle

variables_numericas <- c("No Persona Hogar", "Departamento o locación", "Salario")

escalador <- preProcess(train[, variables_numericas],method = c("center", "scale"))
train_s[, variables_numericas] <- predict(escalador, train[, variables_numericas])

test_s[, variables_numericas] <- predict(escalador, test[, variables_numericas])

train_s <- as_tibble(train_s)
test_s <- as_tibble(test_s)
train <- as_tibble(train)
test <- as_tibble(test)

# train_s$infielTRUE <- as.numeric(train_s$infielTRUE)


modelo1 <- train(x = select(train_s, -PobreTRUE), 
                 y = as.factor(train_s$PobrelTRUE),
                 preProcess = NULL,
                 method = "glmnet")

#****************************  por aqui vamos....d
vi
modelo1

# modelo1 <- lm(formula = infielTRUE ~ ., data = train_s)
# summary(modelo1)

y_hat_insample1 <- predict(modelo1, train_s)
y_hat_outsample1 <- predict(modelo1, test_s)
probs_insample1 <- predict(modelo1, train_s, type = "prob")[, "1", drop = T]
probs_outsample1 <- predict(modelo1, test_s, type = "prob")[, "1", drop = T]

acc_insample1 <- Accuracy(y_pred = y_hat_insample1, y_true = train$infielTRUE)
acc_outsample1 <- Accuracy(y_pred = y_hat_outsample1, y_true = test$infielTRUE)

acc_insample1
acc_outsample1

# Accurracy es lo que acabamos de hacer

# Pero lo que quiero es ir buscar los 1, vamos a ir por los 1, los infieles

pre_insample1 <- Precision(y_pred = y_hat_insample1, 
                           y_true = train$infielTRUE, positive = 1)
pre_outsample1 <- Precision(y_pred = y_hat_outsample1, 
                            y_true = test$infielTRUE, positive = 1)
pre_insample1
pre_outsample1


rec_insample1 <- Recall(y_pred = y_hat_insample1, 
                        y_true = train$infielTRUE, positive = 1)
rec_outsample1 <- Recall(y_pred = y_hat_outsample1, 
                         y_true = test$infielTRUE, positive = 1)
rec_insample1
rec_outsample1

# La media armonica, es estsa

f1_insample1 <- F1_Score(y_pred = y_hat_insample1, 
                         y_true = train$infielTRUE, positive = 1)
f1_outsample1 <- F1_Score(y_pred = y_hat_outsample1, 
                          y_true = test$infielTRUE, positive = 1)
f1_insample1
f1_outsample1

# para correr un logit, uso glmnet


metricas_insample1 <- data.frame(Modelo = "Regresión logistica", 
                                 "Muestreo" = NA, 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample1,
                                 "Precision - PPV" = pre_insample1,
                                 "Recall - TPR - Sensitivity" = rec_insample1,
                                 "F1" = f1_insample1)
metricas_insample1
metricas_outsample1

metricas_outsample1 <- data.frame(Modelo = "Regresión logistica", 
                                  "Muestreo" = NA, 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample1,
                                  "Precision - PPV" = pre_outsample1,
                                  "Recall - TPR - Sensitivity" = rec_outsample1,
                                  "F1" = f1_outsample1)
metricas_insample1
metricas_outsample1

metricas1 <- bind_rows(metricas_insample1, metricas_outsample1)
library (kableExtra)
metricas1 %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)

metricas1

#*******************************************
#** Ahora las tecnicas para poder atacar el desbalanceo
#** upSample(), downSample() or undersample
#** desventaja esta metiendole ruido...
#* resampling o oversaplimg, Downsample, estas perdiendo muestra
#* La funcion de perdida, es la mejor, ponerle pesos
#* Y optimizar el umbral de decision

# Implementamos upsample
train_s$infielTRUE <- factor(train_s$infielTRUE)
train_s2 <- upSample(x = select(train_s, -infielTRUE), 
                     y = train_s$infielTRUE, list = F, yname = "infielTRUE")

prop.table(table(train_s$infielTRUE))

table (train_s$infielTRUE)
nrow(train_s)
table (train_s2$infielTRUE)
nrow(train_s2)

#***************************************************
# train_s$infielTRUE <- as.numeric(train_s$infielTRUE)
modelo2 <- train(x = select(train_s2, -infielTRUE), 
                 y = as.factor(train_s2$infielTRUE),
                 preProcess = NULL,
                 method = "glmnet")





  
  