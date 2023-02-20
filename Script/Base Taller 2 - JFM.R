
# LIBRERIAS
library(e1071)
library(MASS)

# DATOS

# Importacion
train_hogares <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/train_hogares.csv")
test_hogares <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/test_hogares.csv")
train_personas <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/train_personas.csv")
test_personas <- read.csv("~/Downloads/uniandes-bdml-20231-ps2/test_personas.csv")

# Union de las bases de datos
train_merge <- merge(train_personas,train_hogares,by='id')
test_merge <- merge(test_personas,test_hogares,by='id')

# LOGIT

# Modelo
glm.fits <- glm(Pobre ~ P5000 + Nper + P6210s1 + Ingtot + Des + P6050,
                data = train_merge, family = "binomial")
summary(glm.fits)

# Train check
glm.probs <- predict(glm.fits, type = "response")
glm.pred <- rep("No-pobre", length(fitted(glm.fits)))
glm.pred[glm.probs > .6] = "Pobre"
table(glm.pred, glm.fits$y)

# Test!
glm.probs <- predict(glm.fits, test_merge , type = "response")
glm.pred <- rep("No-pobre", length(fitted(glm.fits)))
glm.pred[glm.probs > .6] = "Pobre"
table(glm.pred, glm.fits$y)

# LDA

lda.fit <- lda(
  Pobre ~ P5000 + Nper + P6210s1 + Ingtot + P6050,
  data = train_merge)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, train_merge)
lda.class <- lda.pred$class
table(lda.class, train_merge$Pobre)
(338454+16400)/(338454+16400+83371+9434)

# QDA

qda.fit <- qda(
  Pobre ~ P5000 + Nper + P6210s1 + Ingtot + P6050,
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


