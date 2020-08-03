#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Proyecto:     Análisis de Score Credict
# Descripcion:  Árboles de decisión
# Datos:        Cupo_com.rds & Consumo_com.rds
# Por:          Felipe Ruiz
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list = ls())

##  analisis mediante arboles de decision
library(rpart)  
library(rattle) 
library(RColorBrewer)
library(caret)
library(rpart.plot)

#### CUPO ####

# Leer datos
cupo = readRDS("./Datos/Cupo_com.rds")

set.seed(1234567)
p          <- createDataPartition(y=cupo$ByM, p= 70/100,list=FALSE)

# Seleccion de la muestra
training   <- cupo[p,]
test       <- cupo[-p,]

# Comprobamos si las muestras están balanceadas
table(training$ByM)/length(training$ByM)
table(test$ByM)/length(test$ByM)

# Escoger las variables del data set 
X      <- c("SMMLV","Edad","MontoAprobado","AntiguedadLaboral","estadocivil","Desctipocontrato")
y      <- "ByM"                   # nuestro objetivo

## construccion del arbol de decision                              
d <- training[c(X, y)] #  data frame de entrenamiento


## MODELO 1 -> EDAD
set.seed(1)           # inicializacion generador de num aleatorios
model1 <- rpart(ByM ~ Edad,data = d, method="class",
                control = rpart.control(minsplit = 1, cp = 0))  # fitting

# dibuja el arbol de decision
x11(15,10)
fancyRpartPlot(model1)      

# Predicciones
predicciones1 <- predict(model1,newdata = test, type="class")

# Matriz de confusión
print(confusionMatrix(predicciones1,test$ByM))

#### Se categoriza la edad
cupo$Edad_RMod <- car::recode(cupo$Edad, "0:28='Menos de 28';29:34='Entre 29 y 34';
                                      35:48='Entre 35 y 48';49:66='Entre 49 y 66'; 
                                      66:120='Mas de 66'", as.factor.resul=T)

## MODELO2 -> SalarioBasicoSolicitante
set.seed(1)           # inicializacion generador de num aleatorios
model2 <- rpart(ByM ~ SMMLV,data = d, method="class",
                control = rpart.control(minsplit = 1, cp = 0 ))

# dibuja el arbol de decision
x11(15,10)
fancyRpartPlot(model2)      

# Predicciones
predicciones2 <- predict(model2,newdata = test, type="class")

# Matriz de confusión
print(confusionMatrix(predicciones2,test$ByM))


## MODELO3 -> MontoAprobado
set.seed(1)           # inicializacion generador de num aleatorios
model3 <- rpart(ByM ~ MontoAprobado,data = d, method="class",
                control = rpart.control(minsplit = 1, cp = 0 ))

# dibuja el arbol de decision
x11(15,10)
fancyRpartPlot(model3)      

# Predicciones
predicciones3 <- predict(model3,newdata = test, type="class")

# Matriz de confusión
print(confusionMatrix(predicciones3,test$ByM))

# Escribimos cupo
saveRDS(cupo, file = "./Datos/Cupo_com.rds")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### CONSUMO ####

rm(list = ls())

##  analisis mediante arboles de decision
library(rpart)  
library(rattle) 
library(RColorBrewer)
library(caret)

# Leer datos
consumo = readRDS("./Datos/Consumo_com.rds")

set.seed(1234567)
p          <- createDataPartition(y=consumo$ByM, p= 70/100,list=FALSE)

# Seleccion de la muestra
training   <- consumo[p,]
test       <- consumo[-p,]

# Comprobamos si las muestras están balanceadas
table(training$ByM)/length(training$ByM)
table(test$ByM)/length(test$ByM)

# Escoger las variables del data set 
X      <- c("SMMLV","Edad","MontoAprobado","AntiguedadLaboral")
y      <- "ByM"                   # nuestro objetivo

## construccion del arbol de decision                              
d <- training[c(X, y)] #  data frame de entrenamiento

## MODELO 1 -> EDAD
set.seed(1)           # inicializacion generador de num aleatorios
model1 <- rpart(ByM ~ Edad,data = d, method="class",
                control = rpart.control(minsplit = 1, cp = 0))  # fitting

# dibuja el arbol de decision
x11(15,10)
fancyRpartPlot(model1)      

# Predicciones
predicciones1 <- predict(model1,newdata = test, type="class")

# Matriz de confusión
print(confusionMatrix(predicciones1,test$ByM))

## MODELO2 -> SalarioBasicoSolicitante
set.seed(1)           # inicializacion generador de num aleatorios
model2 <- rpart(ByM ~ SMMLV,data = d, method="class",
                control = rpart.control(minsplit = 1, cp = 0 ))

# dibuja el arbol de decision
x11(15,10)
fancyRpartPlot(model2)      

# Predicciones
predicciones2 <- predict(model2,newdata = test, type="class")

# Matriz de confusión
print(confusionMatrix(predicciones2,test$ByM))


## MODELO3 -> MontoAprobado
set.seed(1)           # inicializacion generador de num aleatorios
model3 <- rpart(ByM ~ MontoAprobado,data = d, method="class",
                control = rpart.control(minsplit = 1, cp = 0 ))

# dibuja el arbol de decision
x11(15,10)
fancyRpartPlot(model3)

# Predicciones
predicciones3 <- predict(model3,newdata = test, type="class")

# Matriz de confusión
print(confusionMatrix(predicciones3,test$ByM))

# No hay hay necesidad de reescribir consumo