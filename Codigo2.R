datos <- read.csv("TablaVigilancia.csv", header=TRUE)
names(datos)
Xs <- datos[,c(13,14,16)]
unique(Xs[, 3])
datos <- datos[,c(29:44)]
X <- rowSums(datos)
ADVE <- (X - 16)/64

#################### HISTOGRAMA ########################
corte <- seq(0, 1, by = 0.05)
intervalos <- cut(ADVE, breaks = corte, include.lowest = TRUE)
frecuenciasInter <- table(intervalos)

#ann=TRUE permite que se muestren las anotaciones.
#axes=FALSE desactiva los ejes por defecto, pero luego se agregan manualmente.

grafico <- barplot(frecuenciasInter, main = "Comportamiento del ADVE en la población mexicana",
        xlab = "Valor de ADVE", ylab = "Cantidad de Personas",
        col = "skyblue", border = "black", ann=TRUE, axes=FALSE)

usr <- par("usr")
rangoY <- c(usr[1:2], 0, max(frecuenciasInter))
par(usr=rangoY)
axis(2, at=seq(0, 90, 5))
axis(1, at=grafico, labels=names(frecuenciasInter))
box()

#################### ESTADÍSTICAS DESCRIPTIVAS ########################
n <- length(datos)
mean(ADVE)
desviacionEstandar <- sqrt(var(ADVE)); desviacionEstandar
desviacionEstandar / sqrt(n)

sexo <- Xs[,2]; class(sexo)
df <- as.data.frame(sexo, stringsAsFactors = FALSE)
df[df == ""] <- "Hombre"
df[df == "No quiero responder"] <- "Mujer"
sexo <- as.matrix(df); class(sexo)

carrera <- Xs[,3]
df <- as.data.frame(carrera, stringsAsFactors = FALSE)
df[df == ""] <- "Licenciatura Completa"
carrera <- as.matrix(df); class(carrera)

data <- cbind(Xs[,1], sexo, carrera)


#################### MODELO MULTINOMIAL ########################
####################      FACTORES      ########################
library(nnet)
breaks <- c(-Inf, 0.25, 0.5, 0.75, Inf)
labels <- c("bajo", "medio", "medio alto", "alto")
ADVEcat <- cut(ADVE, breaks = breaks, labels = labels, right = FALSE)
ADVEcat <- factor(ADVEcat, levels = c("bajo", "medio", "medio alto", "alto"))

breaks <- c(-Inf, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf)
labels <- c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "90 en adelante")
edad <- data[,1]
edad <- as.numeric(edad)
edad <- cut(edad, breaks = breaks, labels = labels, right = FALSE)
edad <- factor(edad)
sexo <- factor(data[,2])
carrera <- factor(data[,3])

data <- cbind(ADVEcat, edad, sexo, carrera)
data <- as.data.frame(data)


####################  AJUSTE DEL MODELO  ########################
modelo <- multinom(ADVEcat ~ edad + sexo + carrera, data = data)
summary(modelo)
predicciones <- predict(modelo, newdata = data)

library(caret)
library(ggplot2)
data$ADVEcat <- factor(data$ADVEcat)
confusionMatrix(predicciones, data$ADVEcat)

###############################
library(mgcv)
install.packages("VGAM")
library(VGAM)

modelo_gam <- gam(ADVEcat ~ s(edad, bs = "re") + sexo + carrera, data = data, family = multinom(K = 3))


###################### RANDOM FOREST ######################
install.packages("h2o")
library(rsample)      
library(randomForest) 
library(ranger) 
library(caret)
library(h2o)  
library(caret)

set.seed(123)
muestra <- initial_split(data, prop = .7)
datosEnt <- training(muestra )
datosPru  <- testing(muestra )

modelo <- randomForest(formula = factor(ADVEcat) ~ ., data = datosEnt)
plot(modelo)
summary(modelo)

p1 <- predict(modelo, datosEnt)
p1_factor <- factor(p1, levels = unique(p1))
datosEnt_ADVEcat_factor <- factor(datosEnt$ADVEcat, levels = unique(datosEnt$ADVEcat))
confusionMatrix(p1_factor, datosEnt_ADVEcat_factor)

###################
p1 <- predict(modelo, datosPru)
p1_factor <- factor(p1, levels = unique(p1))
datosPru_ADVEcat_factor <- factor(datosPru$ADVEcat, levels = unique(datosEnt$ADVEcat))
confusionMatrix(p1_factor, datosPru_ADVEcat_factor)

