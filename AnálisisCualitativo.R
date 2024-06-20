datosDH <- read.csv("TablaDDH.csv", header=FALSE)
datosDH[is.na(datosDH)] <- 0
datosDH <- as.matrix(datosDH)
datosDH <- rbind(datosDH, matrix(0, nrow = 1, ncol = 28))

### VERIFICACIÓN DE VALORES ###
if (all(datosDH == 0 | datosDH == 1)) {
  print("Los datos contienen únicamente 0 y 1.")
} else {
  print("La matriz contiene otros valores además de 0 y 1.")
  otros_valores <- unique(datosDH[datosDH != 0 & datosDH != 1])
  print(paste("Otros valores encontrados:", otros_valores))
}

contar_filas_de_ceros <- function(matriz) {
  contador <- 0
  for (i in 1:nrow(matriz)) {
    if (all(matriz[i,] == 0)) {
      contador <- contador + 1
    }
  }
  return(contador)
}

contar_filas_de_ceros(datosDH)



imprimir_filas_de_ceros <- function(matriz) {
  for (i in 1:nrow(matriz)) {
    if (all(matriz[i,] == 0)) {
      cat("Fila", i, ":", toString(matriz[i,]), "\n")
    }
  }
}
A <- imprimir_filas_de_ceros(datosDH)



suma_filas <- rowSums(datosDH)

frecuencias <- table(suma_filas)

# Luego, utilizamos barplot para crear el gráfico de barras
barplot(frecuencias, main = "Cantidad de Derechos que respondieron las personas",
        xlab = "Cantidad de Derechos", ylab = "Frecuencia",
        col = "skyblue", border = "black")

##########################################################
datosVigilancia <- read.csv("TablaVigilancia.csv", header=TRUE)

### VERIFICACIÓN DE VALORES ###
if (all(datosVigilancia[,5] == "es-MX")) {
  print("Todos los encuestados son Mexicanos.")
} else {
  print("Hay encuestados que no son mexicanos")
  otros_valores <- unique(datosVigilancia[datosVigilancia[,5] != "es-MX"])
  print(paste("Otros valores encontrados:", otros_valores))
}

if (all(datosVigilancia[,11] == "Sí")) {
  print("Todos los encuestados son mayores de 18 años.")
} else {
  print("Hay encuestados que son menores de 18 años")
  otros_valores <- unique(datosVigilancia[datosVigilancia[,11] != "Sí"])
  print(paste("Otros valores encontrados:", otros_valores))
}

### COLUMNAS IMPORTANTES ###
if (all(datosVigilancia[,14] == "Hombre" | datosVigilancia[,14] == "Mujer")) {
  print("Todos los encuestados son hombres o mujeres")
} else {
  print("Hay encuestados dijeron algo diferente")
  otros_indices <- which(!(datosVigilancia[, 14] %in% c("Hombre", "Mujer")))
  otros_valores_14 <- datosVigilancia[otros_indices, 14]
  otros_valores_15 <- datosVigilancia[otros_indices, 15]
  for (i in 1:length(otros_indices)) {
    print(paste("Respondio: '", otros_valores_14[i], "', posteriormente dijo: '", otros_valores_15[i],"'" ))
  }
}

### VERIFICACIÓN ###
cat("Los estados de la republica visitados fueron", ":\n"); unique(datosVigilancia[,12])
cat("Las edades registradas son", ":\n"); unique(datosVigilancia[,13])
cat("Las carrereas registradas son", ":\n"); unique(datosVigilancia[,16])

### MODIFICACIONES Y EXTRACCIÓN ###
datosVigilancia[, 13] <- replace(datosVigilancia[, 13], is.na(datosVigilancia[, 13]), 0)
cat("Las edades registradas son", ":\n"); unique(datosVigilancia[,13])
datosVigilancia <- datosVigilancia[,c(12,13,14,16)]
for (i in 1:length(datosVigilancia$lugar)) {
  if (datosVigilancia$lugar[i] == "Estado de México (Toluca o Valle de México)") {
    datosVigilancia$lugar[i] <- "Toluca y Valle de México, Estado de México"
  }
}

library(stringr)
division <- str_split(datosVigilancia[,1], ", ")
datos_df <- as.data.frame(do.call(rbind, division))
colnames(datos_df) <- c("Municipio", "Estado")
datosVigilancia <- cbind(datos_df, datosVigilancia[, 2:4])
datos <- cbind(datosDH, datosVigilancia)
datos <- datos[,-29]; dim(datos)

names(datosReg)
############################################################################################
########## MODELO DE REGRESIÓN ##########
datosFila <- rowSums(datos[,1:28])
datosReg <- cbind(datosFila, datos[,29:32])

library(MASS)
poiss <- glm(datosFila ~ Estado + edad + sexo + escolaridad, data = datosReg, family = poisson())
poiss <- glm(datosFila ~ Estado + edad + sexo + escolaridad, family = poisson(link = "log"), datosReg)
poiss <- glm(datosFila ~ Estado + edad + sexo + escolaridad, family = quasipoisson(link = "log"), datosReg)

summary(poiss)


binNeg <- glm.nb(datosFila ~ Estado + edad + sexo + escolaridad, data = datosReg)
summary(binNeg)




data(discoveries)
disc <- data.frame(count = as.numeric(discoveries),
                   year = seq(0, (length(discoveries) - 1)))

############################################################################################
########## ESTADO CONTRA DERECHOS HUMANOS ##########
particionI <- datos[,-c(30,31,32)]
valoresEstado <- unique(particionI[, 29])
datosEstado <- matrix(numeric(), length(valoresEstado), 28)
for (i in valoresEstado){
  sumaColumna <- colSums(particionI[particionI[, 29] == i, 1:28])
  datosEstado[which(valoresEstado == i),] <- sumaColumna
}
rownames(datosEstado) <- valoresEstado
acomodo <- as.matrix(names(sort(rowSums(datosEstado[,1:12]), decreasing = TRUE)))
nombresAcomodo <- rep(acomodo, each= 12)

nombresDDHH1 <- c("Vida", "Libertad", "Igualdad", "Seguridad", "Nacionalidad", "Familia", "Circular", "Reconocimiento", "Sufragio", "Ser electo", "Privacidad", "Protección DP")
nombresDDHH2 <- c("Trabajo", "Sindicato", "Seguridad Social", "Salud", "Educación", "Alimentos", "Viviendo", "Seguridad pública", "Vida Digna")
nombresDDHH3 <- c("Libre Determinación", "Migración", "Medio Ambiente", "Desarrollo")
nombresDDHH4 <- c("Informática", "Seguridad Digital", "Información")
nombresDDHH <- c(nombresDDHH1, nombresDDHH2, nombresDDHH3, nombresDDHH4)
colnames(datosEstado) <- nombresDDHH
colores <- c(c("#2300bb", "#9100a7", "#bd00d9", "#d7008c", "#ff00a6", "#e25bc7", "#9C5CBD", "#B96EDF", "#ffb0b0", "#EC95D3", "#f8daf2", "#dadada"), 
             c("#003c2e", "#007e61", "#009931", "#0ff358", "#baff00", "#5eb203", "#00af81", "#89ffcf", "#e0fbdb"),
             c("#0000ff", "#007fff", "#33ffff", "#7ec8ee"),
             c("#c10000", "#f34f06", "#f68001"))
             
library(dplyr)
library(reshape2)
datosEstadoDDHH <- melt(datosEstado)
colnames(datosEstadoDDHH) <- c("Estado", "Derecho_Humano", "Frecuencia")
datosEstadoDDHH$Derecho_Humano <- factor(datosEstadoDDHH$Derecho_Humano, levels = nombresDDHH)
datosEstadoDDHH <- arrange(datosEstadoDDHH, Estado)
datosEstadoDDHH <- as.data.frame(datosEstadoDDHH)

library(ggplot2)
ggplot(datosEstadoDDHH, aes(x = Estado, y = Frecuencia, fill = factor(Derecho_Humano, levels = nombresDDHH))) +
  geom_bar(stat = "identity") + scale_fill_manual(name = "Derechos Humanos", values = colores, labels = nombresDDHH) +  
  coord_flip() + labs(x = "Estado", y = "Frecuencia", title = "Derecho humano más significativo en cada estado") +
  theme_minimal() + theme(legend.position = "right")



################# GRÁFICO SEPARADO ############################
datosEstadoDH1 <- datosEstado[,1:12] 
datosEstadoDH2 <- datosEstado[,13:21]
datosEstadoDH3 <- datosEstado[,22:25]
datosEstadoDH4 <- datosEstado[,26:28]

#################################
datosEstadoDH1 <- melt(datosEstadoDH1)
colnames(datosEstadoDH1) <- c("Estado", "Derecho_Humano", "Frecuencia")
datosEstadoDH1$Derecho_Humano <- factor(datosEstadoDH1$Derecho_Humano, levels = nombresDDHH[1:12])
datosEstadoDH1 <- arrange(datosEstadoDH1, Estado)
datosEstadoDH1 <- as.data.frame(datosEstadoDH1)
datosEstadoDH1 <- datosEstadoDH1 %>% arrange(Estado, Frecuencia)
colores1 <- c(rep(c(rev(colores[1:12]), rev(colores[1:12])), 10), rev(colores[1:12]))
datosEstadoDH1 <- cbind(datosEstadoDH1, colores1)
datosEstadoDH1 <- datosEstadoDH1 %>%  arrange(factor(Estado, levels = unique(nombresAcomodo)))


###
datosEstadoDH2 <- melt(datosEstadoDH2)
colnames(datosEstadoDH2) <- c("Estado", "Derecho_Humano", "Frecuencia")
datosEstadoDH2$Derecho_Humano <- factor(datosEstadoDH2$Derecho_Humano, levels = nombresDDHH[13:21])
datosEstadoDH2 <- arrange(datosEstadoDH2, Estado)
datosEstadoDH2 <- as.data.frame(datosEstadoDH2)
datosEstadoDH2 <- datosEstadoDH2 %>% arrange(Estado, Frecuencia)
colores2 <- c(rep(c(rev(colores[13:21]), rev(colores[13:21])), 10), rev(colores[13:21]))
datosEstadoDH2 <- cbind(datosEstadoDH2, colores2)
datosEstadoDH2 <- datosEstadoDH2 %>% arrange(factor(Estado, levels = unique(nombresAcomodo)))

###
datosEstadoDH3 <- melt(datosEstadoDH3)
colnames(datosEstadoDH3) <- c("Estado", "Derecho_Humano", "Frecuencia")
datosEstadoDH3$Derecho_Humano <- factor(datosEstadoDH3$Derecho_Humano, levels = nombresDDHH[22:25])
datosEstadoDH3 <- arrange(datosEstadoDH3, Estado)
datosEstadoDH3 <- as.data.frame(datosEstadoDH3)
colores3 <- c(rep(c(rev(colores[22:25]), rev(colores[22:25])), 10), rev(colores[22:25]))
datosEstadoDH3 <- cbind(datosEstadoDH3, colores3)
datosEstadoDH3 <- datosEstadoDH3 %>% arrange(factor(Estado, levels = unique(nombresAcomodo)))

###
datosEstadoDH4 <- melt(datosEstadoDH4)
colnames(datosEstadoDH4) <- c("Estado", "Derecho_Humano", "Frecuencia")
datosEstadoDH4$Derecho_Humano <- factor(datosEstadoDH4$Derecho_Humano, levels = nombresDDHH[26:28])
datosEstadoDH4 <- arrange(datosEstadoDH4, Estado)
datosEstadoDH4 <- as.data.frame(datosEstadoDH4)
colores4 <- c(rep(c(rev(colores[26:28]), rev(colores[26:28])), 10), rev(colores[26:28]))
datosEstadoDH4 <- cbind(datosEstadoDH4, colores4)
datosEstadoDH4 <- datosEstadoDH4 %>% arrange(factor(Estado, levels = unique(nombresAcomodo)))

datosEstadoDH1$Estado <- factor(datosEstadoDH1$Estado, levels = unique(datosEstadoDH1$Estado))
p1 <- ggplot(datosEstadoDH1, aes(x = Frecuencia, y = Estado, fill = factor(Derecho_Humano, levels = unique(Derecho_Humano)))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = datosEstadoDH1$colores1) + 
  labs(x = "", y = "") +
  theme_minimal() +
  ggtitle("Generación I DDHH") +
  theme(legend.position = "none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(size=12), 
        axis.ticks.y=element_blank()) +
  scale_y_discrete(labels = acomodo) 

datosEstadoDH2$Estado <- factor(datosEstadoDH2$Estado, levels = unique(datosEstadoDH2$Estado))
p2 <- ggplot(datosEstadoDH2, aes(x = Frecuencia, y = Estado, fill = factor(Derecho_Humano, levels = unique(Derecho_Humano)))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = datosEstadoDH2$colores2) + 
  labs(x = "", y = "") +
  theme_minimal() +
  ggtitle("Generación II DDHH") +
  theme(legend.position = "none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(), 
        axis.ticks.y=element_blank()) +
  scale_y_discrete(labels = NULL) 

datosEstadoDH3$Estado <- factor(datosEstadoDH3$Estado, levels = unique(datosEstadoDH3$Estado))
p3 <- ggplot(datosEstadoDH3, aes(x = Frecuencia, y = Estado, fill = factor(Derecho_Humano, levels = unique(Derecho_Humano)))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = datosEstadoDH3$colores3) + 
  labs(x = "", y = "") +
  theme_minimal() +
  ggtitle("Generación III DDHH") +
  theme(legend.position = "none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(), 
        axis.ticks.y=element_blank()) +
  scale_y_discrete(labels = NULL) 

datosEstadoDH4$Estado <- factor(datosEstadoDH4$Estado, levels = unique(datosEstadoDH4$Estado))
p4 <- ggplot(datosEstadoDH4, aes(x = Frecuencia, y = Estado, fill = factor(Derecho_Humano, levels = unique(Derecho_Humano)))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = datosEstadoDH4$colores4) + 
  labs(x = "", y = "") +
  theme_minimal() +
  ggtitle("Generación VI DDHH") +
  theme(legend.position = "none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  scale_y_discrete(labels = NULL)  

library(gridExtra)
library(grid)
anchoP1 <- 0.35;  anchoP2 <- 0.35;  anchoP3 <- 0.18;  anchoP4 <- 0.12
grid.arrange(p1, p2, p3, p4, nrow = 1, widths = c(anchoP1, anchoP2, anchoP3, anchoP4))
grid.lines(x = c(anchoP1, anchoP1), y = c(0, 1), gp = gpar(col = "red", lty = 2, lwd = 1))
grid.lines(x = c(anchoP1 + anchoP2, anchoP1 + anchoP2), y = c(0, 1), gp = gpar(col = "red", lty = 2, lwd = 1))
grid.lines(x = c(anchoP1 + anchoP2 + anchoP3, anchoP1 + anchoP2 + anchoP3), y = c(0, 1), gp = gpar(col = "red", lty = 2, lwd = 1))


#### MEJOR ####
coloresMejor <- c(rep(c(colores, colores), 20/2), colores)
datosEstadoDDHH <- cbind(datosEstadoDDHH,coloresMejor)
datosEstadoDDHH <- datosEstadoDDHH %>% arrange(Estado, desc(Frecuencia))
mejoresDDHH <- datosEstadoDDHH %>%   group_by(Estado) %>%  slice(1)
mejoresDDHH <- as.data.frame(mejoresDDHH)

colores <- setNames(mejoresDDHH$coloresMejor, mejoresDDHH$Derecho_Humano)
ggplot(mejoresDDHH, aes(x = Estado, y = Frecuencia, fill = Derecho_Humano)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = colores) +
  labs(title = "Frecuencia de Derechos Humanos por Estado", x = "Estado", y = "Frecuencia") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


############################################################################################
########## CARRERA CONTRA DERECHOS HUMANOS ##########
