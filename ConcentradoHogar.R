
#                                   Descripción de los datos                                  

# Cargar el conjunto de datos

dataset <- read.csv("concentradohogar.csv")
dataset <- dataset[, c("tot_integ","ubica_geo","hombres","mujeres","verduras", "frutas","tabaco")]

general = dim(as.data.frame(dataset))

print(paste("Este conjunto de datos contiene", general[1], "referencias y ", general[2], "variables" ))


str(dataset)
dataset[1] 
summary(dataset)

"""--------------------------------------- Integrantes --------------------------------------"""

# Frecuencia absoluta
fabsInt <- table(dataset$tot_integ)
fabsInt <- as.data.frame(fabsInt)
fabsInt

Fcum <- cumsum(table(dataset$tot_integ))
Fcum <- as.data.frame(Fcum, )
Fcum
barplot(Fcum, main = "Frecuencia Acumulada", xlab = "Integrantes /p Family", ylab = "Conteo", col = c("aquamarine4"))

# Frecuencia relativa
frel <-round(prop.table(table(dataset$tot_integ)),3)
frel

round(cumsum(prop.table(table(dataset$tot_integ))),3)
# Moda
moda <- as.numeric(names(which(table(dataset$tot_integ)== max(table(dataset$tot_integ)))))
print(paste("Esta es la moda: ", moda))

# Media
mean(dataset$tot_integ)

# Cuantile 
  # Primer cuantil
quantile(dataset$tot_integ,0.25)
  # Segundo ciantil
quantile(dataset$tot_integ,0.5)
  # Tercer cuantil
quantile(dataset$tot_integ,0.75)

# Rango Intercuartilico
IQR(dataset$tot_integ)

# Varianza muestral
var(dataset$tot_integ)

# Varianza verdadera
n = length(dataset$tot_integ)
var(dataset$tot_integ)*(n-1)/n

# Desvianción estandar muestral
sd(dataset$tot_integ)

# Desviación estandar verdadera
sd(dataset$tot_integ)*sqrt((n-1)/n)



str(dataset$tot_integ)

summary(dataset$tot_integ)

boxplot(dataset$tot_integ, outline = F,main= "Número de integrantes en las familias",
        ylim = c(1,10), ylab="Personas", col = c("chocolate4"))

# Load ggplot2
library(ggplot2)

names(fabsInt) = c("Integrantes","Frec")

ggplot(fabsInt, aes(x=Integrantes, y=Frec)) + 
  geom_bar(stat = "identity", show.legend = T, colour="coral4", fill="aquamarine4") +
  ggtitle("Histograma de Integrantes") +
  xlab("Integrantes /p Familia") + ylab("Conteo")
fabsInt

""" ----------------------------------- Tabaco --------------------------------------------- """

# Frecuencia absoluta
fabsTab <- table(dataset$tabaco)
fabsTab <- as.data.frame(fabsTab)

cumsum(table(dataset$tabaco))

# Frecuencia relativa
frel <-round(prop.table(table(dataset$tabaco)),3)
frel

round(cumsum(prop.table(table(dataset$tabaco))),3)
# Moda
moda <- as.numeric(names(which(table(dataset$tabaco)== max(table(dataset$tabaco)))))
print(paste("Esta es la moda: ", moda))

# Media
mean(dataset$tabaco)

# Cuantile 
# Primer cuantil
quantile(dataset$tabaco,0.25)
# Segundo ciantil
quantile(dataset$tabaco,0.5)
# Tercer cuantil
quantile(dataset$tabaco,0.75)

# Rango Intercuartilico
IQR(dataset$tabaco)

# Varianza muestral
var(dataset$tabaco)

# Varianza verdadera
n = length(dataset$tabaco)
var(dataset$tabaco)*(n-1)/n

# Desvianción estandar muestral
sd(dataset$tabaco)

# Desviación estandar verdadera
sd(dataset$tabaco)*sqrt((n-1)/n)



str(dataset$tabaco)

summary(dataset$tabaco)
dataset$tabaco
#------------------------------------------ Hacer intervalos
boxplot(dataset$tabaco, outline = F,main= "Gasto en Tabaco",
        ylim = c(1,10), ylab="Personas", col = c("chocolate4"))

# Load ggplot2
library(ggplot2)

names(fabsTab) = c("Integrantes","Frec")

ggplot(fabsTab, aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity", show.legend = T, colour="coral4", fill="aquamarine4") +
  ggtitle("Histograma de Integrantes") +
  xlab("Integrantes /p Familia") + ylab("Conteo")


"""-----------------------------------------  Verduras -------------------------------------- """

# Frecuencia absoluta
fabsVer <- table(dataset$verduras)
fabsVer <- as.data.frame(fabsVer)

cumsum(table(dataset$verduras))

# Frecuencia relativa
frel <-round(prop.table(table(dataset$verduras)),3)
frel

round(cumsum(prop.table(table(dataset$verduras))),3)
# Moda
moda <- as.numeric(names(which(table(dataset$verduras)== max(table(dataset$verduras)))))
print(paste("Esta es la moda: ", moda))

# Media
mean(dataset$verduras)

# Cuantile 
# Primer cuantil
quantile(dataset$verduras,0.25)
# Segundo ciantil
quantile(dataset$verduras,0.5)
# Tercer cuantil
quantile(dataset$verduras,0.75)

# Rango Intercuartilico
IQR(dataset$verduras)

# Varianza muestral
var(dataset$verduras)

# Varianza verdadera
n = length(dataset$verduras)
var(dataset$verduras)*(n-1)/n

# Desvianción estandar muestral
sd(dataset$verduras)

# Desviación estandar verdadera
sd(dataset$verduras)*sqrt((n-1)/n)



str(dataset$verduras)

summary(dataset$verduras)
dataset$verduras
#------------------------------------------ Hacer intervalos
boxplot(dataset$verduras, outline = F,main= "Gasto en Tabaco",
        ylim = c(1,10), ylab="Personas", col = c("chocolate4"))

# Load ggplot2
library(ggplot2)


ggplot(fabsVar, aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity", show.legend = T, colour="coral4", fill="aquamarine4") +
  ggtitle("Histograma de Integrantes") +
  xlab("Integrantes /p Familia") + ylab("Conteo")

"""------------------------------------------- Frutas --------------------------------------- """
# Frecuencia absoluta
fabsFru <- table(dataset$frutas)
fabsFru <- as.data.frame(fabsFru)

cumsum(table(dataset$frutas))

# Frecuencia relativa
frel <-round(prop.table(table(dataset$frutas)),3)
frel

round(cumsum(prop.table(table(dataset$frutas))),3)
# Moda
moda <- as.numeric(names(which(table(dataset$frutas)== max(table(dataset$frutas)))))
print(paste("Esta es la moda: ", moda))

# Media
mean(dataset$frutas)

# Cuantile 
# Primer cuantil
quantile(dataset$frutas,0.25)
# Segundo ciantil
quantile(dataset$frutas,0.5)
# Tercer cuantil
quantile(dataset$frutas,0.75)

# Rango Intercuartilico
IQR(dataset$frutas)

# Varianza muestral
var(dataset$frutas)

# Varianza verdadera
n = length(dataset$frutas)
var(dataset$frutas)*(n-1)/n

# Desvianción estandar muestral
sd(dataset$frutas)

# Desviación estandar verdadera
sd(dataset$frutas)*sqrt((n-1)/n)



str(dataset$frutas)

summary(dataset$frutas)
dataset$frutas
#------------------------------------------ Hacer intervalos
boxplot(dataset$frutas, outline = F,main= "Gasto en Tabaco",
        ylim = c(1,10), ylab="Personas", col = c("chocolate4"))

# Load ggplot2
library(ggplot2)


ggplot(fabsVar, aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity", show.legend = T, colour="coral4", fill="aquamarine4") +
  ggtitle("Histograma de Integrantes") +
  xlab("Integrantes /p Familia") + ylab("Conteo")

#---------------------------------------------------------------------------------------------

""" Analisis por subconjunto """

# Subset
integrantes = as.data.frame(fabsInt)
integrantes[,1]

for (i in integrantes[,1]) {
  resumen = summary(subset(dataset, tot_integ == i, c("verduras","frutas", "tabaco")))
  print(resumen)
}




"""---------------------------------------------------------------------------------------"""
# libraries:
library(ggplot2)
library(gganimate)

# Make 2 basic states and concatenate them:
a <- data.frame(group=c("A","B","C"), values=c(3,2,4), frame=rep('a',3))
b <- data.frame(group=c("A","B","C"), values=c(5,3,7), frame=rep('b',3))
data <- rbind(a,b)  

# Basic barplot:
ggplot(a, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity')

# Make a ggplot, but add frame=year: one image per year
ggplot(data, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity') +
  theme_bw() +
  # gganimate specific bits:
  transition_states(
    frame,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out')

# Save at gif:
anim_save("288-animated-barplot-transition.gif")
