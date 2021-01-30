###########################################################-
# Objective: Detección Fraude en Seguros
# Author: Gabriel Sainz Vázquez
# Date Modified: 28/01/2021
###########################################################-

#Fijar directorio----

setwd("C:\\Users\\Gabo\\Downloads")

#Cargar librerias----

library(dplyr)
library(ggplot2)
library(AppliedPredictiveModeling)
library(lattice)
library(caret)
library(e1071)
library(corrplot)
library(readxl)
library(ggcorrplot)
library(gridExtra)

#Lectura de datos----

fraud = as.data.frame(read_xlsx("Dataset.xlsx"))
head(fraud)
names(fraud)
str(fraud)
dim(fraud)

# Convertir las variables en factores para un mejor procesamiento

fraud$Month <- as.factor(fraud$Month)
fraud$WeekOfMonth <- as.factor(fraud$WeekOfMonth) 
fraud$DayOfWeek <- as.factor(fraud$DayOfWeek)
fraud$Make <- as.factor(fraud$Make)
fraud$AccidentArea <- as.factor(fraud$AccidentArea)
fraud$DayOfWeekClaimed <- as.factor(fraud$DayOfWeekClaimed)
fraud$MonthClaimed <- as.factor(fraud$MonthClaimed)
fraud$WeekOfMonthClaimed <- as.factor(fraud$WeekOfMonthClaimed)
fraud$Sex <- as.factor(fraud$Sex)
fraud$MaritalStatus <- as.factor(fraud$MaritalStatus)
fraud$Fault <- as.factor(fraud$Fault)
fraud$PolicyType <- as.factor(fraud$PolicyType)
fraud$VehicleCategory <- as.factor(fraud$VehicleCategory)
fraud$VehiclePrice <- as.factor(fraud$VehiclePrice)
fraud$FraudFound_P <- as.factor(fraud$FraudFound_P)
fraud$RepNumber <- as.factor(fraud$RepNumber)
fraud$Deductible <- as.factor(fraud$Deductible)
fraud$DriverRating <- as.factor(fraud$DriverRating)
fraud$Days_Policy_Accident <- as.factor(fraud$Days_Policy_Accident)
fraud$Days_Policy_Claim <- as.factor(fraud$Days_Policy_Claim)
fraud$AgeOfVehicle <- as.factor(fraud$AgeOfVehicle)
fraud$AgeOfPolicyHolder <- as.factor(fraud$AgeOfPolicyHolder)
fraud$PoliceReportFiled <- as.factor(fraud$PoliceReportFiled)
fraud$WitnessPresent <- as.factor(fraud$WitnessPresent)
fraud$AgentType <- as.factor(fraud$AgentType)
fraud$NumberOfCars <- as.factor(fraud$NumberOfCars)
fraud$AddressChange_Claim <- as.factor(fraud$AddressChange_Claim)
fraud$BasePolicy <- as.factor(fraud$BasePolicy)
fraud$NumberOfSuppliments <- as.factor(fraud$NumberOfSuppliments)
fraud$PastNumberOfClaims <- as.factor(fraud$PastNumberOfClaims)

# Veamos qué valores tienen valores NA y veamos los campos que tiene el data frame

dim(fraud)
names(fraud)
head(fraud)

colSums(is.na(fraud)) # Note que la variable que mayor tiene NA es la de edad

a <- 0
for (i in 1:length(fraud$Age)){
  if (is.na(fraud$Age[i])){
    a <- c(a,i)      
  } 
}

fraud[a,]

# Proporción de fraudes en el data frame

table(fraud$FraudFound_P)

## Variables con Varianza cercana a Cero
# Para filtrar predictores con varianza cercana a cero, se utiliza nearZeroVar, 
# la cual es una función que nos regresa los números de columnas que tienen 
# casi cero varianza:

nzvar = nearZeroVar(fraud)
nzvar

# Como vemos, las columnas 19, 21, 22, 26, 27, 28, 30 y 31 tienen casi varianza 
# cero, estas variables son:

names(cbind(fraud[19], fraud[21], fraud[22], fraud[26], fraud[27], fraud[28], fraud[30], fraud[31]))

## Valores perdidos en la base de datos

# Ahora veremos cuantos valores perdidos se encuentran en el código y en donde 
# se encuentran

which(is.na(fraud))
sum(is.na(fraud))

# GRÁFICOS: Ver variables significativas (Estadística) ----

# Podemos observar que tenemos 11565 registros, donde 685 son fraude y 
# 10880 no son fraude

fraud1 <- filter(fraud, FraudFound_P == 1)
no_fraud <- filter(fraud, FraudFound_P == 0)

# Podemos analizar la edad de ambos casos (si es o no fraude) graficando histogramas
# de la distribución de la edad

m1 <- round(mean(na.omit(fraud1$Age)), 2)
m2 <- round(mean(na.omit(no_fraud$Age)), 2)

p <- ggplot(fraud1, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), fill = "blue", col = "black") +
  ggtitle("Histograma de edad de Fraude", 
          subtitle = paste("Media", m1)) + 
  geom_density(aes(y = ..density..), col = "red")+
  geom_vline(xintercept = m1, col = "red", lwd = 1.5, lty =2) + 
  xlab("Edad") + 
  ylab("Densidad") + 
  theme_light() 

p2 <- ggplot(no_fraud, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), fill = "blue", col = "black") +
  ggtitle("Histograma de edad de No Fraude", 
          subtitle = paste("Media", m2)) + 
  geom_density(aes(y = ..density..), col = "red")+
  geom_vline(xintercept = m2, col = "red", lwd = 1.5, lty =2) + 
  xlab("Edad") + 
  ylab("Densidad") + 
  theme_light() 

grid.arrange(p, p2)

# Gráfico de barras de los diferentes precios de los coches

p3 <- ggplot(fraud1, aes(x = VehiclePrice)) +
  geom_bar(fill = "blue", col = "black") +
  ggtitle("Frecuencia costo del auto de Fraude") + 
  xlab("Costo del Auto") + 
  ylab("Frecuencia") + 
  theme_light() 

p4 <- ggplot(no_fraud, aes(x = VehiclePrice)) +
  geom_bar(fill = "blue", col = "black") +
  ggtitle("Frecuencia costo del auto de NO Fraude") + 
  xlab("Costo del Auto") + 
  ylab("Frecuencia") + 
  theme_light() 

grid.arrange(p3, p4)

# Histograma de suma reclamada ----

m3 <- round(mean(na.omit(fraud1$ClaimSize)), 2)
m4 <- round(mean(na.omit(no_fraud$ClaimSize)), 2)

p5 <- ggplot(fraud1, aes(x = ClaimSize)) +
  geom_histogram(aes(y = ..density..), fill = "blue", col = "black") +
  ggtitle("Histograma de Suma Asegurada Fraude", 
          subtitle = paste("Media", m3)) + 
  geom_density(aes(y = ..density..), col = "red")+
  geom_vline(xintercept = m3, col = "red", lwd = 1.5, lty =2) + 
  xlab("Suma Asegurada") + 
  ylab("Densidad") + 
  xlim(0, 100000) + 
  theme_light() 

p6 <- ggplot(no_fraud, aes(x = ClaimSize)) +
  geom_histogram(aes(y = ..density..), fill = "blue", col = "black") +
  ggtitle("Histograma de Suma Asegurada No Fraude", 
          subtitle = paste("Media", m4)) + 
  geom_density(aes(y = ..density..), col = "red")+
  geom_vline(xintercept = m4, col = "red", lwd = 1.5, lty =2) + 
  xlab("Suma Asegurada") + 
  ylab("Densidad") + 
  xlim(0, 100000) +
  theme_light() 

grid.arrange(p5, p6)

#  edad, sexo, estado civil y precio de vehículo

ggplot(fraud, aes(x = Sex , y = ClaimSize, fill = FraudFound_P)) + geom_boxplot() +
  ggtitle("Boxplots") +
  scale_fill_discrete(name = "Fraude", labels = c("No", "Si"))+
  xlab("Sexo") +
  ylab("Reclamación")












