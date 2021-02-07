###########################################################-
# Objective: Detección Fraude en Seguros
# Author: Team 11
# Date Modified: 06/02/2021
###########################################################-

#Fijar directorio

setwd("C:/Users/Gabo/Desktop/Becas Santander- Data Science/2.Fase2/2. Estadistica y Programacion con R/Proyecto")

# Desactivar notacion cientifica
options(scipen = 999)

#Cargar librerias----

library(readxl)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(DMwR)
library(C50)
library(kernlab)
library(AppliedPredictiveModeling)
library(lattice)
library(caret)
library(e1071)
library(gridExtra)
library(pROC)
library(ROCR)
library(nnet)

# LECTURA Y RECODIFICACIÓN DE VARIABLES ----
################################################-
# - Lectura de datos ----
fraud = as.data.frame(read_xlsx("Dataset.xlsx"))
head(fraud)

# - Principales datos----
names(fraud)
str(fraud)
dim(fraud)

# - Cambiar variables ordinales a enteros----

fraud$Month[fraud$Month == "Jan"] <- 1
fraud$Month[fraud$Month == "Feb"] <- 2
fraud$Month[fraud$Month == "Mar"] <- 3
fraud$Month[fraud$Month == "Apr"] <- 4
fraud$Month[fraud$Month == "May"] <- 5
fraud$Month[fraud$Month == "Jun"] <- 6
fraud$Month[fraud$Month == "Jul"] <- 7
fraud$Month[fraud$Month == "Aug"] <- 8
fraud$Month[fraud$Month == "Sep"] <- 9
fraud$Month[fraud$Month == "Oct"] <- 10
fraud$Month[fraud$Month == "Nov"] <- 11
fraud$Month[fraud$Month == "Dec"] <- 12
fraud$DayOfWeek[fraud$DayOfWeek == "Monday"] <- 1
fraud$DayOfWeek[fraud$DayOfWeek == "Tuesday"] <- 2
fraud$DayOfWeek[fraud$DayOfWeek == "Wednesday"] <- 3
fraud$DayOfWeek[fraud$DayOfWeek == "Thursday"] <- 4
fraud$DayOfWeek[fraud$DayOfWeek == "Friday"] <- 5
fraud$DayOfWeek[fraud$DayOfWeek == "Saturday"] <- 6
fraud$DayOfWeek[fraud$DayOfWeek == "Sunday"] <- 7
fraud$DayOfWeekClaimed[fraud$DayOfWeekClaimed == "Monday"] <- 1
fraud$DayOfWeekClaimed[fraud$DayOfWeekClaimed == "Tuesday"] <- 2
fraud$DayOfWeekClaimed[fraud$DayOfWeekClaimed == "Wednesday"] <- 3
fraud$DayOfWeekClaimed[fraud$DayOfWeekClaimed == "Thursday"] <- 4
fraud$DayOfWeekClaimed[fraud$DayOfWeekClaimed == "Friday"] <- 5
fraud$DayOfWeekClaimed[fraud$DayOfWeekClaimed == "Saturday"] <- 6
fraud$DayOfWeekClaimed[fraud$DayOfWeekClaimed == "Sunday"] <- 7
fraud$MonthClaimed[fraud$MonthClaimed == "Jan"] <- 1
fraud$MonthClaimed[fraud$MonthClaimed == "Feb"] <- 2
fraud$MonthClaimed[fraud$MonthClaimed == "Mar"] <- 3
fraud$MonthClaimed[fraud$MonthClaimed == "Apr"] <- 4
fraud$MonthClaimed[fraud$MonthClaimed == "May"] <- 5
fraud$MonthClaimed[fraud$MonthClaimed == "Jun"] <- 6
fraud$MonthClaimed[fraud$MonthClaimed == "Jul"] <- 7
fraud$MonthClaimed[fraud$MonthClaimed == "Aug"] <- 8
fraud$MonthClaimed[fraud$MonthClaimed == "Sep"] <- 9
fraud$MonthClaimed[fraud$MonthClaimed == "Oct"] <- 10
fraud$MonthClaimed[fraud$MonthClaimed == "Nov"] <- 11
fraud$MonthClaimed[fraud$MonthClaimed == "Dec"] <- 12
fraud$VehiclePrice[fraud$VehiclePrice == "less than 20000"] <- 1
fraud$VehiclePrice[fraud$VehiclePrice == "20000 to 29000"] <- 2
fraud$VehiclePrice[fraud$VehiclePrice == "30000 to 39000"] <- 3
fraud$VehiclePrice[fraud$VehiclePrice == "40000 to 59000"] <- 4
fraud$VehiclePrice[fraud$VehiclePrice == "60000 to 69000"] <- 5
fraud$VehiclePrice[fraud$VehiclePrice == "more than 69000"] <- 6
fraud$Days_Policy_Accident[fraud$Days_Policy_Accident == "none"] <- 0
fraud$Days_Policy_Accident[fraud$Days_Policy_Accident == "1 to 7"] <- 1
fraud$Days_Policy_Accident[fraud$Days_Policy_Accident == "8 to 15"] <- 2
fraud$Days_Policy_Accident[fraud$Days_Policy_Accident == "15 to 30"] <- 3
fraud$Days_Policy_Accident[fraud$Days_Policy_Accident == "more than 30"] <- 4
fraud$Days_Policy_Claim[fraud$Days_Policy_Claim == "none"] <- 0
fraud$Days_Policy_Claim[fraud$Days_Policy_Claim == "8 to 15"] <- 1
fraud$Days_Policy_Claim[fraud$Days_Policy_Claim == "15 to 30"] <- 2
fraud$Days_Policy_Claim[fraud$Days_Policy_Claim == "more than 30"] <- 3
fraud$Sex[fraud$Sex == "Female"] <- 0
fraud$Sex[fraud$Sex == "Male"] <- 1
fraud$MaritalStatus[fraud$MaritalStatus == "Single"] <- 1
fraud$MaritalStatus[fraud$MaritalStatus == "Married"] <- 2
fraud$MaritalStatus[fraud$MaritalStatus == "Widow"] <- 3
fraud$MaritalStatus[fraud$MaritalStatus == "Divorced"] <- 3
fraud$Fault[fraud$Fault == "Policy Holder"] <- 0
fraud$Fault[fraud$Fault == "Third Party"] <- 1
fraud$PolicyType[fraud$PolicyType == "Sedan - All Perils"] <- "SedA"
fraud$PolicyType[fraud$PolicyType == "Sedan - Collision"] <- "SedC"
fraud$PolicyType[fraud$PolicyType == "Sedan - Liability"] <- "SedL"
fraud$PolicyType[fraud$PolicyType == "Sport – All Perils"] <- "SpoAL"
fraud$PolicyType[fraud$PolicyType == "Sport - Liability"] <- "SpoAL"
fraud$PolicyType[fraud$PolicyType == "Sport - Collision"] <- "SpoC"
fraud$PolicyType[fraud$PolicyType == "Utility – All Perils"] <- "UtiA"
fraud$PolicyType[fraud$PolicyType == "Utility - Collision"] <- "UtiCL"
fraud$PolicyType[fraud$PolicyType == "Utility - Liability"] <- "UtiCL"
fraud$PastNumberOfClaims[fraud$PastNumberOfClaims == "none"] <- 0
fraud$PastNumberOfClaims[fraud$PastNumberOfClaims == "1"] <- 1
fraud$PastNumberOfClaims[fraud$PastNumberOfClaims == "2 to 4"] <- 2
fraud$PastNumberOfClaims[fraud$PastNumberOfClaims == "more than 4"] <- 3
fraud$AgeOfVehicle[fraud$AgeOfVehicle == "new"] <- 0
fraud$AgeOfVehicle[fraud$AgeOfVehicle == "2 years"] <- 2
fraud$AgeOfVehicle[fraud$AgeOfVehicle == "3 years"] <- 3
fraud$AgeOfVehicle[fraud$AgeOfVehicle == "4 years"] <- 4
fraud$AgeOfVehicle[fraud$AgeOfVehicle == "5 years"] <- 5
fraud$AgeOfVehicle[fraud$AgeOfVehicle == "6 years"] <- 6
fraud$AgeOfVehicle[fraud$AgeOfVehicle == "7 years"] <- 7
fraud$AgeOfVehicle[fraud$AgeOfVehicle == "more than 7"] <- 8
fraud$AgeOfPolicyHolder[fraud$AgeOfPolicyHolder == "16 to 17"] <- 1
fraud$AgeOfPolicyHolder[fraud$AgeOfPolicyHolder == "18 to 20"] <- 2
fraud$AgeOfPolicyHolder[fraud$AgeOfPolicyHolder == "21 to 25"] <- 3
fraud$AgeOfPolicyHolder[fraud$AgeOfPolicyHolder == "26 to 30"] <- 4
fraud$AgeOfPolicyHolder[fraud$AgeOfPolicyHolder == "31 to 35"] <- 5
fraud$AgeOfPolicyHolder[fraud$AgeOfPolicyHolder == "36 to 40"] <- 6
fraud$AgeOfPolicyHolder[fraud$AgeOfPolicyHolder == "41 to 50"] <- 7
fraud$AgeOfPolicyHolder[fraud$AgeOfPolicyHolder == "51 to 65"] <- 8
fraud$AgeOfPolicyHolder[fraud$AgeOfPolicyHolder == "over 65"] <- 9
fraud$PoliceReportFiled[fraud$PoliceReportFiled == "No"] <- 0
fraud$PoliceReportFiled[fraud$PoliceReportFiled == "Yes"] <- 1
fraud$WitnessPresent[fraud$WitnessPresent == "No"] <- 0
fraud$WitnessPresent[fraud$WitnessPresent == "Yes"] <- 1
fraud$AgentType[fraud$AgentType == "External"] <- 0
fraud$AgentType[fraud$AgentType == "Internal"] <- 1
fraud$NumberOfSuppliments[fraud$NumberOfSuppliments == "none"] <- 0
fraud$NumberOfSuppliments[fraud$NumberOfSuppliments == "1 to 2"] <- 1
fraud$NumberOfSuppliments[fraud$NumberOfSuppliments == "3 to 5"] <- 2
fraud$NumberOfSuppliments[fraud$NumberOfSuppliments == "more than 5"] <- 3
fraud$AddressChange_Claim[fraud$AddressChange_Claim == "no change"] <- 0
fraud$AddressChange_Claim[fraud$AddressChange_Claim == "under 6 months"] <- 1
fraud$AddressChange_Claim[fraud$AddressChange_Claim == "1 year"] <- 1
fraud$AddressChange_Claim[fraud$AddressChange_Claim == "2 to 3 years"] <- 2
fraud$AddressChange_Claim[fraud$AddressChange_Claim == "4 to 8 years"] <- 3
fraud$NumberOfCars[fraud$NumberOfCars == "1 vehicle"] <- 0
fraud$NumberOfCars[fraud$NumberOfCars == "2 vehicles"] <- 1
fraud$NumberOfCars[fraud$NumberOfCars == "3 to 4"] <- 2
fraud$NumberOfCars[fraud$NumberOfCars == "5 to 8"] <- 3
fraud$NumberOfCars[fraud$NumberOfCars == "more than 8"] <- 4
fraud$BasePolicy[fraud$BasePolicy == "All Perils"] <- "AP"
fraud$BasePolicy[fraud$BasePolicy == "Collision"] <- "C"
fraud$BasePolicy[fraud$BasePolicy == "Liability"] <- "L"

fraud$Month <- as.integer(fraud$Month)
fraud$DayOfWeek <- as.integer(fraud$DayOfWeek)
fraud$DayOfWeekClaimed <- as.integer(fraud$DayOfWeekClaimed)
fraud$MonthClaimed <- as.integer(fraud$MonthClaimed)
fraud$VehiclePrice <- as.integer(fraud$VehiclePrice)
fraud$Days_Policy_Accident <- as.integer(fraud$Days_Policy_Accident)
fraud$Days_Policy_Claim <- as.integer(fraud$Days_Policy_Claim)
fraud$Sex <- as.integer(fraud$Sex)
fraud$MaritalStatus <- as.integer(fraud$MaritalStatus)
fraud$Fault <- as.integer(fraud$Fault)
fraud$PastNumberOfClaims <- as.integer(fraud$PastNumberOfClaims)
fraud$AgeOfVehicle <- as.integer(fraud$AgeOfVehicle)
fraud$AgeOfPolicyHolder <- as.integer(fraud$AgeOfPolicyHolder)
fraud$PoliceReportFiled <- as.integer(fraud$PoliceReportFiled)
fraud$WitnessPresent <- as.integer(fraud$WitnessPresent)
fraud$AgentType <- as.integer(fraud$AgentType)
fraud$NumberOfSuppliments <- as.integer(fraud$NumberOfSuppliments)
fraud$AddressChange_Claim <- as.integer(fraud$AddressChange_Claim)
fraud$NumberOfCars <- as.integer(fraud$NumberOfCars)
fraud$WeekOfMonth <- as.integer(fraud$WeekOfMonth) # agregada
fraud$FraudFound_P <- as.factor(fraud$FraudFound_P) # agregada
fraud$FraudFound_P <- recode(fraud$FraudFound_P, "0" = "No",
                             "1" = "Si")
fraud$Make <- as.factor (fraud$Make) # agregada
fraud$AccidentArea <- as.factor(fraud$AccidentArea) # agregada
fraud$PolicyType <- as.factor(fraud$PolicyType) # agregada
fraud$VehicleCategory <- as.factor(fraud$VehicleCategory) # agregada
fraud$BasePolicy <- as.factor(fraud$BasePolicy) # agregada


# - Particionar datos en entrenamiento y prueba (50% para todos)----
trainingRows = createDataPartition(fraud$FraudFound_P, p = 0.50, 
                                   list = FALSE)
Fraud_trainX = fraud[trainingRows,-16]
Fraud_trainY = fraud[trainingRows,16]
Fraud_testX = fraud[-trainingRows,-16]
Fraud_testY = fraud[-trainingRows,16]
rm(fraud)
gc()

# PRE-PROCESAMIENTO ----
################################################-
# - Buscar variables con un solo valor----
FraudUnique = names(Filter(function(x){length(unique(x)) == 1}, 
                           Fraud_trainX))
if (length(FraudUnique) > 0) {
  sink(file = "Fraud_2_ResumenModeloFinal.txt", append = TRUE)
  cat("\nVariables eliminadas por tener un único valor:\n")
  cat(FraudUnique) 
  Fruad_train = Fraud_trainX[,-..pmUnique]
  sink()
}
rm(FraudUnique)

# - Eliminar predictores con un alto porcentaje de valores perdidos----
MissingObs = apply(Fraud_trainX, 2 ,
                   function(col) sum(is.na(col))/length(col))
sink(file = "Fraud_2_ResumenModeloFinal.txt", append = TRUE)
cat("\nPorcentaje de observaciones perdidas por variable, casos con %> 0%:\n")
VarsWithMissing = MissingObs[MissingObs > 0.0]
VarsWithMissing

# - Eliminar variables con más del 10% de valores perdidos----
cat("\nVariables que se eliminan por tener más de 10% de valores perdidos:\n")
MissingGT10pct = MissingObs[MissingObs > .10]
MissingNames = names(MissingGT10pct)
MissingNames
cat("\n")
sink()

Fraud_trainX = Fraud_trainX %>% dplyr::select(!names(MissingGT10pct))
if(length(MissingNames > 0))  Fraud_trainX = Fraud_trainX[,-..MissingNames]

# - Separar los predictores en: clase, enteros y númericos----

# - Lista de predictores de caracter----
ClassVariables <- names(which(sapply(Fraud_trainX, class) == 
                              c("factor")))
lchr = length(ClassVariables)
ClassVariables

# - Para los predictores de clase retener solo aquellos que no tienen NA's----
if (lchr > 0) {
  sink(file = "Fraud_2_ResumenModeloFinal.txt", append = TRUE)
  cat("\nSe eliminan los siguientes predictores de clase con al menos\n")
  cat("un valor perdido\n")
  Fraud_trainXC = Fraud_trainX %>% 
    dplyr::select(all_of(ClassVariables))
  Fraud_trainXC [Fraud_trainXC == ""] =  NA
  PctPerdidos = apply(Fraud_trainXC, 2, 
                      function(col){sum(is.na(col))/length(col)})
  cat(names(PctPerdidos))
  Fraud_trainXC = Fraud_trainXC[,PctPerdidos == 0]
  if(length(PctPerdidos) == lchr) lchr = 0
  sink()
}

# - Separar predictores enteros en Fraud_trainXI----
IntegerVariables <- names(which(sapply(Fraud_trainX, class) == c("integer")))
Fraud_trainXI = Fraud_trainX %>% dplyr::select(all_of(IntegerVariables))

# - Separar predictores numericos (no enteros) en Fraud_trainXN----
NumericVariables = names(which(sapply(Fraud_trainX, class) == c("numeric")))
Fraud_trainXN = Fraud_trainX %>% dplyr::select(all_of(NumericVariables))

# - Pre-procesamiento de predictores numéricos----

# - Eliminación de predictores con casi cero varianza----
CasiCeroVar = nearZeroVar(Fraud_trainXN, freqCut = 90/1, uniqueCut = 1)

if(length(CasiCeroVar) > 0){
  CasiCeroVarNames = colnames(Fraud_trainXN[,..CasiCeroVar])
  sink(file = "Fraud_2_ResumenModeloFinal.txt", append=TRUE)
  cat("\nSe eliminan los siguientes predictores por tener varianza")
  cat("\ncasi cero\n")
  CasiCeroVarNames
  Fraud_trainXN = Fraud_trainXN[, -..CasiCeroVar]
  sink()
}

# - Eliminar predictores con alta correlación----
TooHigh <- findCorrelation(cor(Fraud_trainXN, use = "pairwise.complete.obs"), 
                           cutoff = 0.90, exact = TRUE)
if (length(TooHigh) > 0){
  TooHighNames = colnames(Fraud_trainXN[,TooHigh])
  sink(file = "Fraud_2_ResumenModeloFinal.txt", append=TRUE)
  cat("\nSe eliminan los siguientes predictores por estar altamente")
  cat("\ncorrelacionados\n")
  TooHighNames
  Fraud_trainXN = Fraud_trainXN[, -TooHigh]
  sink()
}

# - Matriz de correlación----
corr <- cor(Fraud_trainXN)
ggcorrplot(corr,  ggtheme = ggplot2::theme_minimal)
rm(corr)

# - Re unir los predictores----

Fraud_trainX = cbind(Fraud_trainXC,Fraud_trainXN, Fraud_trainXI)

# - Validación Cruzada en k-capas con repetición----

indx <- createMultiFolds(Fraud_trainY, k = 5, times = 2)
str(indx)
ctrl <- trainControl(method = "repeatedcv",index = indx, 
                     summaryFunction = twoClassSummary,
                     sampling = "up",
                     classProbs = TRUE)

# - Balanceo de los datos de entrenamiento----
#FraudTr$VehiclePrice <- as.factor(FraudTr$VehiclePrice)
#Fraud_trainY <- as.factor(Fraud_trainY)
#fraud1 <- filter(FraudTr, FraudTr$FraudFound_P == 1)
#no_fraud <- filter(FraudTr, FraudTr$FraudFound_P == 0)
#SFraudTr <- SMOTE(FraudFound_P ~ ., FraudTr, perc.over = 600, perc.under = 100)
#rm(FraudTr)


# GRÁFICOS----
################################################-
# - Desbalanceo de datos ----

graficas <- cbind(Fraud_trainX, Fraud_trainY)
tail(graficas)

fraud1 <- graficas %>% filter(Fraud_trainY == "Si")
no_fraud <- graficas %>% filter(Fraud_trainY == "No")

ggplot(graficas,aes(Fraud_trainY))+geom_bar(fill=c("blue","red")) + scale_x_discrete(name = "Fraude",labels = c("No", "Si"))


# - Histograma de la edad ----


m1 <- round(mean(na.omit(fraud1$Age)), 2)
m2 <- round(mean(na.omit(no_fraud$Age)), 2)

p <- ggplot(fraud1, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), fill = "deepskyblue4", col = "black") +
  ggtitle("Histograma de Edad de Fraude", 
          subtitle = paste("Media", m1)) + 
  geom_density(aes(y = ..density..), col = "blue")+
  geom_vline(xintercept = m1, col = "red", lwd = 1.5, lty =2) + 
  xlab("Edad") + 
  ylab("Densidad") + 
  theme_bw() 

p2 <- ggplot(no_fraud, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), fill = "deepskyblue4", col = "black") +
  ggtitle("Histograma de Edad de No Fraude", 
          subtitle = paste("Media", m2)) + 
  geom_density(aes(y = ..density..), col = "blue")+
  geom_vline(xintercept = m2, col = "red", lwd = 1.5, lty =2) + 
  xlab("Edad") + 
  ylab("Densidad") + 
  theme_bw() 

grid.arrange(p, p2)

# - Histograma de suma reclamada----

m3 <- round(mean(na.omit(fraud1$ClaimSize)), 2)
m4 <- round(mean(na.omit(no_fraud$ClaimSize)), 2)

p3 <- ggplot(fraud1, aes(x = ClaimSize)) +
  geom_histogram(aes(y = ..density..), fill = "orange3", col = "black") +
  ggtitle("Histograma de Suma Asegurada Fraude", 
          subtitle = paste("Media", m3)) + 
  geom_density(aes(y = ..density..), col = "blue")+
  geom_vline(xintercept = m3, col = "black", lwd = 1.5, lty =2) + 
  xlab("Suma Asegurada") + 
  ylab("Densidad") + 
  xlim(0, 100000) + 
  theme_light() 

p4 <- ggplot(no_fraud, aes(x = ClaimSize)) +
  geom_histogram(aes(y = ..density..), fill = "orange3", col = "black") +
  ggtitle("Histograma de Suma Asegurada No Fraude", 
          subtitle = paste("Media", m4)) + 
  geom_density(aes(y = ..density..), col = "blue")+
  geom_vline(xintercept = m4, col = "black", lwd = 1.5, lty =2) + 
  xlab("Suma Asegurada") + 
  ylab("Densidad") + 
  xlim(0, 100000) +
  theme_light() 

grid.arrange(p3, p4)

# - Histograma monto del vehiculo----

p5 <- ggplot(fraud1, aes(x = VehiclePrice)) +
  geom_bar(fill = "deepskyblue4", col = "black") +
  ggtitle("Frecuencia Costo del Auto de Fraude") + 
  xlab("Costo del Auto") + 
  ylab("Frecuencia") + 
  theme_light() 

p6 <- ggplot(no_fraud, aes(x = VehiclePrice)) +
  geom_bar(fill = "deepskyblue4", col = "black") +
  ggtitle("Frecuencia Costo del Auto de Fraude") + 
  xlab("Costo del Auto") + 
  ylab("Frecuencia") + 
  theme_light() 

grid.arrange(p5, p6)

# - BoxPlot Sexo y Monto de Reclamación----

ggplot(graficas, aes(x = as.factor(Sex) , y = ClaimSize, fill = Fraud_trainY)) + geom_boxplot() +
  ggtitle("Boxplots") +
  scale_fill_discrete(name = "Fraude", labels = c("No", "Si")) +
  xlab("Sexo") +
  ylab("Reclamación")

# - BoxPlot Sexo y Edad----

ggplot(graficas, aes(x = as.factor(Sex) , y = Age, fill = Fraud_trainY)) + geom_boxplot() +
  ggtitle("Boxplots") +
  scale_fill_discrete(name = "Fraude", labels = c("No", "Si")) +
  xlab("Sexo") +
  ylab("Edad")

# - BoxPlot Area de Accidente----

ggplot(graficas, aes(x = AccidentArea , y = ClaimSize, fill = Fraud_trainY)) + geom_boxplot() +
  ggtitle("Boxplots") +
  scale_fill_discrete(name = "Fraude", labels = c("No", "Si")) +
  xlab("Área Accidente") +
  ylab("Reclamación")

# - BoxPlot por año de vehículo----
ggplot(graficas, aes(x = as.factor(AgeOfVehicle) , y = ClaimSize, fill = Fraud_trainY)) + geom_boxplot() +
  ggtitle("Boxplots") +
  scale_fill_discrete(name = "Fraude", labels = c("No", "Si")) +
  xlab("Año de Vehiculo") +
  ylab("Reclamación")

# MODELOS ----
################################################-
# - Eliminar la variable respuesta del data frame de entrenamiento----
#dropfraud <- names(SFraudTr) %in% c("FraudFound_P")

#levels(graficas$Fraud_trainY) <- c("No", "Yes")

# - Entrenamiento del modelo C5.0----

tiempoI <- Sys.time()
c50Grid = expand.grid(trials = c(1:13), model = c("rules","tree"),
                      winnow = c(FALSE, TRUE))

bc.C5.0 <- caret::train(x = Fraud_trainX, 
                        y = Fraud_trainY, 
                        tuneGrid = c50Grid, verbose = FALSE,
                        Preproc = c("BoxCox"),
                        method = "C5.0", metric = "ROC", 
                        trControl = ctrl)
tiempoF <- Sys.time()
print(paste("Tiempo entrenamiento modelo C5.0",
            difftime(tiempoF,tiempoI,units = "mins"),"minutos"))
saveRDS(bc.C5.0, file = "bc_C50.rds")
plot(bc.C5.0)
names(bc.C5.0)
bc.C5.0$bestTune
bc.C5.0

# - Entrenamiento del modelo Regresión Logística----
tiempoI <- Sys.time()
logisticReg = train(x = Fraud_trainX, 
                    y = Fraud_trainY, 
                    method = "multinom", preProc = c("center","scale"),
                    trControl = ctrl)
tiempoF <- Sys.time()
print(paste("Tiempo entrenamiento modelo de regresión logística",
            difftime(tiempoF,tiempoI,units = "mins"),"minutos"))
logisticReg
# - Entrenamiento del modelo Redes Neuronales----
nnetGrid <- expand.grid(size = 1:10, decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$size)

tiempoI = Sys.time()
nnetFit <- train(x = Fraud_trainX, 
                 y = Fraud_trainY, 
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 2000,
                 MaxNWts = 1*(maxSize * (length(Fraud_trainY) + 1) 
                              + maxSize + 1),
                 trControl = ctrl)
tiempoF = Sys.time()
print(paste("Tiempo para entrenar RN nnet: ",
            difftime(tiempoF,tiempoI,units = "mins"),"minutos"))
saveRDS(nnetFit, file = "nnetFit.rds")
plot(nnetFit)

# - Comparación de Modelos----
################################################-
# - Crear el objeto resamples de los modelos----
modelos = list( C5.0 = bc.C5.0,
                RegresionLogistica = logisticReg, 
                RedesNeuronales = nnetFit)
resamp = resamples(modelos)
summary(resamp)
names(resamp)
resamp$metrics

# - Visualizar los resultados----
xyplot(resamp, what = "scatter")
dotplot(resamp)
bwplot(resamp)

# Para evaluar las posibles diferencias entre los modelos utilizaMOS la función diff
modelDifferences = diff(resamp)
summary(modelDifferences)

modelDifferences$statistics$ROC

########################################################################-
# Dado que el C5.0 fué el mejor modelo bajo el criterio del áre bajo   
# la curva ROC, utilizamos la AUC para ver qué tan bien predice
########################################################################-

# - Importancia de variables del modelo final ----

ImportanciaVars = varImp(bc.C5.0)
ImportanciaVars
plot(ImportanciaVars, 
     main = paste("Importancia de Factores C5.0 Final",Sys.Date()))

# - Curva ROC del modelo ----

prediccion_C5Rules = predict(bc.C5.0, Fraud_testX, type="prob")

roc_obj_C5Rules <- roc(Fraud_testY, prediccion_C5Rules$Si)

roc_obj_C5Rules$auc

# - Plot the five ROC curves ----

Sens_C50 = roc_obj_C5Rules$sensitivities
Spec_C50 = roc_obj_C5Rules$specificities

par(mfrow=c(1,1))
plot(x=(1-Spec_C50), y=Sens_C50, type="l", lty="solid", col="black", 
     main=paste("ROC for models: C5.0 "), xlab="1-Specificity",
     ylab = "Sensitivity")
legend(x=.46, y=.5, paste("C5.0 Final, AUC = ", 
                          round(roc_obj_C5Rules$auc,digits=2)), 
       text.col = "black", bty="n")

# - Matriz de confusión ----

data_predicted <- predict(bc.C5.0, Fraud_testX)
head(Fraud_testY)
confusionMatrix(data = data_predicted, reference = Fraud_testY, positive = "Si")

