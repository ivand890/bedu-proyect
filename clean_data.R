
df <- read.csv("Dataset.csv", header = TRUE,stringsAsFactors = FALSE)


#delete features
fraud <- df[,-c(1,2,3,6,7,8,17)]


install.packages("fastDummies")

library(dplyr)


str(fraud)

dim(fraud)



#one hotencoding

library(fastDummies)
fraud2<-dummy_cols(fraud,  select_columns = c("Make", "MaritalStatus","BasePolicy","VehicleCategory","PolicyType")) %>% 
  select(-c("Make","MaritalStatus","BasePolicy","VehicleCategory","PolicyType"))
str(fraud2)
head(fraud2)



#normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


fraud2$ClaimSize <- normalize(fraud2$ClaimSize)



#char to int
fraud2$AccidentArea <- as.integer(as.factor(fraud2$AccidentArea))
fraud2$Sex <- as.integer(as.factor(fraud2$Sex))
fraud2$Fault <- as.integer(as.factor(fraud2$Fault))
fraud2$VehiclePrice <- as.integer(as.factor(fraud2$VehiclePrice))
fraud2$FraudFound_P <- (as.factor(fraud2$FraudFound_P))
fraud2$RepNumber <- as.integer(as.factor(fraud2$RepNumber))
fraud2$Deductible <- as.integer(as.factor(fraud2$Deductible))
fraud2$DriverRating <- as.integer(as.factor(fraud2$DriverRating))
fraud2$Days_Policy_Accident <- as.integer(as.factor(fraud2$Days_Policy_Accident))
fraud2$Days_Policy_Claim <- as.integer(as.factor(fraud2$Days_Policy_Claim))
fraud2$AgeOfVehicle <- as.integer(as.factor(fraud2$AgeOfVehicle))
fraud2$AgeOfPolicyHolder <- as.integer(as.factor(fraud2$AgeOfPolicyHolder))
fraud2$PoliceReportFiled <- as.integer(as.factor(fraud2$PoliceReportFiled))
fraud2$WitnessPresent <- as.integer(as.factor(fraud2$WitnessPresent))
fraud2$AgentType <- as.integer(as.factor(fraud2$AgentType))
fraud2$NumberOfCars <- as.integer(as.factor(fraud2$NumberOfCars))
fraud2$AddressChange_Claim <- as.integer(as.factor(fraud2$AddressChange_Claim))
fraud2$NumberOfSuppliments <- as.integer(as.factor(fraud2$NumberOfSuppliments))
fraud2$PastNumberOfClaims <- as.integer(as.factor(fraud2$PastNumberOfClaims))




#Manage null 
colSums(is.na(fraud2))

fraud2$Age[is.na(fraud2$Age)]<-0

colSums(is.na(fraud2))

fraud2 <- na.omit(fraud2)

colSums(is.na(fraud2))



#split train test
table(fraud2$FraudFound_P)

names(fraud2)<-gsub(" ","",names(fraud2))
names(fraud2)<-gsub("PolicyType_","",names(fraud2))
names(fraud2)<-gsub("-","",names(fraud2))
dt = sort(sample(nrow(fraud2), nrow(fraud2)*.7))
train<-fraud2[dt,]
test<-fraud2[-dt,]
names(train) == names(fraud2)
names(train) == names(test)
table(train$FraudFound_P)
table(test$FraudFound_P)

#imbalanced data

library(ggplot2)


ggplot(fraud2,aes(FraudFound_P))+geom_bar(fill=c("blue","red")) + scale_x_discrete(name = "Fraude",labels = c("No", "Si"))

prop.table(table(fraud2$FraudFound_P))



library(rpart)
library(readr)
library(ROSE)


#Undersampling and oversampling
data_balanced<- ovun.sample(FraudFound_P~., data = train, method = "both",p=0.5)$data


table(data_balanced$FraudFound_P)


#Model decision tree algorithm
model = rpart(FraudFound_P~.,data= data_balanced)
score = predict(model,newdata = test,type = "class")

#Metrics
confusionMatrix(score, test$FraudFound_P)


mc<-table(true=test$FraudFound_P,pred=score)
mc
round(sum(diag(mc))/sum(colSums(mc)), 5)
rs <- apply(mc, 1, sum)
r1 <- round(mc[1,]/rs[1], 5)
r2 <- round(mc[2,]/rs[2], 5)
rbind(No=r1, Yes=r2)


roc.curve(test$FraudFound, score)




