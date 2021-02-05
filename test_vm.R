# test vector machine
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(e1071)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(ISLR)))
suppressMessages(suppressWarnings(library(corrplot)))
suppressMessages(suppressWarnings(library(class)))
suppressMessages(suppressWarnings(library(caret)))
#suppressMessages(suppressWarnings(library(randomForest)))
#suppressMessages(suppressWarnings(library(caTools)))

data_frame <- read.csv("./Dataset.csv", header = TRUE, stringsAsFactors = FALSE)

clean_data <- data_frame[, -c(1, 2, 3, 6, 7, 8, 17)]
#nan
clean_data <- na.omit(data_frame)
clean_data$FraudFound_P <- factor(ifelse(clean_data$FraudFound_P == 1, "Yes", "No"))
#dimension reduction(zero variance)
zero_features <- nearZeroVar(clean_data)
clean_data <- clean_data[, -zero_features]
#numeric and correlation
numeric_data <- clean_data[sapply(clean_data, is.numeric)]
cor_data <- cor(numeric_data)
# Print correlation matrix and look at max correlation
#print(cor_data)
#summary(cor_data[upper.tri(cor_data)])
# Check Correlation Plot
#corrplot(cor_data, order = "FPC", method = "color", type = "lower",
#            tl.cex = 0.7, tl.col = rgb(0, 0, 0))
# find attributes that are highly corrected
highly_cor <- findCorrelation(cor_data, cutoff = 0.7)
# print indexes of highly correlated attributes
#print(highly_cor)
# Indentifying Variable Names of Highly Correlated Variables
highly_cor_col <- colnames(numeric_data)[highly_cor]
# Print highly correlated attributes
#print(highly_cor_col)
# Remove highly correlated variables and create a new dataset
clean_data <- clean_data[, -which(colnames(clean_data) %in% highly_cor_col)]
#normalize
predic_data <- clean_data[, -match("FraudFound_P", names(clean_data))]
nor_data <- preProcess(predic_data, method = c("range"))
#nor_data <- preProcess(clean_data, method = c("range"))
nor_data <- predict(nor_data, predic_data)
#dumify
dummy_data <- dummyVars("~ .", data = nor_data)
dummy_data <- data.frame(predict(dummy_data, newdata = nor_data))
dummy_data$FraudFound_P <- unlist(clean_data["FraudFound_P"])
#dummy_data$FraudFound_P <- clean_data["FraudFound_P"]

#spliting data
set.seed(200)
ran <- sort(sample(1:nrow(dummy_data), 0.8 * nrow(dummy_data)))
train_data <- dummy_data[ran, ]
test_data <- dummy_data[-ran, ]
target_category_data <- clean_data["FraudFound_P"][ran, ]
test_category_data <- clean_data["FraudFound_P"][-ran, ]

# tuning model
# tune_rad <- tune(svm, FraudFound_P~., data = train_data,
#                 kernel = "linear",
#                 ranges = list(
#                             cost = c(0.1, 1, 10, 100, 1000))
#                 )


model <- svm(FraudFound_P~.,  data = train_data,
            kernel = "linear",
            cost = 1,
            )

mc <- table(true = test_category_data,
            pred = predict(model, newdata = test_data))
print(mc)

round(sum(diag(mc))/sum(colSums(mc)), 5)

rs <- apply(mc, 1, sum)
r1 <- round(mc[1, ] / rs[1], 5)
r2 <- round(mc[2, ] / rs[2], 5)
print(rbind(No = r1, Yes = r2))

###############
fit <- svm(FraudFound_P ~ ., data = train_data ,
            kernel = "linear",
            cost = 1,
            decision.values = TRUE)

fitted <- attributes(predict(fit, test_data, decision.values = TRUE))$decision.values

####
eti <- ifelse(fitted < 0, "Yes", "No")

mc <- table(true = test_category_data, pred = eti)
print(mc)

print(round(sum(diag(mc))/sum(colSums(mc)), 5))

rs <- apply(mc, 1, sum)
r1 <- round(mc[1, ] / rs[1], 5)
r2 <- round(mc[2, ] / rs[2], 5)
print(rbind(No = r1, Yes = r2))

#####
eti <- ifelse(fitted < 1.002, "Yes", "No")

mc <- table(true = test_category_data,
            pred = eti)
print(mc)

print(round(sum(diag(mc))/sum(colSums(mc)), 5))

rs <- apply(mc, 1, sum)
r1 <- round(mc[1, ] / rs[1], 5)
r2 <- round(mc[2, ] / rs[2], 5)
print(rbind(No = r1, Yes = r2))