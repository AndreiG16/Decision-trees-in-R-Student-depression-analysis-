# Title: Decision Trees Model for Depression Dataset
# Author: Andrei Galca

# Load the dataf.csv file
dataf <- read.csv("/Users/andreig/Desktop/dataf.csv")

# Install necessary packages 
install.packages("rpart")
install.packages("rpart.plot")
install.packages("Metrics")
install.packages("C50")
install.packages("caret")
install.packages("pROC")

# Load required libraries
library(rpart)
library(rpart.plot)
library(Metrics)
library(C50)
library(caret)
library(pROC)

# Classification Tree for Depression
model2 <- rpart(Depression ~ Gender + Age + Academic.Pressure + Study.Satisfaction +
                  Work.Study.Hours + Financial.Stress + Sleep.Duration + Dietary.Habits, 
                method = "class", data = dataf, control = rpart.control(cp = 0.015), y = TRUE)

# Print and plot classification tree details
printcp(model2)
plotcp(model2)
rpart.plot(model2, digits = 4, extra = 1)

# Predictions and confusion matrix for Depression
a <- predict(model2, dataf, type = "class")
# Ensure both are factors with the same levels
a <- factor(a, levels = c("0", "1"))  
dataf$Depression <- factor(dataf$Depression, levels = c("0", "1"))
confusionMatrix(as.factor(a), dataf$Depression, positive = "1")

# ROC and AUC for Depression classification
roc_qda <- roc(response = dataf$Depression, predictor = as.numeric(a), plot = TRUE)
auc_val <- auc(response = dataf$Depression, predictor = as.numeric(a))
print(paste("AUC for Depression classification:", auc_val))
