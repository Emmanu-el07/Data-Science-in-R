# Loading the libraries
library(tidyverse)
library(randomForest)
library(tuneRF)
library(caret)
# Loading the Data
diabet <- read_csv("C:/Users/LOLADE/Desktop/RandomForest/diabetes.csv")
str(diabet)
# Changing the Variable Types
diabet$Outcome <- ifelse(diabet$Outcome == "1","YES", "NO")
diabet$Outcome = as.factor(diabet$Outcome)

######### Splitting the Dataset ##########

set.seed(123)
split <- sample(2,nrow(diabet),prob = c(0.7,0.3),replace = TRUE)
diabet_train <- diabet[split == 1,]
str(diabet_train)
diabet_test <- diabet[split == 2,]

######## Optimizing the model and getting the Priority Variables #######
# Creating the model
diabet_forest <- randomForest(Outcome~.,data = diabet_train)
diabet_forest

# to get the Gini Index (Priority of Importance)
diabet_forest$importance
importance(diabet_forest)
varImpPlot(diabet_forest)
# Best mtry value
bestmtry <- tuneRF(diabet_train, diabet_train$Outcome,stepFactor = 1.2,improve = 0.01,trace = T,plot = T)

# Predicting the model with the test data 

diabet_pred <- predict(diabet_forest,diabet_test,type = "class")
as.data.frame(diabet_pred)
### To Visualise
barplot(table(diabet_pred))

####### To Validate the model better

confusionMatrix(table(diabet_pred,diabet_test$Outcome))
