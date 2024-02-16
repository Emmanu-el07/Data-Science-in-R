# Loading the Libraries
library(tidyverse)
library(rpart)
library(caret)

# Loading the dataset
diabet <- read_csv("C:/Users/LOLADE/Desktop/My Project in R/Data-Science-in-R/diabetes.csv")
diabet
diabet$Outcome <- ifelse(diabet$Outcome == 1, "Yes","No")
diabet$Outcome = as.factor(diabet$Outcome)
str(diabet)

# Spliting the dataset
split <- sample(2,nrow(diabet), prob = c(0.7,0.3),replace = TRUE)
split
diabet_train <- diabet[split == 1,]
diabet_test <- diabet[split == 2,]

# Modeling

diabet_model <- rpart(Outcome~., data = diabet)
diabet_model

plot(diabet_model,margin = 0.1)
text(diabet_test,UseMethod = TRUE,pretty = TRUE ,cex = -0.8)

# Predictions 

diabet_pred <- predict(diabet_model,diabet_test,type = "class")
diabet_pred

# To check the accuracy
confusionMatrix(table(diabet_pred,diabet_test$Outcome))
















