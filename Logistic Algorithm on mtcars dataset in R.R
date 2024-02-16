library(tidyverse)
library(caTools)
library(caret)
library(corrplot)

# Loading the mtcars Dataset
mtcars

# Splitting the Dataset
split <- sample.split(mtcars,SplitRatio = 0.7)
split

# Subset the Training and Testing Dataset
training <- subset(mtcars,split == TRUE)
testing <- subset(mtcars,split == FALSE)

# Creating the model on the Training Dataset
model <- glm(vs ~ disp + wt, data = training, family = "binomial")
summary(model)

#Predicting the model
# The Prediction would be based on a binomial basis since the family is bimomial i.e 0 or 1
# Using "Ford Pantera L to see if the model predicts the vs type correctly"
data <- data.frame(disp = 351.0, wt = 3.570)
answer <- predict(model,data,type = "response")
answer
# "0.0008230118" , this shows that the probability of the car having a vs Engine is less that 0.5

# Predicting for another vehicle
data2 <- data.frame(disp = 120.1  , wt = 2.465)
answer2 <- predict(model,data2,type = "response")
answer2
# "0.7237814" , this shows that the probability of the vehicle being vs is greater than 0.5
