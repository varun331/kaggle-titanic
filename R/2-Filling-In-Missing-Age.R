# Load clean data from ProcessedData
train <- read.csv("ProcessedData/clean-train.csv")
test <- read.csv("ProcessedData//clean-test.csv")

#install.packages("mice")
library(mice)

summary(train$Age)
summary(test$Age)

names(train)
train.predict <- train[, c(2, 4, 6, 7, 9)]
train.imp <- mice(train.predict, seed=123, m=3, diagnostics=FALSE,
                  method="norm.boot")
train.imp$predictorMatrix
train.imp <- complete(train.imp)

test.predict <- test[, c(2, 4, 6, 7, 9)]
test.imp <- mice(test.predict, seed=123, m=7, diagnostics=TRUE,
                 method="norm.boot")

