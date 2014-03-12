train <- read.csv("train.csv")
test <- read.csv("test.csv")

# adding Survive variable to test set
test$Survived <- rep(0, 418)
#sum(train$Survived)
#summary(train)

# exploring gender 
xtabs(~ Sex + Survived, data = train)
prop.table(table(train$Sex, train$Survived),1)

# updating survival based on Sex
#test$Survived[test$Sex == 'female'] <- 1

# extract first letter of the Cabin 
# for cabin classes: 
# http://adventure.howstuffworks.com/titanic2.htm
names(train)
logReg1 <- glm(Survived ~ as.factor(Sex) + Age + Fare 
               + as.factor(Pclass)
               + SibSp + Parch,
               data = train, family="binomial")
names(logReg1)
# some sort of R-sqrd
cor(logReg1$y,predict(logReg1))^2 
# predicting probabilities
# vector of 0 for Survived (train set)
trainPredict <- rep(0, 891)
# Prediction probabilities (train set)
logProbs <- predict(logReg1, type="response")
logProbs[1:10]
# Probability threshold to classify survivors
trainPredict[logProbs > 0.5 ] = 1
sum(testPredict)
sum(train$Survived)
# Check how many observations were misclassified 
table(trainPredict, train$Survived)
# Percent of correctly predicted events 
mean(trainPredict == train$Survived)
# Can also be computed by summing diagonal elements of the table 
# and deviding them by total N
(336+134)/891

# Adding predicted values to test dataset 
testPredict <- predict(logReg1, test, type="response")
test$Survived[testPredict > 0.5 ] = 1

# submission dataset 
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "submit.csv", row.names = FALSE)

# imputting missing values
library(mice)
summary(train$Age)
summary(test$Age)
names(train)
trainPreds <- train[, c(2,4:6,7,9)]
trainImp <- mice(trainPreds, seed=123, m=3, diagnostics=FALSE, 
                 method="norm.boot")
trainImp$predictorMatrix
trainImp <- complete(trainImp)

testPreds <- test[, c(2,4:6,7,9)]
testImp <- mice(testPreds, seed=123, m=7, diagnostics=TRUE, 
                 method="norm.boot")
testImp$predictorMatrix
testImp <- complete(testImp)
testImp1 <- cbind(testImp, test[,c(1,3,8,10:11)])

# QDA
library(MASS)
trainQDA <- na.omit(train)
qda1 <- qda(Survived ~ as.factor(Sex) + Age + Fare 
            + as.factor(Pclass)
            + SibSp + Parch, data=trainQDA)
qda1
qdaClass <- predict(qda1, trainQDA)$class
table(qdaClass, trainQDA$Survived)
mean(qdaClass == trainQDA$Survived)

qdaTest <- predict(qda1, testImp1)$class
sum(is.na(qdaTest))
qdaTest[1:10]
testImp1 <- cbind(testImp1, qdaTest)
names(testImp1)[12] <- "Survived"  
# submission dataset 
submit <- data.frame(PassengerId = testImp1$PassengerId, Survived = testImp1$Survived)
write.csv(submit, file = "submit.csv", row.names = FALSE)
# 2-18-14 - only 74% accuracy 
