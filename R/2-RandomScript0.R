# Load clean data from ProcessedData
train <- load("ProcessedData//clean-train.rda")
test <- load("ProcessedData//clean-test.rda")

# explore gender
xtabs(~ Sex + Survived, data = train)
prop.table(table(train$Sex, train$Survived),1)

# Varun's script - first logic
# extract first letter of the Cabin 
# for cabin classes: 
# http://adventure.howstuffworks.com/titanic2.htm

logit.reg <- glm(Survived ~ as.factor(Sex) + Age + Fare
               + as.factor(Pclass)
               + SibSp + Parch,
               data=train, family="binomial")

# some sort of R-sqrd
cor(logit.reg$y, predict(logit.reg) ^ 2)

# Predict using Logit Reg model on training set
prediction <- predict(logit.reg, type="response")
head(prediction, 10)
train.predict <- rep(0, nrow(clean.train))
train.predict[prediction > 0.5] = 1
sum(train.predict)
prediction.summary <- table(train.predict, clean.train$Survived)
prediction.summary
mean(train.predict == clean.train$Survived)

# Predict on test set using the exact model above
test.predict <- predict(logit.reg, test, type="response")
test$Survived = rep(0, nrow(test))
test$Survived[test.predict > 0.5] = 1

submit <- data.frame(Passenger.ID=test$PassengerId, Survived=test$Survived)
write.csv(submit, file="SubmissionData/varun-test.csv", row.names=FALSE)

