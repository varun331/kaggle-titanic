# Load clean data from ProcessedData
train <- read.csv("ProcessedData//clean-train.csv")
train1 <- train[,c(3,4,5,7,9,10,12,14,17)]

names(train1)
head(train, 5)

# split the train by 70%:30%
set.seed(-1025)
train.sample <- train1[sample(nrow(train1), nrow(train1) * 0.7), ]
indexes <- as.vector(train.sample[, 1])
test.sample <- subset(train1, !train1[, 1] %in% indexes)

# select variables using forward selection
regfit.fws=regsubsets(Survived~.,data=train1,nvmax=9,method="forward")
regfit.fws.summary=summary(regfit.fws)

# identify the min BIC for forward selection and plot
plot(regfit.fws.summary$bic,xlab="Number of Variables",ylab="bic",type='l')
which.min (regfit.fws.summary$bic)

# list the varibale of min BIC
coef(regfit.fws,4)

# select variables using backward selection
regfit.bwd=regsubsets(Survived~.,data=train1,nvmax=9,method="backward")
regfit.bwd.summary=summary(regfit.bwd)

# identify the min BIC and plot
plot(regfit.bwd.summary$bic,xlab="Number of Variables",ylab="bic",type='l')
which.min (regfit.bwd.summary$bic)

# list the varibale of min BIC
coef(regfit.bwd,4)



# Building Model

names(train.sample)

# Logistic Model
interesting.columns <- c("Pclass", "Sex", "age2", "Parch", "SibSp", "IndividualFare", "CabinClass", "Embarked", "Survived")

interesting.columns <- c("Pclass", "Sex", "age2", "Parch", "SibSp", "Embarked", "Survived")
df <- train.sample[, interesting.columns]


model.log <- glm(Survived ~ ., data=df, family="binomial")

predicted <- predict(model.log, type="response")

train.prediction <- rep(0, nrow(df))
train.prediction[predicted > 0.5] = 1
table(train.prediction, df$Survived)
mean(train.prediction == df$Survived)

test.predicted <- predict(model.log, test.sample, type="response")
test.prediction <- rep(0, nrow(test.sample))
test.prediction[test.predicted > 0.5] = 1
model.log.table <- table(test.prediction, test.sample$Survived)
model.log.table
model.log.mean <- mean(test.prediction == test.sample$Survived)
model.log.mean

summary(model.log)

#
