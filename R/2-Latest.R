# Load clean data from ProcessedData
train <- read.csv("ProcessedData//clean-train.csv")
train1 <- train[,c(3,4,5,7,9,10,12,14,17)]

names(train)
head(train, 5)

# Relationship between Cabin and Ticket
df <- train[, c("Ticket", "Cabin", "Fare")]

# First Letter of Cabin
train$CabinClass <- substr(as.character(train$Cabin), 0, 1)

splitted.cabin <- strsplit(as.character(train$Cabin), " ")
train$CabinCount <- sapply(splitted.cabin, length)

# Divide Fare / # of Cabin

train$IndividualFare <- train$Fare

boolean <- train$CabinCount != 0
train$IndividualFare[boolean] <- train$Fare[boolean] / train$CabinCount[boolean]

# Split Ticket by space
splitted.ticket <- strsplit(as.character(train$Ticket), " ")

join.front <- function(x) {
  return.value <- NA
  if (length(x) > 1) {
    return.value <- paste(as.character(x[1:length(x)-1]), collapse="|")
  } 
  return(return.value)
} 

#install.packages("gdata")
library(gdata)

train$Front <- trim(sapply(splitted.ticket, join.front))
table(sapply(splitted.ticket, length))
train$Last <- trim(sapply(splitted.ticket,
                  function(x) {x[length(x)]}))

# split the train by 70%:30%
set.seed(-1025)
train.sample <- train[sample(nrow(train), nrow(train) * 0.7), ]
indexes <- as.vector(train.sample[, 1])
test.sample <- subset(train, !train[, 1] %in% indexes)

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
