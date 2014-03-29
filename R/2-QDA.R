# Load clean data from ProcessedData
train <- read.csv("ProcessedData//clean-train.csv")

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

## Building Model
names(train.sample)

## Select interesting columns for QDA model
interesting.columns <- c("Pclass", "Sex", "age2", "Parch", "SibSp", "Embarked", "Survived")
df <- train.sample[, interesting.columns]

# Model QDA
#install.packages("MASS")
library(MASS)

sapply(df, class)
df.new <- na.omit(df)

names(df)
sapply(df, class)
model.qda <- qda(Survived ~ as.factor(Sex) + age2 + as.factor(Pclass)
                 + Parch + SibSp, data=df)

predicted <- predict(model.qda, df)$class
summary(predicted)
table(predicted, df$Survived)
mean(predicted == df$Survived)

test.predicted <- predict(model.qda, test.sample)$class
table(test.predicted, test.sample$Survived)
model.qda.mean <- mean(test.predicted == test.sample$Survived)
model.qda.mean

