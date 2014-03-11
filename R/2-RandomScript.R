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

df$CabinClass <- as.numeric(df$CabinClass)
df$IndividualFare <- as.numeric(df$IndividualFare)

temp <- df[boolean, ]
temp
boxplot(temp$IndividualFare ~ temp$CabinClass)

# Decision Tree



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

interesting.columns <- c("Pclass", "Sex", "age2", "Parch", "SibSp", "IndividualFare", "Embarked", "Survived")
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

# Model tree
#install.packages("MASS")
library(tree)

sapply(df, class)
df.new <- na.omit(df)

model.tree <- tree(Survived ~ ., data=df)

predicted <- predict(model.tree, df)

train.prediction <- rep(0, nrow(df))
train.prediction[predicted > 0.5] = 1
table(train.prediction, df$Survived)
mean(train.prediction == df$Survived)

test.predicted <- predict(model.tree, test.sample)
test.prediction <- rep(0, nrow(test.sample))
test.prediction[test.predicted > 0.5] = 1
table(test.prediction, test.sample$Survived)
mean(test.prediction == test.sample$Survived)


# Model QDA
#install.packages("MASS")
library(MASS)

sapply(df, class)
df.new <- na.omit(df)

names(df)
sapply(df, class)
model.qda <- qda(Survived ~ as.factor(Sex) + age2 + as.factor(Pclass)
                 + Parch + SibSp + IndividualFare, data=df)

predicted <- predict(model.qda, df)$class
summary(predicted)
table(predicted, df$Survived)
mean(predicted == df$Survived)

test.predicted <- predict(model.qda, test.sample)$class
table(test.predicted, test.sample$Survived)
model.qda.mean <- mean(test.predicted == test.sample$Survived)
model.qda.mean


#sum(is.na(as.numeric(df$Last)))
#length(df$Last)
#sum(!is.na(as.numeric(df$Last)))

#df$nChar <- sapply(df$Last, nchar)

#table(df$nChar)
#df[df$nChar == 5, ]


