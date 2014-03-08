# Load clean data from ProcessedData
train <- read.csv("ProcessedData//clean-train.csv")

names(train)
head(train, 5)

# Relationship between Cabin and Ticket
df <- train[, c("Ticket", "Cabin", "Fare")]

# First Letter of Cabin
df$FirstCabin <- substr(as.character(df$Cabin), 0, 1)

splitted.cabin <- strsplit(as.character(df$Cabin), " ")
df$CabinCount <- sapply(splitted.cabin, length)

# Divide Fare / # of Cabin

df$DividedFare <- df$Fare

boolean <- df$CabinCount != 0
df$DividedFare[boolean] <- df$Fare[boolean] / df$CabinCount[boolean]

df$FirstCabin <- as.numeric(df$FirstCabin)
df$DividedFare <- as.numeric(df$DividedFare)

temp <- df[boolean, ]
temp
boxplot(temp$DividedFare ~ temp$FirstCabin)

# Decision Tree



# Split Ticket by space
splitted.ticket <- strsplit(as.character(df$Ticket), " ")

join.front <- function(x) {
  return.value <- NA
  if (length(x) > 1) {
    return.value <- paste(as.character(x[1:length(x)-1]), collapse="|")
  } 
  return(return.value)
} 

#install.packages("gdata")
library(gdata)

df$Front <- trim(sapply(splitted.ticket, join.front))
table(sapply(splitted.ticket, length))
df$Last <- trim(sapply(splitted.ticket,
                  function(x) {x[length(x)]}))



sum(is.na(as.numeric(df$Last)))
length(df$Last)
sum(!is.na(as.numeric(df$Last)))

df$nChar <- sapply(df$Last, nchar)

table(df$nChar)
df[df$nChar == 5, ]
