
train <- read.csv("train.csv")
# splitting and trimming names 
library(gdata)
train$Title <- trim(sapply(strsplit(as.character(train$Name),"[,.]"), "[[", 2))
table(train$Title)

# getting mean and stddev by category 
titleMean <- aggregate(Age ~ Title, data=train, mean)
titleSd <- aggregate(Age ~ Title, data=train, sd)
colnames(titleSd)[2] <- "Sd"
colnames(titleMean)[2] <- "Mean"
stats <- merge(titleMean, titleSd, by="Title")
# adding group mean and SD as columns to train dataset 
# later will use those values to draw numbers from normal distribution 
train <- merge(train, stats, by="Title")

library(Runuran)
# variable for the new age values 
train$age2 <- NA
set.seed(-1025)
for (i in 1:891){
# indicator var for missing Age 
  ind <- ifelse(is.na(train$Age), 1,0)
  if(ind[i] == 1){
# if age is missing, it is imputed with a new value from norm distr 
# with mean and stddev of a title group 
    train[i,16]  <- urnorm(1, mean=train$Mean[i], sd=train$Sd[i], lb=0)
  }else{
# if age is not missing, than new age = old age
  train[i,16] <- train[i,7]  
  }
}

rm(titleSd, titleMean, stats, ind, i)
# checking original and imputed ages 
summary(train$Age)
summary(train$age2)

