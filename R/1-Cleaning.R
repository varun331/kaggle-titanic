# load data into data.frame
train <- read.csv("RawData//train.csv")
test <- read.csv("RawData//test.csv")

# Extract Title from Name
splited.names <- strsplit(as.character(train$Name), "[,.]")
# install.packages("gdata")
require(gdata)
train$title <- trim(sapply(splited.names, function(x) { x[2] }))

# getting mean and stddev by category 
# later will use those values to draw numbers from normal distribution
library(plyr)
train <- ddply(train, .(title), mutate, 
               Mean=ave(Age, FUN=function(x){mean(x, na.rm=TRUE)}), 
               Sd=ave(Age, FUN=function(x){sd(x, na.rm=TRUE)}))

#install.packages("Runuran")
library(Runuran)
# variable for the new age values 
train$age2 <- NA
set.seed(-1025)
for (i in 1:nrow(train)){
  # indicator var for missing Age 
  ind <- ifelse(is.na(train$Age), 1,0)
  if(ind[i] == 1){
    # if age is missing, it is imputed with a new value from norm distr 
    # with mean and stddev of a title group 
    train$age2[i]  <- urnorm(1, mean=train$Mean[i], sd=train$Sd[i], lb=0)
  }else{
    # if age is not missing, than new age = old age
    train$age2[i] <- train$Age[i]
  }
}

rm(titleSd, titleMean, stats, ind, i)
# checking original and imputed ages 
summary(train$Age)
summary(train$age2)




# Save clean data
save(train, file="ProcessedData/clean-train.rda")
save(test, file="ProcessedData/clean-test.rda")

write.csv(train, file="ProcessedData/clean-train.csv")
write.csv(test, file="ProcessedData/clean-test.csv")

