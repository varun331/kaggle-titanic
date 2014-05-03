# load data into data.frame
test <- read.csv("RawData//test.csv")

# Extract Title from Name
splited.names <- strsplit(as.character(test$Name), "[,.]")
# install.packages("gdata")
require(gdata)
test$title <- trim(sapply(splited.names, function(x) { x[2] }))

# Age Column from Maria
# getting mean and stddev by category 
titleMean <- aggregate(Age ~ title, data=test, mean)
titleSd <- aggregate(Age ~ title, data=test, sd)
colnames(titleSd)[2] <- "Sd"
colnames(titleMean)[2] <- "Mean"
stats <- merge(titleMean, titleSd, by="title")
# adding group mean and SD as columns to train dataset 
# later will use those values to draw numbers from normal distribution 
test <- merge(test, stats, by="title")
#install.packages("Runuran")
library(Runuran)
# variable for the new age values 
test$age2 <- NA
set.seed(-1025)
for (i in 1:nrow(test)){
  # indicator var for missing Age 
  ind <- ifelse(is.na(test$Age), 1,0)
  if(ind[i] == 1){
    # if age is missing, it is imputed with a new value from norm distr 
    # with mean and stddev of a title group 
    test$age2[i]  <- urnorm(1, mean=test$Mean[i], sd=test$Sd[i], lb=0)
  }else{
    # if age is not missing, than new age = old age
    test$age2[i] <- test$Age[i]
  }
}

rm(titleSd, titleMean, stats, ind, i)
# checking original and imputed ages 
summary(test$Age)
summary(test$age2)




# Save clean data
save(train, file="ProcessedData/clean-train.rda")
save(test, file="ProcessedData/clean-test.rda")

write.csv(train, file="ProcessedData/clean-train.csv")
write.csv(test, file="ProcessedData/clean-test.csv")

