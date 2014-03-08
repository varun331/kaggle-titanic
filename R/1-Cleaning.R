# load data into data.frame
train <- read.csv("RawData//train.csv")
test <- read.csv("RawData//test.csv")

# Extract Title from Name
splited.names <- strsplit(as.character(train$Name), "[,.]")
# install.packages("gdata")
require(gdata)
train$title <- trim(sapply(splited.names, function(x) { x[2] }))


# Save clean data
save(train, file="ProcessedData/clean-train.rda")
save(test, file="ProcessedData/clean-test.rda")

write.csv(train, file="ProcessedData/clean-train.csv")
write.csv(test, file="ProcessedData/clean-test.csv")

