train1 <- subset(train,is.na(train$Age))
train2 <- subset(train,!is.na(train$Age))
splitnames = strsplit(as.character(train$Name),"[,.]")
firstelement <- function(x){x[2]}
sapply(splitnames,firstelement)
Titel<- sapply(splitnames,firstelement)
train$title <- Titel
test <- train[which(is.na(train$Age)),]
table(test$title)
train1 <- train[which(!is.na(train$Age)),]