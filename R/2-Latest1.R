library(leaps)

# Load clean data from ProcessedData
train <- read.csv("ProcessedData//clean-train.csv")
test <- read.csv("ProcessedData//clean-test.csv")


# to select variables using forward selection
regfit.fws=regsubsets(Survived~.,data=train,nvmax=9,method="forward")
regfit.fws.summary=summary(regfit.fws)

# to identify the min BIC for forward selection and plot
plot(regfit.fws.summary$bic,xlab="Number of Variables",ylab="bic",type='l')
which.min (regfit.fws.summary$bic)

# to list the varibale of min BIC
coef(regfit.fws,4)

# to select variables using backward selection
regfit.bwd=regsubsets(Survived~.,data=train,nvmax=9,method="backward")
regfit.bwd.summary=summary(regfit.bwd)

# to identify the min BIC and plot
plot(regfit.bwd.summary$bic,xlab="Number of Variables",ylab="bic",type='l')
which.min (regfit.bwd.summary$bic)

# to list the varibale of min BIC
coef(regfit.bwd,4)


#select best subset selection
regfit.best=regsubsets(Survived~.,data=train.sample,nvmax=9)

#commpute validation set error for the best model of each model size
test.mad=model.matrix(Survived~.,data=test.sample)

# run a loop to select the best model for size i, mutiply the model with test data and compute test MSE
val.errors=rep(NA,19)
for (i in 1:9){ 
coefi=coef(regfit.best,id=i)
pred=test.mad[,names(coefi)]%*%coefi
val.errors[i]=mean((test.sample$Survived-pred)^2)}

#find the best model based of low MSE
which.min(val.errors)
coef(regfit.best,5)

#predict using these 5 variable model
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi }

#choose a model of different sizes using K-fold cross validation
k=10
set.seed(1)
folds=sample(1:k,nrow(train1),replace=TRUE)
cv.errors=matrix(NA,k,9,dimnames=list(NULL,paste(1:9)))
for(j in 1:k){
  best.fit=regsubsets(Survived~.,data=train1[folds!=j,],nvmax=9)
  for(i in 1:9){
    pred=predict(best.fit,train1[folds==j,],id=i)
    cv.errors[j,i]=mean((train1$Survived[folds==j]-pred)^2)
  }}
#prepare a matrix jth variable is the cross validation for the jth model
mean.cv.errors=apply(cv.errors,2,mean)
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Survived~.,data=train1,nvmax=9)
coef(reg.best,5)

# Building Model
#perform ridge regression
x=model.matrix(Survived~Pclass+Sex+SibSp+Embarked+age2,train)[,-1]
y=train$Survived
Z=model.matrix(PassengerId~Pclass+Sex+SibSp+Embarked+age2,test)[,-1]
grid=10^seq(10,-2,length=100)
library(glmnet)

# split X to train and test
set.seed(1)
train.sample=sample(1:nrow(x),nrow(x)/2)
test.sample=(-train.sample)
y.test=y[test.sample]
ridge.mod=glmnet(x[train.sample,],y[train.sample],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test.sample,])
pred.ridge=rep(0,446)
pred.ridge[ridge.pred>.45]=1
mean((pred.ridge-y.test)^2)

#predict in actual test set
set.seed(1)
train.sample=(1:nrow(x))
test.sample=(1:nrow(test1))
ridge.mod=glmnet(x,y,alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=6,newx=z[test.sample,])
pred.ridge=rep(0,417)
pred.ridge[ridge.pred>.4]=1 
table(pred.ridge)
Survived = pred.ridge
PassengerId <- test1$PassengerId
Submit4 =data.frame(PassengerId,Survived)

#using cross validation to select lambda
set.seed(1)
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=Z)
pred.ridge=rep(0,417)
pred.ridge[ridge.pred>.55]=1 
table(pred.ridge)
Survived = pred.ridge
PassengerId <- test$PassengerId
Submit =data.frame(PassengerId,Survived)
write.csv(Submit, file="ProcessedData/Submit2.csv")

# buidling a model with Lasso and using cross validation to select lambda
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=Z)
pred.ridge=rep(0,417)
pred.ridge[lasso.pred>.45]=1
table(pred.ridge)
Survived = pred.ridge
PassengerId <- test$PassengerId
Submit =data.frame(PassengerId,Survived)
write.csv(Submit, file="ProcessedData/Submit4.csv")

# buidling a model with Principal Component Regression
library(pls)
set.seed(2)
pcr.fit=pcr(Survived~Pclass+Sex+SibSp+Embarked+age2,data=train,scale=TRUE,validation="CV")
test1 <- test[,c(4,6,8,13,16)]
pcr.pred=predict(pcr.fit,x,ncomp=5)
pred.ridge=rep(0,417)
pred.ridge[pcr.pred>.55]=1 
table(pred.ridge)
Survived = pred.ridge
PassengerId <- test$PassengerId
Submit =data.frame(PassengerId,Survived)
write.csv(Submit, file="ProcessedData/Submit8.csv")

#Building a model with random forect
library(randomForest)
forest <- randomForest(Survived~Pclass+Sex+SibSp+age2,data=train,prox=T)
pred <- predict(forest,test)
pred.ridge[pred>.55]=1 
