FSCR = function(X, Y, k)
{
  J<- rep(NA, ncol(X))
  names(J)<- colnames(X)
  for (i in 1:ncol(X))
  {
    X1<- X[which(Y==0),i]
    X2<- X[which(Y==1),i]
    mu1<- mean(X1); mu2<- mean(X2); mu<- mean(X[,i])
    var1<- var(X1); var2<- var(X2)
    n1<- length(X1); n2<- length(X2)
    J[i]<- (n1*(mu1-mu)^2+n2*(mu2-mu)^2)/(n1*var1+n2*var2)
  }
  J<- sort(J, decreasing=TRUE)[1:k]
  return(list(score=J))
}
WLCX = function(X, Y, k) # X - matrix with predictors, Y - binary outcome, k top candidates\n",
{
  J<- rep(NA, ncol(X))
  names(J)<- colnames(X)
  for (i in 1:ncol(X))
  {
    X_rank<- apply(data.matrix(X[,i]), 2, function(c) rank(c))
    X1_rank<- X_rank[which(Y==0)]
    X2_rank<- X_rank[which(Y==1)]
    mu1<- mean(X1_rank); mu2<- mean(X2_rank); mu<- mean(X_rank)
    n1<- length(X1_rank); n2<- length(X2_rank); N<- length(X_rank)
    num<- (n1*(mu1-mu)^2+ n2*(mu2-mu)^2)
    denom<- 0
    for (j in 1:n1)
      denom<- denom+(X1_rank[j]-mu)^2
    for (j in 1:n2)
      denom<- denom+(X2_rank[j]-mu)^2
    J[i]<- (N-1)*num/denom
  }
  J<- sort(J, decreasing=TRUE)[1:k]
  return(list(score=J))
}
library(pROC);
library(ggplot2); 
library(randomForest);
library(caret)
data("PimaIndiansDiabetes");
df <- PimaIndiansDiabetes;
summary(pimaInd)


#install.packages('robustHD')
library(robustHD)
summary(df$pregnant)
summary(df$glucose)
summary(df$pregnant)
summary(df$pressure)
summary(df$triceps)
summary(df$insulin)
summary(df$mass)
summary(df$pedigree)
summary(df$age)
df$pregnant <- robustHD::standardize(df$pregnant)
df$glucose <- robustHD::standardize(df$glucose)
df$pressure <- robustHD::standardize(df$pressure)
df$triceps <- robustHD::standardize(df$triceps)
df$insulin <- robustHD::standardize(df$insulin)
df$mass <- robustHD::standardize(df$mass)
df$pedigree <- robustHD::standardize(df$pedigree)
df$age <- robustHD::standardize(df$age)
df$diabetes <-ifelse(df$diabetes=="pos",1,0)
summary(df$pregnant)
summary(df$glucose)
summary(df$pregnant)
summary(df$pressure)
summary(df$triceps)
summary(df$insulin)
summary(df$mass)
summary(df$pedigree)
summary(df$age)



library(caTools)
set.seed(55)
#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample.split(df$diabetes, SplitRatio = 0.7)
training   <- subset(df, sample == TRUE)
testing  <- subset(df, sample == FALSE)
train_x <-select(training,pregnant,glucose,pressure,triceps,insulin,mass,pedigree,age)
WLCX(train_x, training$diabetes, 8)
FSCR(train_x, training$diabetes, 8)
# Selecting the top 3 features 
training_redc <- select(training, glucose, mass,age,diabetes)
head(training_redc)
testing_redc <- select(testing, glucose, mass,age,diabetes)
head(testing_redc)





logsticReg1 <- glm(diabetes~.,data=training_redc,family="binomial")
summary(logsticReg1)



randomforst1 <- randomForest(factor(diabetes)~.,data=training_redc,proximity=TRUE, trees=500)
importance(randomforst2)
varImpPlot(randomforst2)
pRedued <- predict(randomforst1,newdata=testing_redc, type = "response")
confMatrix = table(factor(testing_redc$diabetes),pRedued)
confMatrix
sensitivity(confMatrix)
specificity(confMatrix)
randomforst2 <- randomForest(factor(diabetes)~.,data=training,proximity=TRUE)
importance(randomforst2)
varImpPlot(randomforst2)
pFull <- predict(randomforst2,newdata=testing, type = "response")
confMatrix = table(factor(testing$diabetes),pFull)
confMatrix
sensitivity(confMatrix)
specificity(confMatrix)


library(ROCR)
logsticReg1<- glm(diabetes~.,data=training_redc,family="binomial")
pred1 <- predict(logsticReg1,newdata = testing_redc,type = "response")
pred.obj <- prediction(predictions = pred1, labels = testing_redc$diabetes)
perf <- performance(pred.obj, "tpr", "fpr")
auc_ROCR <- performance(pred.obj, measure = "auc")
sprintf("AUC:(Reduced Dataset) %f",auc_ROCR@y.values[[1]])

plot(perfcol="blue")
par(new=TRUE)
logsticReg2<- glm(diabetes~.,data=training,family="binomial")
pred2 <- predict(logsticReg2,newdata = testing,type = "response")
pred.obj2 <- prediction(predictions = pred2, labels = testing$diabetes)
perf2 <- performance(pred.obj2, "tpr", "fpr")
auc_ROCR <- performance(pred.obj2, measure = "auc")
sprintf("AUC: (full dataset) %f",auc_ROCR@y.values[[1]])
plot(perf2,col="red")

