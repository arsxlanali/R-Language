#install.packages('mlbench')
#install.packages('dplyr')
#install.packages('DescTools')
#install.packages("randomForest")
#install.packages('Hmisc')
#install.packages('ggplot2')
#install.packages('gridExtra')
#install.packages('ROCR')
#install.packages('caret')
#install.packages('pROC')
#install.packages('Metrics')
library(pROC)
library(ROCR);
library(Hmisc); 
library(ggplot2); 
library(gridExtra)
library(mlbench)
library(dplyr)
library(DescTools)
library(randomForest)
library(caret)
library(robustHD)
data("PimaIndiansDiabetes")
data <- PimaIndiansDiabetes
summary(data)


# preparing the datset
data$glucose[data$glucose==0] <- NA
data$pressure[data$pressure==0] <- NA
data$triceps[data$triceps==0] <- NA
data$insulin[data$insulin==0] <- NA
data <- na.omit(data)
data$pregnant <- robustHD::standardize(data$pregnant)
data$glucose <- robustHD::standardize(data$glucose)
data$pressure <- robustHD::standardize(data$pressure)
data$triceps <- robustHD::standardize(data$triceps)
data$insulin <- robustHD::standardize(data$insulin)
data$mass <- robustHD::standardize(data$mass)
data$pedigree <- robustHD::standardize(df$pedigree)
data$age <- robustHD::standardize(df$age)


Data_X <- data[, c('pregnant', 'glucose', 'pressure','triceps','insulin','mass','pedigree','age')]
Data_Y <- data[,c('diabetes')]
Data_Y <-ifelse(Data_Y=="pos",1,0)
data['diabetes'] <- Data_Y

FSCR = function(X, Y, k) # X - matrix with predictors, Y - binary outcome, k top candidates\n",
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


#use 70% of dataset as training set and 30% as test set 
#create ID column
data$id <- 1:nrow(data)
train <- data %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(data, train, by = 'id')
data <- data[,-1]
K <- 8
train_x <- train[, c('pregnant', 'glucose', 'pressure','triceps','insulin','mass','pedigree','age')]
train_y <- train[,c('diabetes')]
FSCR(train_x, train_y, K)
WLCX(Data_X, Data_Y, K)
# feature selection 
reduce_training <- train[,c("glucose","insulin","age","diabetes")]
reduce_testing <- test[,c("glucose","insulin","age","diabetes")]




# Logistic regression with all the predictors included
logit1<- glm(diabetes~.,data=reduce_training,family="binomial")
summary(logit1)




# Logistic regression with confusion matrix
rf1 <- randomForest(factor(diabetes)~.,data=reduce_training,importance=TRUE,ntree=50, mtry=3, replace=TRUE)
retain_p <- rf1 %>% 
  predict(newdata = reduce_testing)

table(
  actualclass = reduce_testing$diabetes,
  predictedclass = retain_p
) %>% 
  confusionMatrix() %>% 
  print()
threshold=0.5
predicted_values<-ifelse(predict(logit1, newdata = reduce_testing,type="response")>threshold,1,0)
actual_values<-reduce_testing$diabetes
conf_matrix<-table(predicted_values,actual_values)
confusionMatrix(conf_matrix)


rf2 <- randomForest(factor(diabetes)~.,data=train,importance=TRUE,ntree=50, mtry=3, replace=TRUE)
retain_p <- rf2 %>% 
  predict(newdata = test)

table(
  actualclass = test$diabetes,
  predictedclass = retain_p
) %>% 
  confusionMatrix() %>% 
  print()


train <- train[,-1]
test <- test[,-1]
# Logistic regression with all the predictors included
logit2<- glm(diabetes~.,data=train,family="binomial")
summary(logit2)
log_predict <- predict(logit1,newdata = reduce_testing,type = "response")

plot(roc(reduce_testing$diabetes, log_predict, direction="<"),
     col="blue", lwd=3)
par(new=TRUE)
log_predict <- ifelse(log_predict > 0.5,1,0)
sprintf("AUC: %f",auc(reduce_testing$diabetes,log_predict))
log_predict2 <- predict(logit2,newdata = test,type = "response")
plot(roc(test$diabetes, log_predict2, direction="<"),
     col="yellow", lwd=3, main="Reduced Feature Model/Full")
log_predict2 <- ifelse(log_predict2 > 0.5,1,0)
sprintf("AUC: %f",auc(test$diabetes,log_predict2))




















