install.packages("ggplot2","corrplot","caTools")
library(ggplot2) # for ploting gprah
library(corrplot) # For calulcating corelation
library(caTools) # For train & test split
library(pROC) # For ROC cure
library(caret) # confusion matrix library

data <- read.csv("creditcard1.csv") # importing dataset
df <- data

summary(df) # showing summary of dataset
unique(df$Class) # finding unique values in the Class column


df <- na.omit(df) # remove the NA values
df$Amount[df$Amount == 0] <- NA # replacing the 0 values to NA
summary(df$Amount) # showing summary
df <- na.omit(df)  # remove NA values again

df[duplicated(df), ] # remove duplicate if any
dim(df) # priting dimension of dataset


total <- nrow(data) # finding number of rows
remaining <- nrow(df) # fidning remaning rows after data prepration
removed <- total-remaining # claculating removed rows
x <- c(removed, remaining) # saving values in x 

piepercent<- round(100*x/sum(x), 1) # calulcate the percentage

labels <-  c("Removed","Remaining") # Adding labels
# Plot the chart.
pie(x, labels=piepercent, main = "Total Dateset",col = rainbow(2))
legend("topright", c("Removed","Remaining"), cex = 0.8,
       fill = rainbow(2)) # Add lagend in pie chart


# printing box plot for amount and class
boxplot(Amount ~ Class, data = df, xlab = "Fraud Happened",
        ylab = "Amount", main = "Distribution of Amount")


# printing box plot for time and class
boxplot(Time ~ Class, data = df, xlab = "Fraud Happened",
        ylab = "Time", main = "Distrubtion of Time")

# finding the correlation between features
data.cor = cor(df)

# ploting head map for correlation matrix
heatmap(data.cor)

# Saving elements with less than 0.01 correlation between class
class <- which(abs(data.cor['Class',])>0.01)
classNames <- c(names(class)) # Saving their names in list



df <- df[classNames]  # keeping the necessery columnns

#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample.split(df$Class, SplitRatio = 0.7)
train  <- subset(df, sample == TRUE)
test   <- subset(df, sample == FALSE)


a <- table(train$Class) 
a[names(a)==1] # number of 1 in train dataset
a[names(a)==0] # number of 0's in train dataset

a <- table(test$Class)
Fruad <- a[names(a)==1] # number of 1 in test dataset
Normal <- a[names(a)==0] # number of 0 in test dataset
x <- c(Normal, Fruad)

piepercent<- round(100*x/sum(x), 1) # calculating percentage

labels <-  c("Normal","Fruad") # adding labels
par(mfrow = c(1,1))
pie(x, labels=piepercent, main = "Fraud & Normal Transation",col = rainbow(4))
legend("topright", c("Normal","Fraud"), cex = 0.8,
       fill = rainbow(4))


x_test <- df[,-c(22)] # spllting in x features
y_test <- df[,c(22)] # splliting in y feautres
test_y <- ifelse(y_test > 0.5, "Fraud", "Normal") # converting 0 and 1 in string


fit <- glm(Class~., data = train, family = "binomial") # model traning
summary(fit) # prinintg model summary
predicted <- predict(fit, x_test, type="response") # predicted values
predictedClass <- ifelse(predicted > 0.5, "Fraud", "Normal") # converting values in class
# Calculate the accuracy
accuracy1 <- mean(predictedClass == test_y)
confusionMatrix(as.factor(predictedClass),as.factor(test_y))
# ROC-curve using pROC library
scor_roc <- roc(y_test, predicted)
plot(scor_roc, main="ROC curve -- For Un-Weighted Model ") # roc curve ploting





