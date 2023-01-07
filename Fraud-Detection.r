install.packages("ggplot2","corrplot","caTools")
library(ggplot2)
library(corrplot)
library(caTools)
data <- read.csv("creditcard.csv")

df <- data

summary(df)
unique(df$Class)


df <- na.omit(df)
df$Amount[df$Amount == 0] <- NA
summary(df$Amount)
df <- na.omit(df)

df[duplicated(df), ]
dim(df)


total <- nrow(data)
remaining <- nrow(df)
removed <- total-remaining
x <- c(removed, remaining)

piepercent<- round(100*x/sum(x), 1)

labels <-  c("Removed","Remaining")
# Plot the chart.
pie(x, labels=piepercent, main = "Total Dateset",col = rainbow(2))
legend("topright", c("Removed","Remaining"), cex = 0.8,
       fill = rainbow(2))



boxplot(Amount ~ Class, data = df, xlab = "Fraud Happened",
        ylab = "Amount", main = "Distribution of Amount")



boxplot(Time ~ Class, data = df, xlab = "Fraud Happened",
        ylab = "Time", main = "Distrubtion of Time")
dev.off()

data.cor = cor(df)


heatmap(data.cor)


class <- which(abs(data.cor['Class',])>0.01)
classNames <- c(names(class))



df <- df[classNames]



#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample.split(df$Class, SplitRatio = 0.7)
train  <- subset(df, sample == TRUE)
test   <- subset(df, sample == FALSE)


a <- table(test$Class)
a[names(a)==1]
a[names(a)==0]

a <- table(test$Class)
Fruad <- a[names(a)==1]
Normal <- a[names(a)==0]
x <- c(Normal, Fruad)

piepercent<- round(100*x/sum(x), 1)

labels <-  c("Normal","Fruad")
par(mfrow = c(1,1))
pie(x, labels=piepercent, main = "Fraud & Normal Transation",col = rainbow(4))
legend("topright", c("Normal","Fraud"), cex = 0.8,
       fill = rainbow(4))


fit <- glm(Class~., data = train, family = "binomial")

summary(fit)

test[c('Class')]

predicted <- predict(fit, test, type="response")
predicted
accuracy <- mean(predicted == test$Class)
# ROC-curve using pROC library
library(pROC)
roc(test$Class, predicted)

predicted <- as.factor(predicted)

class(as.factor(predicted))
as.factor(test$Class)

library(caret)

confusionMatrix(test$Class, predicted)


