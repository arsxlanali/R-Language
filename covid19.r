library('ggplot2')


data <- read.csv("data.csv")
summary(data)
names(data)
data$YoungAge <-as.numeric(data$YoungAge)
data$MiddleAge <-as.numeric(data$MiddleAge)
data$OldAge <-as.numeric(data$OldAge)
data$White <-as.numeric(data$White)
data$Asian <-as.numeric(data$Asian)
data$GoodHealth <-as.numeric(data$GoodHealth)
data$FairHealth <-as.numeric(data$FairHealth)
data$BadHealth <-as.numeric(data$BadHealth)
data$Level1 <-as.numeric(data$Level1)
data$Level2 <-as.numeric(data$Level2)
data$Level3 <-as.numeric(data$Level3)
data$Level4 <-as.numeric(data$Level4)
data$TotalDeaths <-as.numeric(data$TotalDeaths)
data <-na.omit(data)
summary(data)
par(mfrow = c(2, 2))  # Set up a 2 x 2 plotting space

order <- data[order(-data$TotalDeaths):10,]


vector <- c('Level3','Level4')
for (col in vector) { 
  plot(data[[col]], data$TotalDeaths,xlab = col, ylab = "Total Deaths", type = "p", col = "red")
  boxplot(data[[col]],
          xlab = col,
          ylab = "Total Deaths",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )
}
vector <- c('Level1','Level2')
for (col in vector) { 
  plot(data[[col]], data$TotalDeaths,xlab = col, ylab = "Total Deaths", type = "p", col = "red")
  boxplot(data[[col]],
          xlab = col,
          ylab = "Total Deaths",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )
}
par(mfrow = c(2, 2))
vector <- c('GoodHealth','FairHealth')
for (col in vector) { 
  plot(data[[col]], data$TotalDeaths,xlab = col, ylab = "Total Deaths", type = "p", col = "red")
  boxplot(data[[col]],
          xlab = col,
          ylab = "Total Deaths",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )
}
par(mfrow = c(2, 2))
vector <- c('BadHealth','YoungAge')
for (col in vector) { 
  plot(data[[col]], data$TotalDeaths,xlab = col, ylab = "Total Deaths", type = "p", col = "red")
  boxplot(data[[col]],
          xlab = col,
          ylab = "Total Deaths",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )
}
par(mfrow = c(2, 2))
vector <- c('MiddleAge','OldAge')
for (col in vector) { 
  plot(data[[col]], data$TotalDeaths,xlab = col, ylab = "Total Deaths", type = "p", col = "red")
  boxplot(data[[col]],
          xlab = col,
          ylab = "Total Deaths",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )
}
par(mfrow = c(2, 2))
vector <- c('White','Asian')
for (col in vector) { 
  plot(data[[col]], data$TotalDeaths,xlab = col, ylab = "Total Deaths", type = "p", col = "red")
  boxplot(data[[col]],
          xlab = col,
          ylab = "Total Deaths",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )
}


#install.packages("corrplot")
par(mfrow = c(1, 1))

library(corrplot)

corrplot.mixed( cor(data[,2:14]),
               lower = "number", 
               upper = "circle",
               tl.col = "black")

qqnorm(data$YoungAge, main="Young Age")
qqline(data$YoungAge)
shapiro.test(data$YoungAge)

qqnorm(data$MiddleAge, main="Middle Age")
qqline(data$MiddleAge)
shapiro.test(data$MiddleAge)

qqnorm(data$OldAge, main="Old Age")
qqline(data$OldAge)
shapiro.test(data$OldAge)

qqnorm(data$White, main="White")
qqline(data$White)
shapiro.test(data$White)

+
qqnorm(data$Asian, main="Asian")
qqline(data$Asian)
shapiro.test(data$Asian)

qqnorm(data$GoodHealth, main="Good Health")
qqline(data$GoodHealth)
shapiro.test(data$GoodHealth)

qqnorm(data$FairHealth, main="Fair Health")
qqline(data$FairHealth)
shapiro.test(data$FairHealth)

qqnorm(data$BadHealth, main="Bad Health")
qqline(data$BadHealth)
shapiro.test(data$BadHealth)

qqnorm(data$Level1, main="Eductiona Level 1")
qqline(data$Level1)
shapiro.test(data$Level1)


qqnorm(data$Level2, main="Eductiona Level 2")
qqline(data$Level2)
shapiro.test(data$Level2)


qqnorm(data$Level3, main="Eductiona Level 3")
qqline(data$Level3)
shapiro.test(data$Level3)


qqnorm(data$Level4, main="Eductiona Level 4")
qqline(data$Level4)
shapiro.test(data$Level4)


#data <- data[,2:14]
scaled.data <- as.data.frame(scale(data[,2:12]))

scaled.data$TotalDeaths <- data$TotalDeaths

#install.packages("caTools")
library(caTools)
library(caret)
# Set the seed for reproducibility
set.seed(123)

# Split the data into training and test sets
train_index <- sample(1:nrow(scaled.data), size = 0.7 * nrow(scaled.data))
train <- scaled.data[train_index, ]
test <- scaled.data[-train_index, ]


lm<-lm(TotalDeaths~., data = scaled.data)
summary(lm)

par(mfrow=c(2,2))
plot(lm)
par(mfrow=c(1,1))

testx <- test[,1:11]

predicted <- predict(lm, testx,type = "response")
RMSE(predicted,test$TotalDeaths)

#income.graph<-ggplot(test, aes(x=income, y=happiness))+geom_point()
plot(predicted,                                # Draw plot using Base R
     test$TotalDeaths,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0,                                        # Add straight line
       b = 1,
       col = "red",
       lwd = 2)


