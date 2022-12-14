#install.packages('stringr')
#install.packages('dplyr')
library(stringr)
library(dplyr)
data <- read.csv("data.csv")
summary((data))


data[c('str', 'cost')] <- str_split_fixed(data$repaircost, ' ', 2)
data <- data[c('driver','age','address','passenger1','passenger2','cost','fraudFlag')]
data1 <- replace(data$cost,str_detect(data$cost, "3k"),3000)
data1 <- replace(data1,str_detect(data1, "2k"),2000)
data1 <- replace(data1,str_detect(data1, "1k"),1000)
data$cost <- data1
data$cost[data$cost=='$*0'] <- NA
data$cost <- as.integer(data$cost)
data <- na.omit(data)



boxplot(cost~fraudFlag,data=data, main="Fraud in Taxi Service",
        xlab="Fraud Happened?", ylab="Cost")
boxplot(age~fraudFlag,data=data, main="Fraud in Taxi Service",
        xlab="Fraud Happened?", ylab="Age of Driver")
data$fraudFlag <-as.integer(data$fraudFlag)
df_grp <- data %>% group_by(driver) %>% 
  summarise(fraud=sum(fraudFlag))


n_driver <- nrow(df_grp)
df_grp$fraud[df_grp$fraud==0] <- NA
df_grp <- na.omit(df_grp)
nf_driver <- nrow(df_grp)
n_driver <- n_driver - nf_driver
pie(c(nf_driver,n_driver), c("0.11%","0.89%"), main="Percentage of Honest & Dishonest Drivers",col = rainbow(2))
legend("topright", c("Dishonest","Honest"), cex = 0.8,
       fill = rainbow(2))
df_grp <- df_grp[order(df_grp$fraud,decreasing = TRUE),]
# Plot the bar chart 
par(mai=c(1,2,1,1))
barplot(df_grp$fraud,names.arg=df_grp$driver,xlab="Number of times Fraud done by driver",
        main="Driver Fruad Chart",horiz = TRUE,las=1, col = 'black')



df_grp <- data %>% group_by(cost) %>% 
  summarise(fraud=sum(fraudFlag))
barplot(df_grp$fraud,names.arg=df_grp$cost, main="Fraud in Taxi Service",
        xlab="Amount", ylab="Number of Times Fraud Happned")



df_grp <- data %>% group_by(passenger1) %>% 
  summarise(fraud=sum(fraudFlag))
df_grp$fraud[df_grp$fraud==0] <- NA
df_grp <- na.omit(df_grp)
df_grp <- df_grp[order(df_grp$fraud,decreasing = TRUE),]
par(mai=c(1,2,1,1))
barplot(df_grp$fraud,names.arg=df_grp$passenger1,xlab="Number of times Fraud done by driver",
        main="Driver Fruad Chart",horiz = TRUE,las=1, col = 'black')




