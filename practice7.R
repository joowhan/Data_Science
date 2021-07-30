PRSA_data <- read.csv("PRSA_data.csv")
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ROCR)
library(ggplot2)

glimpse(PRSA_data)
train_data<-subset(PRSA_data, PRSA_data$year<2014)
test_data <- subset(PRSA_data, PRSA_data$year==2014)

colSums(is.na(train_data))
colSums((is.na(test_data)))

train_month <- subset(train_data, is.na(train_data$pm2.5))
test_month<-subset(test_data, is.na(test_data$pm2.5))
table(train_month$month)
table(test_month$month)

## number of sample before delete NA
num_train <-nrow(train_data)
num_test <-nrow(test_data)

## Delete NA
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

## number of sample agter delete NA
num_train <-nrow(train_data)
num_test <-nrow(test_data)

## rate before delete NA
round(prop.table(table(PRSA_data$year<2014))*100,2)
## rate After remove NA
round(nrow(train_data) / (num_train+ num_test)*100,2)
round(nrow(test_data) / (num_train + num_test)*100,2)


#plot(density(train_data$pm2.5), xlab = "N = 244118 Bandwidth = 9.884",xlim = c(0,1000), ylim=c(0,0.01))
plot(density(train_data$pm2.5),xlim = c(0,1000), ylim=c(0,0.01))
par(new = TRUE)
#plot(density(test_data$pm2.5),xlim = c(0,1000),ylim=c(0,0.01), col="red")
lines(density(test_data$pm2.5), col="red")
