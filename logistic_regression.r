library(tidyverse)
library(miscset)
library(dplyr)
library(caret)
data <- read.csv("~/Desktop/Churn/1Churn.csv", header=TRUE)
dim_desc(data)
glimpse(data)

log_dude <- createDataPartition(data$Churn,p=0.7,list=FALSE) 
set.seed(2017) 
training<- data[log_dude,] 
testing<- data[-log_dude,]
dim(training); dim(testing)
LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))

