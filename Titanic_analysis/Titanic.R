rm(list = ls())


library(ROCR)
library(ggplot2)
library(MASS)
library(zoo)
library(caret)
library(DMwR)

setwd("C:\\Users\\mohad\\Desktop\\INSOFE\\Titanic")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

str(train)

summary(train)


train$Survived <- as.factor(train$Survived)

train$Pclass <- as.factor(train$Pclass)

train$SibSp <- as.factor(train$SibSp)

train$Parch <- as.factor(train$Parch)

train$Cabin <- as.numeric(train$Cabin)


train$Ticket <- as.numeric(train$Ticket)

train$Name <- as.character(train$Name)






narows <- train[which(is.na(train$Age) == TRUE ),]

#removing first column
train <- train[-1]

length()


names(train)
mean(train$Age)



train1 <- subset(train, select=  c(Survived, Pclass, Sex, SibSp, Parch, Embarked))

train1glm <- glm(Survived~., data = train1, family = binomial())
summary(train1glm)

stepAIC(train1glm)

train1_updated <- glm(Survived~Pclass + Sex + Age + SibSp, data = train1, family = binomial())
summary(train1_updated)





#prediciting 

prob11 <- predict(train1_updated, train1,type = "response")
predict11 <- ifelse(prob11>0.5,1,0)
table(train1$Survived, predict11)


unique(train1$SibSp)
unique(test$SibSp)
prob2 <- prediction(prob11, train1$Survived)

tprfpr1 <- performance(prob2, "tpr", "fpr")
plot(tprfpr1)





























train1pred <- predict(train1aic, type = "response")
pred_train1 <- ifelse(train1pred > 0.5,1,0)
tabtrain1 <- table(train1$Survived, pred_train1)
predtain1 <- predict(train1$Survived, pred_train1)





precision(tabtrain1)
recall(tabtrain1)
specificity(tabtrain1)
accuracy(tabtrain1)

tprfpr <- performance(prob, "tpr", "fpr")
# Plotting the true positive rate and false negative rate based on the threshold value
plot(tprfpr)


tprfpr1 <- performance(prob2, "tpr", "fpr")
