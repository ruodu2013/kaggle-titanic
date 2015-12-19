setwd("C:/Users/Ruo/Documents/Courses-2015/tatanic")
train<-read.csv("train.csv", stringsAsFactors=FALSE)
head(train)
str(train)
prop.table(table(train$Survived))

test<-read.csv("test.csv", stringsAsFactors=FALSE)
nrow(test)
test$survived<-rep(0,418)
submit<-data.frame(passengerId=test$PassengerId,survived=test$survived)
write.csv(submit,file="theyallperish.csv",row.names=F)

#women all survive
prop.table(table(train$Sex,train$Survived),1)
test$survived[test$Sex=="female"]<-1
test
submit<-data.frame(passengerId=test$PassengerId,survived=test$survived)
write.csv(submit,file="womenallsurvive.csv",row.names=F)

#age model
summary(train$Age)
train$Child <- 0
train$Child[train$Age<18]<-1

aggregate(Survived~Child+Sex,data=train,FUN=function(x){sum(x)/length(x)})

#prediction1 use rpart
library(rpart)
install.packages('rattle')
install.packages('rpart.plot')
library(rattle)
library(rpart.plot)

fitrpart<-rpart(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train,method="class")

fancyRpartPlot(fitrpart)

Prediction1 <- predict(fitrpart, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction1)
write.csv(submit, file = "prediction1.csv", row.names = FALSE)


#prediction2 random tree
train<-read.csv("train.csv",stringsAsFactors=FALSE)
test<-read.csv("test.csv",stringsAsFactors=FALSE)

test$Survived <- NA

combi <- rbind(train, test)
str(combi)
#check missing values: age 263,fare 1,embarked 2
summary(combi)
#add missing values
summary(combi$Embarked)
combi$Embarked[combi$Embarked==""]<-"S"
combi$Embarked<-factor(combi$Embarked)

summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

#split train and test data
train <- combi[1:891,]
test <- combi[892:1309,]

install.packages('randomForest')
library(randomForest)
fitrandomtree <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, importance=TRUE, ntree=100)


prediction21<-predict(fitrandomtree,test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction21)
write.csv(submit, file = "prediction21.csv", row.names = FALSE)

#logistic regression
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test  <- read.csv("test.csv",  stringsAsFactors=FALSE)

str(train)
extractFeatures <- function(data) {
    features <- c("Pclass",
                  "Age",
                  "Sex",
                  "Parch",
                  "SibSp",
                  "Fare",
                  "Embarked")
    fea <- data[,features]
    fea$Age[is.na(fea$Age)] <- -1
    fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
    fea$Embarked[fea$Embarked==""] = "S"
    fea$Sex      <- as.factor(fea$Sex)
    fea$Embarked <- as.factor(fea$Embarked)
    return(fea)
}
logistic.fit<-glm(train$Survived ~ Pclass +Sex +Age+Parch+SibSp+Fare+Embarked, family = binomial, data=extractFeatures(train))

prediction3<-predict(logistic.fit,extractFeatures(test))
prediction3[prediction3 >=0.5] <- 1
prediction3[prediction3 != 1] <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction3)
write.csv(submit, file = "prediction3.csv", row.names = FALSE)

