datTrain_notImputed <- datTrain_casted

datTrain_NaReduced <- na.omit(datTrain_notImputed)

#NAs
detectNA <- function(x){sum(is.na(x))/length(x)*100}
apply(datTrain_NaReduced, 2, detectNA)


#Test / Training Split
set.seed(304)
trainingRows2 <- sort(sample(nrow(datTrain_NaReduced), nrow(datTrain_NaReduced)*.7))

Train_NaReduced <- datTrain_NaReduced[trainingRows2,]
Test_NaReduced <- datTrain_NaReduced[-trainingRows2,]

#Decision Tree
library(rpart)
library(rpart.plot)

##1st tree
set.seed(31)
dt.default_NotImputed <- rpart(target~ ., data=Train_NaReduced, cp=-1)

printcp(dt.default_NotImputed)
plotcp(dt.default_NotImputed)


#Prediction & Performance
#Train Data
train.default_NotImputed.pred <- predict(dt.default_NotImputed, newdata = Train_NaReduced, type = "class")
train.default_NotImputed.confMatrix <- table(train.default_NotImputed.pred, Train_NaReduced[, 12])
print(train.default_NotImputed.confMatrix)

train.default_NotImputed.accuracy <- sum(diag(train.default_NotImputed.confMatrix))/sum(train.default_NotImputed.confMatrix)
print(train.default_NotImputed.accuracy)

#Test Data
test.default_NotImputed.pred <- predict(dt.default_NotImputed, newdata = Test_NaReduced, type = "class")
test.default_NotImputed.confMatrix <- table(test.default_NotImputed.pred, Test_NaReduced[, 12])
print(test.default_NotImputed.confMatrix)

test.default_NotImputed.accuracy <- sum(diag(test.default_NotImputed.confMatrix))/sum(test.default_NotImputed.confMatrix)
print(test.default_NotImputed.accuracy)

#cp splitted by n=4
cp.manual <- 0.00486855

#pruning
dt.pruned_manually <- prune(dt.default_NotImputed, cp=cp.manual)

summary(dt.pruned_manually)

rpart.plot(dt.pruned_manually)

rpart.rules(dt.pruned_manually)

#Train Data
train.pruned_NotImputed.pred <- predict(dt.pruned_manually, newdata = Train_NaReduced, type = "class")
train.pruned_NotImputed.confMatrix <- table(train.pruned_NotImputed.pred, Train_NaReduced[, 12])
print(train.pruned_NotImputed.confMatrix)

train.pruned_NotImputed.accuracy <- sum(diag(train.pruned_NotImputed.confMatrix))/sum(train.pruned_NotImputed.confMatrix)
print(train.pruned_NotImputed.accuracy)

#Test Data
test.pruned_NotImputed.pred <- predict(dt.pruned_manually, newdata = Test_NaReduced, type = "class")
test.pruned_NotImputed.confMatrix <- table(test.pruned_NotImputed.pred, Test_NaReduced[, 12])
print(test.pruned_NotImputed.confMatrix)

test.pruned_NotImputed.accuracy <- sum(diag(test.pruned_NotImputed.confMatrix))/sum(test.pruned_NotImputed.confMatrix)
print(test.pruned_NotImputed.accuracy)

#glm
library(ggplot2)

set.seed(7)
glm.fit_NotImputed <- glm(target ~ ., family  = binomial, Train_NaReduced)
summary(glm.fit_NotImputed)

#Prediction & Performance

#Training Accuracy with cut-off from 0.1 to 0.9
c = 0.1
for(i in 1:9){
  pred <- ifelse(predict(glm.fit_NotImputed, type="response")>c,1,0)
  matrix <- table(pred, Train_NaReduced[,12])
  acc[i] <- sum(diag(matrix))/sum(matrix)
  
  pred <- NULL
  matrix <- NULL
  
  c = c + 0.1;
}
acc
acc.train <- data.frame(x=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), y=acc)

ggplot(data=acc.train, aes(x=x, y=y))+
  geom_line()+
  theme_classic()+
  xlab("Cut-Off")+
  ylab("Training-Accuracy")+
  labs(title="Training Accuracy in Bezug auf verschiedene Cut-Off Parameter")

#best Cut-Off
best.acc <- max(acc)
best.acc
best.CutOff <- 0.4

#Train Error
glm.train.predicted <- predict.glm(glm.fit_NotImputed, Train_NaReduced, type = "response")
glm.train.confMatrix <- table(glm.train.predicted > best.CutOff, Train_NaReduced$target)
glm.train.confMatrix
glm.train.accuracy <- sum(diag(glm.train.confMatrix))/sum(glm.train.confMatrix)
print(glm.train.accuracy)


#Test Error
glm.test.predicted <- predict.glm(glm.fit_NotImputed, Test_NaReduced, type = "response")

glm.test.confMatrix <- table(glm.test.predicted > best.CutOff, Test_NaReduced$target)
glm.test.confMatrix
glm.test.accuracy <- sum(diag(glm.test.confMatrix))/sum(glm.test.confMatrix)
print(glm.test.accuracy)

