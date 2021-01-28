library(caret)

#Dealing with NAs
#detect variables with more than 5% (threshold) of NAs
detectNA <- function(x){sum(is.na(x))/length(x)*100}
apply(datTrain, 2, detectNA)


#Data Preparation
datTrain_casted <- datTrain %>% 
  select(-enrollee_id, -city) %>%
  mutate(city_development_index = as.numeric(city_development_index),
         target = as.factor(target)) %>% 
  as.data.frame() %>%
  mutate_if(is.character, as.factor) #converting all variables with type=char to factor

  

#Data Imputation
library(mice)

impute <- mice(datTrain_casted, m = 1, remove.constant = FALSE) #takes time :)
impute$loggedEvents
datTrain_imputed <- complete(impute)

apply(datTrain_imputed, 2, detectNA)

#Data Balancing
datTrain_imputed %>% 
  group_by(target) %>%
  summarise(no_rows = length(target))

library(groupdata2)

target.balancing <- datTrain_imputed %>% 
  group_by(target) %>%
  summarise(no_rows = length(target))

datTrain_balanced <- upsample(datTrain_imputed, cat_col = "target")

datTrain_balanced %>% 
  group_by(target) %>%
  summarise(no_rows = length(target))

#Test / Training Split
set.seed(3024)
trainingRows <- sort(sample(nrow(datTrain_balanced), nrow(datTrain_balanced)*.7))

Train <- datTrain_balanced[trainingRows,]
Test <- datTrain_balanced[-trainingRows,]

###Decision tree
library(rpart)
library(rpart.plot)

##1st tree
set.seed(34)
dt.default <- rpart(target~ ., data=Train, cp=-1)

printcp(dt.default)
plotcp(dt.default)

#Prediction & Performance
#Train Data
train.default.pred <- predict(dt.default, newdata = Train, type = "class")
train.default.confMatrix <- table(train.default.pred, Train[, 12])
print(train.default.confMatrix)

train.default.accuracy <- sum(diag(train.default.confMatrix))/sum(train.default.confMatrix)
print(train.default.accuracy)

#Test Data
test.default.pred <- predict(dt.default, newdata = Test, type = "class")
test.default.confMatrix <- table(test.default.pred, Test[, 12])
print(test.default.confMatrix)

test.default.accuracy <- sum(diag(test.default.confMatrix))/sum(test.default.confMatrix)
print(test.default.accuracy)

##2nd tree with cp limitation
set.seed(39)
dt.fit <- rpart(target~ ., data=Train, cp=0.005)

summary(dt.fit)

printcp(dt.fit)
plotcp(dt.fit)

#search cp with lowest Cross-Validation Error
xerror.min <- dt.fit$cptable[which.min(dt.fit$cptable[,4]),]
cp.best <- xerror.min[1]
cp.best

#Pruning with best cp
dt.pruned <- prune(dt.fit, cp=cp.best)

summary(dt.pruned)

rpart.plot(dt.pruned)

#Prediction & Performance
#Train Data
train.pred <- predict(dt.pruned, newdata = Train, type = "class")
train.confMatrix <- table(train.pred, Train[, 12])
print(train.confMatrix)

train.accuracy <- sum(diag(train.confMatrix))/sum(train.confMatrix)
print(train.accuracy)

#Test Data
test.pred <- predict(dt.pruned, newdata = Test, type = "class")
test.confMatrix <- table(test.pred, Test[, 12])
print(test.confMatrix)

test.accuracy <- sum(diag(test.confMatrix))/sum(test.confMatrix)
print(test.accuracy)

###glm
#dummy variable

dummy_target <- as.numeric(Train[12] == 1)
dat.dummy_target <- cbind(Train,dummy_target)

set.seed(4)
glm.fit <- glm(target ~ ., family  = binomial, Train)
summary(glm.fit)

varImp(glm.fit, scale = FALSE)

glm.fit.predicted <- predict( 
  object = glm.fit, 
  data   = Test, 
  type   = "response"
)

glm.fit.predicted
plot(glm.fit)

plot(
  x   = dat.dummy_target$city_development_index, 
  y   = dat.dummy_target$dummy_target, 
  col = "red"
)
lines(Train$city_development_index, glm.fit.predicted, col="blue")
