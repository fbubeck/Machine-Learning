library(randomForest)
library(missForest)
library(caret)

#Dealing with NAs
#detect variables with more than 5% (threshold) of NAs
detectNA <- function(x){sum(is.na(x))/length(x)*100}
apply(datTrain, 2, detectNA)

#Data Preparation
datTrain.casted <- datTrain %>% select(-enrollee_id, -city, -gender ) %>%
  mutate(city_development_index = as.numeric(city_development_index),
         target = as.factor(target)) %>% as.data.frame() %>%
  mutate_if(is.character, as.factor) #converting all variables with type=char to factor 

#Data Imputation
datTrain.imputed <- missForest(xmis=datTrain.casted, maxiter=5, ntree=10)

datTrain.ready <- datTrain.imputed$ximp
apply(datTrain.ready, 2, detectNA)


#Model Default
set.seed(35)
model.rf <- randomForest(target ~ ., datTrain.ready, ntree=200, mtry = 3, importance=TRUE)

model.rf

plot(model.rf)

varImpPlot(model.rf,
           sort = T,
           n.var = 5,
           main = "Feature Selection - Top 5 Variables")

#Using Cross Validation to tune parameters

#10 fold cross validation
trControl <- trainControl(method = "cv",
                          number = 4,
                          search = "grid")

model.rf.train <- train(target~.,
                    data = datTrain.ready,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)
