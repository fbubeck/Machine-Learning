library(randomForest)
library(missForest)

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


#Model
set.seed(35)
treeModel <- randomForest(target ~ ., datTrainNA, ntree=400, mtry = 3, importance=TRUE)

treeModel

plot(treeModel)

varImpPlot(treeModel,
           sort = T,
           n.var = 5,
           main = "Feature Selection - Top 5 Variables")
