library(randomForest)

#Data Preparation
datTrainNew <- datTrain %>% select(-enrollee_id, -city, -gender ) %>%
  mutate(city_development_index = as.numeric(city_development_index),
         target = as.factor(target)) %>% as.data.frame() %>%
  mutate_if(is.character, as.factor) #converting all variables with type=char to factor 

#Dealing with NAs
datTrainNA <- na.omit(datTrainNew)

#Model
set.seed(35)
treeModel <- randomForest(target ~ ., datTrainNA, ntree=400, mtry = 3, importance=TRUE)

treeModel

plot(treeModel)

varImpPlot(treeModel,
           sort = T,
           n.var = 5,
           main = "Feature Selection - Top 5 Variables")
