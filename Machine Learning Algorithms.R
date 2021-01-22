library(caret)
library(mice)

#Dealing with NAs
#detect variables with more than 5% (threshold) of NAs
detectNA <- function(x){sum(is.na(x))/length(x)*100}
apply(datTrain, 2, detectNA)
md.pattern(datTrain[,2:13])


#Data Preparation
datTrain.casted <- datTrain %>% select(-enrollee_id, -city) %>%
  mutate(city_development_index = as.numeric(city_development_index),
         target = as.factor(target)) %>% as.data.frame() %>%
  mutate_if(is.character, as.factor) #converting all variables with type=char to factor 


#Data Imputation

datTrain.imputed <- mice(datTrain.casted, m = 1, remove.constant = FALSE)
datTrain.imputed$loggedEvents
datTrain.ready <- complete(datTrain.imputed)

apply(datTrain.ready, 2, detectNA)

###Decision tree

library(rpart)
library(rpart.plot)

#cross validation
set.seed(45)
dt.control <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats=3,
                          verboseIter = TRUE)

dt.cv <- train(target ~ ., 
                  data = datTrain.ready,
                  method = "rpart",
                  trControl = dt.control,
                  metric = "Accuracy")

dt.cv

#return final model
cp.best <- dt.cv$bestTune$cp

dt.final <- dt.cv$finalModel
dt.final$variable.importance
summary(dt.final)

prp(dt.final, box.palette = "Reds", tweak =1)


#predict

dt.pred <- predict.train(dt.cv, datTrain.ready)
confusionMatrix(pred, datTrain.ready[, 12])


###glm
#dummy variable

dummy_target <- as.numeric(datTrain.ready[11] == 1)
dat.dummy_target <- cbind(datTrain.ready,dummy_target)

set.seed(4)
glm.fit <- glm(target ~ city_development_index, family  = binomial, datTrain.ready)
summary(glm.fit)

varImp(glm.fit, scale = FALSE)

glm.fit.predicted <- predict( 
  object = glm.fit, 
  data   = datTrain.ready, 
  type   = "response"
)

glm.fit.predicted
plot(glm.fit)

plot(
  x   = dat.dummy_target$city_development_index, 
  y   = dat.dummy_target$dummy_target, 
  col = "red"
)
lines(datTrain.ready$city_development_index,glm.fit.predicted, col="blue")
