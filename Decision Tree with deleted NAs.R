library(randomForest)
library(missForest)
library(caret)
library(rpart)
library(rpart.plot)

#Dealing with NAs
#detect variables with more than 5% (threshold) of NAs
detectNA <- function(x){sum(is.na(x))/length(x)*100}
apply(datTrain, 2, detectNA)

#create Dataset with 0 NAs
datTrain.cleared <- na.omit(datTrain)

#Data Preparation

datTrain.cleared.ready <- datTrain.cleared %>% select(-enrollee_id, -city) %>%
  mutate(city_development_index = as.numeric(city_development_index),
         target = as.factor(target)) %>% as.data.frame() %>%
  mutate_if(is.character, as.factor) #converting all variables with type=char to factor

###Decision tree

#cross validation

set.seed(567)
dt.control <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats=3,
                           verboseIter = TRUE)

dt.cv2 <- train(target ~ ., 
                data = datTrain.cleared.ready,
                method = "rpart",
                trControl = dt.control,
                tuneLength = 10)

dt.cv2

#return final model
cp.best2 <- dt.cv2$bestTune$cp

dt.final2 <- dt.cv2$finalModel
dt.final2$variable.importance
summary(dt.final2)

prp(dt.final2, box.palette = "Reds", tweak =1)


#predict

pred2 <- predict.train(dt.cv2, datTrain.cleared.ready)
confusionMatrix(pred2, datTrain.cleared.ready[, 12])


###glm
#dummy variable

dummy_target <- as.numeric( datTrain.ready[11] == 1)
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
