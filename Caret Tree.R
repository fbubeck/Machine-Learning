#cross validation
set.seed(45)
dt.control <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats=3,
                           verboseIter = TRUE)

dt.cv <- train(target ~ ., 
               data = Train,
               method = "rpart2",
               trControl = dt.control)

dt.cv

#return final model
cp.best <- dt.cv$bestTune$cp

dt.final <- dt.cv$finalModel
dt.final$variable.importance
summary(dt.final)

rpart.plot(dt.final)


#predict

dt.pred <- predict.train(dt.cv, Test)
confusionMatrix(dt.pred, Test[, 12])
