library(randomForest)
library(missForest)
library(caret)

#Model Default
set.seed(35)
model.rf.default <- randomForest(target ~ ., datTrain.ready, ntree=200, mtry = 6, importance=TRUE)

model.rf.default

plot(model.rf.default)

varImpPlot(model.rf,
           sort = T,
           n.var = 5,
           main = "Feature Selection - Top 5 Variables")


###
mtry.searched <- tuneRF(datTrain.ready[,-11], datTrain.ready[,11], mtryStart=5, ntreeTry=100, stepFactor=.8, improve=0.001,
                        trace=TRUE, plot=TRUE, doBest=FALSE)

model.rf.tuned <- randomForest(target ~ ., datTrain.ready, ntree=1000, mtry = 2, do.trace=50, importance=TRUE)
model.rf.tuned

plot(model.rf.tuned)

varImpPlot(model.rf.tuned,
           sort = T,
           n.var = 5,
           main = "Feature Selection - Top 5 Variables")


#Using Cross Validation to tune parameters
#Create control function for training with 10 folds and keep 3 folds for training. search method is grid.
control <- trainControl(method='cv', 
                        number=4, 
                        search='grid',
                        verboseIter = TRUE)
#create tunegrid with 15 values from 1:15 for mtry to tunning model. Our train function will change number of entry variable at each split according to tunegrid. 
tunegrid <- expand.grid(.mtry = (3:10)) 

rf.grid <- train(target ~ ., 
                 data = datTrain.ready,
                 method = 'rf',
                 metric = 'Accuracy',
                 tuneGrid = tunegrid,
                 trControl = control,
                 importance = TRUE)

print(rf.grid)
plot(rf.grid)

best.mtry <- rf.grid$bestTune$mtry