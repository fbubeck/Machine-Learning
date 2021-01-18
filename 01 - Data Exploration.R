#import libraries
library(readr)
library(dplyr)

#import CSVs
datTest <- read_csv("aug_test.csv")

datTrain <- read_csv("aug_train.csv")

#join of Train & Test Data to have full Dataset for Exploration
datFull <- full_join(datTest, datTrain) 

#first Impressions of Data
head(datFull)
tail(datFull)
    
summary(datFull)

dim(datFull)

#count NAs in column gender
sum(is.na(datFull$gender))


