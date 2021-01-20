#import libraries
library(readr)
library(dplyr)
library(ggplot2)

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

#Plot zu Geschlechterverteilung
ggplot(data=datFull, aes(x=gender))+
  geom_bar(color='blue', fill='blue', alpha=.5)+
  theme_classic()+
  labs(title="Geschlechterverteilung der Teilnehmer")+
  xlab("Geschlecht")+
  ylab("Anzahl")

#Plot der Abschlüsse
ggplot(data=datFull, aes(x=education_level))+
  geom_bar(color='orange', fill='orange', alpha=.5)+
  coord_flip()+
  theme_classic()+
  labs(title="Verteilung der Abschlüsse bei den Teilnehmer")+
  xlab("Art des Abschlusses")+
  ylab("Anzahl")

#Plot Anzahl der Trainngsstunden und Abschluss (Boxplot)
ggplot(data=datFull, aes(x=education_level, y=training_hours))+
  geom_boxplot()+
  theme_classic()+
  labs(title="Abschluss und Anzahl der Trainingsstunden")+
  xlab("Art des Abschlusses")+
  ylab("Anzahl Trainingsstunden")

#Plot Anzahl der Trainngsstunden und Abschluss (Point/Jitter)
ggplot(data=datFull, aes(x=education_level, y=training_hours))+
  geom_jitter(color='black', alpha=.3, size=.9)+
  theme_classic()+
  labs(title="Abschluss und Anzahl der Trainingsstunden")+
  xlab("Art des Abschlusses")+
  ylab("Anzahl Trainingsstunden")

#ersetze < und > durch feste numerische Werte um typecast durchzuführen
datFull$experience <- sub("<1", "0", datFull$experience)
datFull$experience <- sub(">20", "21", datFull$experience)
datFull$experience <- as.numeric(datFull$experience)

ggplot(data=datFull, aes(x=enrolled_university, y=experience))+
  geom_boxplot(fill="grey", alpha=.7)+
  theme_classic()+
  labs(title="Berufserfahrung und Immatrikulatonsstatus")+
  xlab("Immatrikulatonsstatus")+
  ylab("Berufserfahrung")


###
ggplot()+
  geom_histogram(data=subset(datFull, gender == "Male"), aes(x=training_hours), fill="blue", alpha=.4)+
  geom_histogram(data=subset(datFull, gender == "Female"), aes(x=training_hours), fill="red", alpha=.4)+
  geom_histogram(data=subset(datFull, gender == "Other"), aes(x=training_hours), fill="grey", alpha=.4)+
  theme_classic()+
  labs(title="Histogramm Anzahl Trainingsstunden und Geschlecht")+
  xlab("Anzahl Trainingsstunden")+
  ylab("Count")

ggplot(data=datFull, aes(x = training_hours)) +
  geom_histogram(bins = 25, color = "black", fill = "violet", alpha=.7) +
  theme_classic() +
  facet_wrap(vars(target))+
  labs(title="Histogramm Trainingsstunden nach target")+
  xlab("Anzahl Trainingsstunden")+
  ylab("Count")


