#import libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

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

datFull %>% 
  select(enrolled_university, experience) %>%
  filter(!is.na(enrolled_university),!is.na(experience)) %>%
  ggplot(data=., aes(x=enrolled_university, y=experience))+
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

datFull %>%
  select(training_hours, target) %>%
  filter(!is.na(training_hours),!is.na(target)) %>%
  ggplot(data=., aes(x = training_hours)) +
    geom_histogram(bins = 25, color = "black", fill = "violet", alpha=.7) +
    theme_light() +
    facet_wrap(vars(target))+
    labs(title="Histogramm Trainingsstunden nach target")+
    xlab("Anzahl Trainingsstunden")+
    ylab("Count")

ggplot(data=datFull, aes(x=as.factor(education_level)))+
  geom_bar(data=datFull, aes(fill=as.factor(education_level)))+
  facet_wrap(vars(relevent_experience))+
  theme_light()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Relevante Berufserfahrung in Bezug auf Schulabaschluss")+
  xlab("Schulabschluss")

datFull %>% 
  select(major_discipline, education_level) %>%
  filter(!is.na(major_discipline),!is.na(education_level)) %>%
  ggplot(data=.,aes(x=education_level))+
    geom_bar(aes(fill=education_level))+
    facet_wrap(vars(major_discipline))+
    theme_light()+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title="Schulabschluss in Bezug auf akademische Disziplin", subtitle = "log2 y-Achse")+
    xlab("akademische Disziplin")+
    scale_y_continuous(trans = 'log2')

datFull %>% 
  select(city_development_index, training_hours)%>%
  mutate(city_development_index = fct_lump(as.factor(city_development_index), n = 20)) %>%
  filter(!is.na(city_development_index)) %>%
  ggplot(data= ., aes(x=city_development_index, y=training_hours, fill = city_development_index))+
    geom_col()+
    theme_light()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none")+
    labs(title="Stadtentwicklungsindex und Trainingsstunden", subtitle = "top 20 Stadtentwicklungsindexe")+
    xlab("Stadtentwicklungsindex")+
    ylab("Trainingsstunden")
  
