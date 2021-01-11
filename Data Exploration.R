library(ggplot2)

netflix_titles <- read_csv("netflix_titles.csv")
save(netflix_titles, file = "Netflix.RData")

show(netflix_titles)

dim(netflix_titles)

head(netflix_titles)
tail(netflix_titles)

summary(netflix_titles)

netflixDatatypes <- netflix_titles
netflixDatatypes$type <- as.factor(netflixDatatypes$type)

ggplot(netflixDatatypes, aes(release_year))+
  geom_histogram(col="black", alpha=.4)+
  labs(title="Histogramm: Erscheinungsjahr")+
  theme_classic()

ggplot(netflixDatatypes, aes(type))+
  geom_bar(col="blue", fill="blue", alpha=.4)+
  labs(title="Barchart: Film/TV Show")+
  theme_classic()

ggplot(netflixDatatypes, aes(x=release_year, y=type))+
  geom_abline()

#test

#test2

#test3