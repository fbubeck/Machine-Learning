netflix_titles <- read_csv("netflix_titles.csv")
save(netflix_titles, file = "Netflix.RData")

show(netflix_titles)

dim(netflix_titles)

head(netflix_titles)
tail(netflix_titles)

summary(netflix_titles)


