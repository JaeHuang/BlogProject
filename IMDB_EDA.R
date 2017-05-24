library(dplyr)
library(ggplot2)

dataset <- read.csv('movie_metadata.csv', header = TRUE, stringsAsFactors = FALSE)
str(dataset)
head(dataset)

# What are the total number of movies reviewed by year?
movieNumYear <- dataset %>% select(movie_title, title_year) %>% group_by(title_year) %>% summarise(n=n())
movieNumYear <-na.omit(movieNumYear)
head(movieNumYear)

# use ggplot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(movieNumYear, aes(x=movieNumYear$title_year, y=movieNumYear$n))+
  geom_line()+
  geom_smooth(fill=NA)+
  ggtitle("Number of movie by year")+
  labs(x="Year", y="Number of movie")


# What is the average IMDB rating by year?
# put average imdb score by year into temp dataset
avgScoreYear <- dataset %>%
  select(imdb_score, title_year) %>% 
  group_by(title_year) %>% 
  summarise(avg_score=round(mean(imdb_score),1))
avgScoreYear <- na.omit(avgScoreYear)
head(avgScoreYear)

# use ggplot
ggplot(avgScoreYear, aes(x=avgScoreYear$title_year, y=avgScoreYear$avg_score)) +
  ylim(4,10)+
  geom_line()+
  geom_smooth(fill=NA)+
  ggtitle("Average imdb score by year")+
  labs(x="Year", y="avg score")


# How do the average score change for each type of content rating?
avgScoreCont <- dataset %>% 
  select(content_rating, imdb_score) %>% 
  group_by(content_rating) %>% 
  summarise(avg_score=round(mean(imdb_score),1))

avgScoreCont$content_rating[1] <- "Unspecified"
head(avgScoreCont)

# plot histogram of avg score by content rating
ggplot(avgScoreCont, aes(x=avgScoreCont$content_rating, avgScoreCont$avg_score))+
  geom_bar(stat = 'identity') +
  coord_flip()+
  ggtitle("Average imdb rating by content rating")+
  labs(x="content rating", y="imdb score")


# Which director has the highest average IMBD rating? Lets display the top twenty directors.
dirScore <- dataset %>% select(director_name, imdb_score) %>%
  group_by(director_name) %>%
  summarise(avg_score=round(mean(imdb_score),1)) %>%
  arrange(desc(avg_score)) %>%
  top_n(20)

# plot
ggplot(dirScore, aes(x=dirScore$director_name, y=dirScore$avg_score)) +
  geom_bar(stat='identity') +
  coord_flip()+
  ggtitle("IMDB score by Top 20 directors")+
  labs(x="Director name", y="imdb score")

# After 2000
dirScoreRecent <- dataset %>% select(director_name, imdb_score) %>%
  filter(dataset$title_year > 2000)%>%
  group_by(director_name) %>%
  summarise(avg_score=round(mean(imdb_score),1)) %>%
  arrange(desc(avg_score)) %>%
  top_n(20)

ggplot(dirScoreRecent, aes(x=dirScoreRecent$director_name, y=dirScoreRecent$avg_score)) +
  geom_bar(stat='identity') +
  coord_flip()+
  ggtitle("IMDB score by Top 20 directors")+
  labs(x="Director name", y="imdb score")



years = c(2010:2016)
y=2010

for(y in years){
  temp <- dataset %>% 
    select(movie_title, director_name, actor_1_name, gross) %>%
    filter(dataset$title_year==y)
  temp <- temp[!duplicated(temp),]
  temp <- temp %>%
    top_n(3,gross)%>%
    arrange(desc(gross))
  
  print(y)
  print(temp)
  
}

# top gross movie
years = c(2010:2016)
y=2010
for(y in years){
  temp <- dataset %>% 
    select(movie_title, director_name, actor_1_name, gross) %>%
    filter(dataset$title_year==y) %>%
    top_n(3, gross) %>%
    arrange(desc(gross))
  
  noquote(rep('-',30, sep=""))
  print(y)
  print(temp)
}





