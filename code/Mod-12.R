#Lucas Mitchell
#11/12/2019

 install.packages("wordcloud")
 install.packages("textdata")
 install.packages("tidytext")

# clear workspace
rm(list=ls(all=TRUE))

library(tidytext)
library(tidyverse)
library(textdata)
library(wordcloud)
library(lubridate)

tweet <- read_csv("homework-files/data/hurricane-harvey-tweets.csv")

tw <- tweet %>%
  group_by(datetime)%>%
  count()

ggplot(tw)+
  geom_point(mapping = aes(datetime,n), alpha = .5)+
  geom_vline(xintercept = ymd_hms("2017-08-26 3:00:00"), color = "red")+
  theme_bw()

#------------------------------------------------------------------------


new_stops <- c("hurricane", "harvey", "hurricaneharvey", "http", "https", "html", "ift.tt", "pic.twitter.com", "twitter.com", "fb.me", "bit.ly", "dlvr.it", "youtube", "youtu.be")
new_stop <- stop_words %>%
  add_row(word = new_stops )

word <- tweet%>%
  unnest_tokens(word, tweet)%>%
  anti_join(new_stop)%>%
  count(word, sort = TRUE)%>%
  top_n(20)%>%
  mutate(word = reorder(word,n))

ggplot(word)+
  geom_col(mapping = aes(word, n))+
  coord_flip()


#---------------------------------------------------------------------------


ref_tweet <- tweet%>%
  filter(str_detect(tweet, c("refinery", "refineries")))

ref_tweet%>%
  unnest_tokens(word, tweet)%>%
  anti_join(new_stop)%>%
  count(word, sort = TRUE)%>%
  with(wordcloud(word, n, max.words = 100))


#---------------------------------------------------------------------------

get_sentiments("afinn")

f_tweet <- tweet %>%
  group_by(date)%>%
  unnest_tokens(word, tweet)%>%
  count(word, sort = TRUE)

tweet_sent <- f_tweet%>%
  inner_join(get_sentiments("afinn"))%>%
  summarize(mean = mean(value))

tweet_sent %>%
  ggplot()+
  geom_col(mapping = aes(date, mean))
