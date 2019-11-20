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

# Groups tweets by date and the counts the how many on that date
tw <- tweet %>%
  group_by(datetime)%>%
  count()

# plots the number of tweets ber day with a vertical line showing when Harvey made landfall
ggplot(tw)+
  geom_point(mapping = aes(datetime,n), alpha = .5)+
  geom_vline(xintercept = ymd_hms("2017-08-26 03:00:00"), color = "red")+
  labs(x = NULL, y = "Number of tweets")+
  theme_bw()

# 2 ------------------------------------------------------------------------

# Adding new stop words to the predefined stop words
new_stops <- c("hurricane", "harvey", "hurricaneharvey", "http", "https", "html", "ift.tt", "pic.twitter.com", "twitter.com", "fb.me", "bit.ly", "dlvr.it", "youtube", "youtu.be")
new_stop <- stop_words %>%
  add_row(word = new_stops )

# removes stop words from each tweet then counts how many times the top 20 words from the tweets
word <- tweet%>%
  unnest_tokens(word, tweet)%>%
  anti_join(new_stop)%>%
  count(word, sort = TRUE)%>%
  top_n(20)%>%
  mutate(word = reorder(word,n))

# Bar plot show how many times the top 20 words where used
ggplot(word)+
  geom_col(mapping = aes(word, n))+
  labs(x = NULL)+
  coord_flip()+
  theme_bw()


# 3 ---------------------------------------------------------------------------

# filters out tweets with the words refinery and refineries in them
ref_tweet <- tweet%>%
  filter(str_detect(tweet, c("refinery", "refineries")))

# Makes word cloud of the top 100 used words from tweets that contain the filtered words
ref_tweet%>%
  unnest_tokens(word, tweet)%>%
  anti_join(new_stop)%>%
  count(word, sort = TRUE)%>%
  with(wordcloud(word, n, max.words = 100))

# From the word cloud we can see that words like price, gas, fuel, and business are rather large indicating they were used often. While words like gulf, impact, and risk are much smaller. The words in the first sentence are more commonly associated with economics than environmental factors, the tweets mostly referred to the economic impact hurricane Harvey had than the environmental.

# 4 ---------------------------------------------------------------------------

sent <- get_sentiments("afinn")

# removes stop  words from tweets and groups by date
f_tweet <- tweet %>%
  unnest_tokens(word, tweet)%>%
  anti_join(new_stop)%>%
  group_by(date)

#gives sentiments to the tweeted words and gives a mean sentiment for that day
tweet_sent <- f_tweet%>%
  inner_join(sent)%>%
  summarize(mean = mean(value))

# plot to visualized the mean sentiment of tweets per day
tweet_sent %>%
  ggplot()+
  geom_col(mapping = aes(date, mean))+
  scale_x_date(date_breaks = "day", date_labels = "%d")+
  labs(x = "Days in August 2019", y = "Average Sentiment")+
  theme_bw()

