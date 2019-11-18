#Lucas Mitchell
#11/12/2019

# install.packages("wordcloud")
# install.packages("textdata")
# install.packages("tidytext")

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


