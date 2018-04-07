install.packages("zoo")
# Load packages
library(tidytext)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(stringr)
library(ggplot2)
library(scales)
library(zoo)
--------------------------------------------------------------------------------------------------------------------------------------------
# Set working directory
setwd("C:/Users/pawel/Desktop/Masters Degree/1_MSc Business Intelligence/2nd Semester/Social Network Analytics and Text Mining/0_Project")
# Load data
twitter <- readRDS("trumptweets-1515775693.rds")
is.data.frame(twitter)
# Arrange data and select interesting variables
twitter_data <- twitter %>%
  arrange(created_at) %>%
  select(created_at,screen_name, text, source)
# Create index variable
twitter_1 <- twitter_data %>%
  mutate(index = row_number())
# break the text into individual tokens
twitter_2 <- twitter_1 %>%
    unnest_tokens(word, text)
# remove stop words (not useful for analysis)
data("stop_words")
twitter_3 <- twitter_2 %>%
  anti_join(stop_words)
# get lexicon "afinn"
sentiment_01 <- get_sentiments("afinn")
# keep only words that are in "afinn"
twitter_3 <- twitter_3 %>%
  inner_join(sentiment_01)
# check the most popular words all time
popular_words_all_time <- twitter_3 %>%
  count(word, sort = TRUE) %>%
  filter(n > 100)
--------------------------------------------------------------------------------------------------------------------------------------------
# Check the most popular words for each year

  str(twitter_3)
# Convert POSIXct to Date
  twitter_3$created_at <- as.Date(twitter_3$created_at)
# Create a different dataset for this analysis 
  popular_words_years <- twitter_3
# Convert date to years
  popular_words_years$created_at <- year(popular_words_years$created_at)
# Get top 5 words for each year
  popular_words_years <- popular_words_years %>%
     group_by(created_at) %>%
      count(word) %>%
        mutate(proportion = n/sum(n)) %>%
        top_n(n=5)
# get score from lexicon
  popular_words_years <- popular_words_years %>%
    inner_join(sentiment_01)
# make a plot that shows top 5 words and their score for eac year
  ggplot(popular_words_years, aes(word, n, fill = score)) +
    geom_col() + coord_flip()+ facet_wrap(~created_at, scales = "free")

# Make a plot how many tweets per year
  
twitter_4 <- twitter_1
str(twitter_4)

twitter_4$year <- year(twitter_4$created_at)

twitter_4 <- twitter_4 %>%  
group_by(year) %>%
   count(year)
  
 ggplot(test1, aes(x=year, y=n))+ geom_path()
 ggplot(test1, aes(x=year, y=n))+ geom_step()
  --------------------------------------------------------------------------------------------------------------------------------------------


twitter_3 <- twitter_2 %>%
  inner_join(sentiment_1) %>%
  mutate(year = year(created_at))

twitter_3$year <-as.character(twitter_3$year)

twitter_3 %>% 
ggplot(aes(sentiment))+ geom_bar()+
  facet_wrap(~year, scales = "free_y")

twitter_3$year <-as.character(twitter_3$year)

twitter_3$sentiment <- as.factor(twitter_3$sentiment)
