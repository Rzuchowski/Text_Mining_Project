# LOAD PACKAGES
library(tidytext)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(stringr)
library(ggplot2)
library(scales)
library(zoo)
library(yaml)
library(twitteR)
library(tm)
library(wordcloud)
library (Hmisc)
#-------------------------------------------------------------------------------------------------------------------
  
# SET WORKING DIRECTORY
setwd("C:/Users/pawel/Desktop/Masters Degree/1_MSc Business Intelligence/2nd Semester/01_Social Network Analytics and Text Mining/0_Project")
# LOAD DATA
data <- readRDS("twitter_data.rds")
# ALTERNATE DATA
data <- read.csv("Trump_Tweets_Data.csv")
table(is.na(data))
#-------------------------------------------------------------------------------------------------------------------

# IF CSV DATA IS USED:
# REMOVE ID
data <- data[-7]
# CHECK THE DATA
str(data)
# CHANGE TEXT TO CHARACTER AND CREATED_AT TO DATE
data$text <- as.character(data$text)
data$created_at <- as.Date(data$created_at, "%m/%d/%Y")
#-------------------------------------------------------------------------------------------------------------------

# DEAL WITH MISSING OBSERVATIONS
data$created_at <- as.Date(na.approx(data$created_at))
data$retweet_count <- round(impute(data$retweet_count, mean))
data$favorite_count <- round(impute(data$favorite_count, mean))
data$is_retweet <- impute(data$is_retweet, FALSE)
?impute
table(is.na(data$retweet_count))
table(is.na(data$favorite_count))
table(is.na(data$is_retweet))
# CHECK IF MISSING IS_RETWEET ARE RETWEETS OR NOT
data %>%
  filter(is.na(data$is_retweet)) %>%
  str_extract(text, "^RT")

lol <- as.data.frame(str_extract(data2$text, "^RT"))
#-------------------------------------------------------------------------------------------------------------------

# ADD year and year&month variables
data$yearmonth<- format(data$created_at, "%Y-%m")
data$year <- format(data$created_at, "%Y")
levels(data$source)
# REMOVE SUSPICIOUS SOURCES:
data <- as.data.frame(dplyr::filter(data, grepl('Twitter*', data$source)))
# FREQUENCY PLOT
data %>%
  ggplot(aes(created_at)) +
  geom_freqpoly(binwidth = 1)
data <- data %>%
  filter(created_at > "2012-12-31")
#-------------------------------------------------------------------------------------------------------------------

# CHECK THE DATA
str(data)
# CHANGE SOURCE TO FACTOR
data$source <- as.factor(data$source)
levels(data$source)
# ONLY TWITTER RELATED SOURCES
data <- as.data.frame(dplyr::filter(data, grepl('Twitter*', data$source)))
# ADD year and year&month variables
data$monthyear<- format(data$created_at, "%Y-%m")
data$year <- format(data$created_at, "%Y")
# FREQUENCY PLOT
data %>%
  ggplot(aes(created_at)) +
  geom_freqpoly(binwidth = 1)
# FOCUS ONLY ON DATA AFTER 2012, THAT WHERE TRUMP STARTED TWEETING FO' REAL
data <- data %>%
  filter(created_at > "2012-12-31")
# INDEX VARIABLE
twitter_1 <- data %>%
  arrange(created_at) %>%
  mutate(index = row_number())
# BREAK THE TEXT INTO INVIDIVUAL TOKENS
twitter_2 <- twitter_1 %>%
  unnest_tokens(word, text)
# REMOVE STOP WORDS (NOT USEFUL FOR ANALYSIS)
data("stop_words")
twitter_3 <- twitter_2 %>%
  anti_join(stop_words)
# GET LEXICON "AFINN"
sentiment_01 <- get_sentiments("afinn")
#sentiment_01 <- get_sentiments("bing")
# KEEP ONLY WORDS FROM THE LEXICON
twitter_3 <- twitter_3 %>%
  inner_join(sentiment_01)
#-------------------------------------------------------------------------------------------------------------------
  
# SENTIMENT ANALYSIS - SIGNLE WORDS

#twitter_3<- twitter_3 %>% #if bing lexicon then
  #mutate(score = ifelse(sentiment == "positive", 1,-1))
str(twitter_3)

# GRAPH FOR EVERY DAY
twitter_3 %>%
  ggplot(aes(created_at, score, fill=year)) + geom_col()
# AGGREGATE SCORE, SO WE CAN SEE MEAN SCORE FOR EVERY MONTH
twitter_4 <- aggregate(twitter_3$score, by= list(twitter_3$yearmonth), FUN=mean)
str(twitter_4)
# CHANGE to date
twitter_4$Group.1 <- as.factor(twitter_4$Group.1)
twitter_4$Group.1 <- as.Date(as.yearmon(twitter_4$Group.1))
str(twitter_4)
# CHECK MEAN SCORES FOR EVERY MONTH
twitter_4 %>%
  ggplot(aes(Group.1, x, fill = format(Group.1, "%Y")))+geom_col() + scale_x_date(labels = date_format("%Y-%m"))

#-------------------------------------------------------------------------------------------------------------------

# CHECK HASHTAG
Hashtags <- na.omit(as.data.frame(str_extract(data$text, "#\\S+")))
str(Hashtags)
# CHANGE COLUMN NAME
colnames(Hashtags) <- c("Hashtags")
# SAVE HASHTAGS AS CHARACTERS
Hashtags$Hashtags <- as.character(Hashtags$Hashtags)
# REMOVE PUNCTUATION, MAKE EVERYTHING LOWER CASE AND REMOVE NON ENGLISH LETTERS
Hashtags$Hashtags <- removePunctuation(Hashtags$Hashtags)
Hashtags <- Hashtags[which(!grepl("[^\x01-\x7F]+", Hashtags$Hashtags)),]
Hashtags <- as.data.frame(Hashtags)
Hashtags$Hashtags <- tolower(Hashtags$Hashtags)
# CHECK TOP HASHTAGS
Hashtags %>%
  group_by(Hashtags) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(n=10)
# MAKE A WORDCLOUD
wordcloud(Hashtags$Hashtags, min.freq = 30, random.order = FALSE, scale = c(3, 0.3))
  
