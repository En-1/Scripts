library(tm); library(SnowballC)

cor<-Corpus(VectorSource(T$Tweet))
cor<-tm_map(cor,tolower)
cor<-tm_map(cor,removePunctuation)
cor<-tm_map(cor, removeWords, c("apple",stopwords("english")))
cor<-tm_map(cor,stemDocument)
freq<-DocumentTermMatrix(cor)
findFreqTerms(freq, lowfreq = 20)
sparse<-removeSparseTerms(freq, 0.997) #leave words that appear in 0.997*1181=4 or more twets
TM<-as.data.frame(as.matrix(sparse)) #make matrix for model
colnames(TM)<-make.names(colnames(TM)) #make shure that all names are appropriate
colnames(MA) = paste("A", colnames(MA)) #rename columns (add A in the beginnig)
wikiWords = cbind(wordsAdded, wordsRemoved)#combine 2 matrixes

###########################################################################
library(tidyverse); library(ggplot2); library(lubridate); library(tidyr); library(scales); library(tidytext) 
library(broom); library(dslabs)
data("trump_tweets")
campaign_tweets <- trump_tweets %>% extract(source, "source", "Twitter for (.*)") %>%
    filter(source %in% c("Android", "iPhone") & created_at >= ymd("2015-06-17") & created_at < ymd("2016-11-08")) %>%
    filter(!is_retweet) %>%  arrange(created_at)

campaign_tweets %>%  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
    count(source, hour) %>%  group_by(source) %>%  mutate(percent = n / sum(n)) %>%  ungroup %>%
    ggplot(aes(hour, percent, color = source)) +geom_line() +  geom_point() +
    scale_y_continuous(labels = percent_format()) + labs(x = "Hour of day (EST)", y = "% of tweets",color = "")

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- campaign_tweets %>% 
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>% 
    filter(!word %in% stop_words$word & !str_detect(word, "^\\d+$")) %>%
    mutate(word = str_replace(word, "^'", ""))

tweet_words %>% count(word) %>% top_n(10, n) %>% mutate(word = reorder(word, n)) %>% arrange(desc(n))

android_iphone_or <- tweet_words %>% count(word, source) %>% spread(source, n, fill = 0) %>%
    mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% filter(Android+iPhone > 100) %>%arrange(desc(or))
android_iphone_or %>% filter(Android+iPhone > 100) %>%arrange(or)

nrc <- get_sentiments("nrc")
tweet_words %>% left_join(nrc, by = "word") %>%  count(source, sentiment) %>%   spread(source, n) %>%   
    mutate(sentiment = replace_na(sentiment, replace = "none"))->sentiment_counts

sentiment_counts %>% mutate(Android = Android / (sum(Android) - Android) , 
iPhone = iPhone / (sum(iPhone) - iPhone),  or = Android/iPhone) %>%  arrange(desc(or))

android_iphone_or %>% inner_join(nrc) %>% filter(sentiment == "disgust" & Android + iPhone > 10) %>% arrange(desc(or))

