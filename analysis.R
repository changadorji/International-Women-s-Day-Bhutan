setwd("E:/kaggle/news bhutan")
library(readxl)
news <- read_xlsx('news.xlsx')

library(quanteda)
library(tidyverse)
library(tidytext)
library(stringr)
library(RColorBrewer)
theme_set(theme_classic())
library(SnowballC)
library(spacyr)
spacy_initialize('en_core_web_sm')
names(news) <- c('doc_id', 'text')
news_corpus <- corpus(news)
news_summary <- summary(news_corpus)
news_summary %>% 
  pivot_longer(cols = c(2:4), names_to = 'Type', values_to = 'Total') %>% 
  mutate(Type = ifelse(Type=='Tokens', 'Words', 
                       ifelse(Type=='Types', 'Unique Words', 'Sentences'))) %>% 
  ggplot(aes(reorder(Text, Total), Total, fill=Text))+
  geom_bar(stat='identity', show.legend = FALSE)+
  facet_wrap(facets = 'Type', scales = 'free')+
  coord_flip()+
  labs(x='Newspaper',
       y='Total',
       title= 'Total sentences, words and unique words')+
  scale_color_brewer(palette = 'Set3')

textstat_readability(news_corpus, measure = 'Flesch.Kincaid') %>% 
  ggplot(aes(reorder (document, Flesch.Kincaid), Flesch.Kincaid , fill=document))+
  geom_bar(stat='identity', show.legend = FALSE)+
  scale_fill_brewer(palette = 'Dark2')+
  coord_flip()+
  labs(x='Newspaper',
       y = 'Flesch-Kincaid Grade Level',
       title = 'Readibility of the Newpaper')


news_token <- news %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) 
news_token %>% 
  inner_join(get_sentiments('nrc')) %>% 
  count(sentiment, sort = TRUE) %>% 
  ggplot(aes(reorder (sentiment, -n), n, fill=sentiment))+
  geom_bar(stat='identity', show.legend = FALSE)+
  labs(x='Sentiment',
       y = 'Total',
       title = 'Sentiment analysis of newpaper article')



news_name <- spacy_extract_entity(news$text)
news_name[21,3] <- 'PERSON'
news_name1 <- news_name %>% 
  filter(ent_type=='PERSON'|ent_type=='ORG') %>% 
  count(text, sort = TRUE)
news_name1 <- news_name1[-c(1, 4, 6, 8, 10, 12,13,14,16,17,18, 19, 21,22,23),]

news_name1 <- news_name1 %>% 
  mutate(text = str_trim(text), text=gsub('Elise Raymonde Derochoe', 'Elise Raymonde Deroche', text)) %>%
  group_by(text) %>% 
  summarise(n=sum(n))
news_name1 %>% 
  ggplot(aes(reorder (text, n), n, fill=text))+
  geom_bar(stat='identity', show.legend = FALSE)+
  coord_flip()+
  labs(x='Names of the people',
       y='Total',
       title = 'Names mentioned in the report')
  


#codes not used
