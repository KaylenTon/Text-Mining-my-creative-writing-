library(tidyverse)
library(tidytext)
library(ggplot2)
library(textclean)

MissMorgan <- read.csv("chaptertext.csv")

MissMorgan <- MissMorgan %>% 
  mutate(TEXT = replace_curly_quote(TEXT)) %>%
  mutate(TEXT = replace_contraction(TEXT)) %>% 
  mutate(TEXT = replace_number(TEXT))

custom_stop_words <- tribble(
~word, ~lexicon,
"koffee", "NAME",
"morgan", "NAME",
"terry", "NAME",
"id", "CONTRACTION",
"basil", "NAME",
"aaron", "NAME",
"amelia", "NAME"
)

all_custom_stop_words <- stop_words %>% 
  bind_rows(custom_stop_words)

Act1 <- MissMorgan %>% 
  filter(ACT == 1)

myEar <- Act1 %>% 
  filter(CHAPTER.TITLE == "My Ear") %>%
  unnest_tokens(word, TEXT) %>% 
  anti_join(all_custom_stop_words)

mySanctuary <- Act1 %>% 
  filter(CHAPTER.TITLE == "My Sanctuary") %>%
  unnest_tokens(word, TEXT) %>% 
  anti_join(all_custom_stop_words)

word_counts <- Act1 %>% 
  unnest_tokens(word, TEXT) %>% 
  anti_join(all_custom_stop_words) %>% 
  count(word, CHAPTER.TITLE) %>% 
  group_by(CHAPTER.TITLE) %>% 
  ungroup() %>% 
  mutate(word2 = fct_reorder(word, n))
  
# NEED TO DO WORD STEMMING/COMPLETION TO CLEAN THE DATA FURTHER.

Act2 <- MissMorgan %>% 
  filter(ACT == 2)

Act3 <- MissMorgan %>% 
  filter(ACT == 3)