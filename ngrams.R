
#tri-grams of lyrics
lyric_trigrams <- data %>%
  unnest_tokens(trigram, lyrics, token = "ngrams", n = 3)

lyric_trigrams %>% 
  count(trigram, sort = TRUE)

##remove stop words
trigrams_separated <- lyric_trigrams %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) 

# new trigram counts:
trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

trigram_counts  



## look at negating words
trigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)


AFINN <- get_sentiments("afinn")

not_words <- trigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word",word3 ="word")) %>%
  count(word2,word3, score, sort = TRUE) %>%
  ungroup()

knitr::kable(not_words)


#preceeded by 'not' - word 2
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  mutate(word3 = reorder(word3, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  theme_ipsum()+
  scale_fill_ipsum()+
  ggtitle("Trigrams - Words preceded by \"not\"-2nd Word of 3")+
  xlab(NULL) +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()
ggsave("2017-10-22-Negating-words-2-of-3.png")

# repeat for 3rd word in the trigram
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  mutate(word3 = reorder(word3, contribution)) %>%
  ggplot(aes(word3, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  theme_ipsum()+
  scale_fill_ipsum()+
  ggtitle("Trigrams - Words preceded by \"not\"-3rd Word of 3")+
  xlab(NULL) +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()
ggsave("2017-10-22-Negating-words-3-of-3.png")



#use other negation words
negation_words <- c("not", "no", "never", "without")

negated_words <- trigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word",word3="word")) %>%
  count(word1, word2,word3, score, sort = TRUE) %>%
  ungroup()

negated_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  mutate(word3 = reorder(word3, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  theme_ipsum()+
  scale_fill_ipsum()+
  ggtitle("Negative trigrams - Word 2 of 3 - by negating word")+
  xlab("Words preceded by negating words") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()+
  facet_wrap(~word1,scales="free")

ggsave("2017-10-22-Negative-trigrams-Word-2-of-3-by-negating-word.png")


negated_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  mutate(word3 = reorder(word3, contribution)) %>%
  ggplot(aes(word3, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  theme_ipsum()+
  scale_fill_ipsum()+
  ggtitle("Negative trigrams - Word 3 of 3 - by negating word")+
  xlab("Words preceded by negating words") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()+
  facet_wrap(~word1,scales="free")

ggsave("2017-10-22-Negative-trigrams-Word-3-of-3-by-negating-word.png")



#network plot

library(igraph)
trigram_counts

trigram_graph <- trigram_counts %>%
  filter(n==1) %>% 
  graph_from_data_frame()



library(ggraph)
set.seed(456)

ggraph(trigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  theme_void()
ggsave("2017-10-22-correlation-plot.png")
