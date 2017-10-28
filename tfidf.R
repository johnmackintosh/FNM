#term frequency by album
#shows many words mentioned a few times, few words mentioned often

album_words <- data %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>% 
  filter(stringr::str_detect(word,"[a-z`]$"),
         !word %in% stop_words$word) %>% 
  filter(word %notin% c("b","e","a","g","r","s","i","v","la")) %>% 
  count(Album, word, sort = TRUE) %>%
  ungroup()

total_words <- album_words %>% 
  group_by(Album) %>% 
  summarize(total = sum(n))

all_album_words <- left_join(album_words, total_words)
all_album_words

knitr::kable(head(all_album_words))

ggplot(all_album_words, aes(n/total, fill = Album)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA,0.08)+
  facet_wrap(~Album, ncol = 2, scales = "free_y")+
  theme_ipsum()+
  scale_fill_ipsum()+
  ggtitle("Term Frequency Distribution by Album")
ggsave("2017-10-22-Term-Frequency-by-Album.png",width= 8, height = 6)



#tf - idf
album_tfidf <- all_album_words %>%
  bind_tf_idf(word, Album, n)
knitr::kable(album_tfidf)

album_tfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
album_tfidf

plot_album_tfidf <- album_tfidf %>%
  group_by(Album) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_album_tfidf %>%
  group_by(Album) %>%
  top_n(10) %>%
  ungroup() %>% 
  ggplot(aes(x=reorder(word,tf_idf), tf_idf, fill = Album)) +
  ggtitle("Highest tf_idf words in Faith No More Album Lyrics",
          subtitle = "Studio albums only - Excludes stop words")+
  labs(x = NULL, y = NULL)+
  geom_col(show.legend = FALSE) +
  facet_wrap(~Album,scales = "free_y")+
  coord_flip()+
  theme_ipsum()+
  scale_fill_ipsum()
ggsave("2017-10-22-Inverse-Term-Document-Frequency-by-Album.png",width=10, height = 11)
