source("Processing.R")


text_df <- data %>% 
  unnest_tokens(word,lyrics) %>% 
  filter(word %notin% c("b","e","a","g","r","s","i","v","la")) %>% 
  mutate(linenumber = row_number())

data("stop_words")

tidy_df <- text_df %>% 
  anti_join(stop_words) %>% 
  filter(stringr::str_detect(word,"[a-z`]$"),
         !word %in% stop_words$word)
 
wc <- tidy_df %>% 
  group_by(Title) %>% 
  count(word, sort = TRUE) %>%
  filter(n>10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) 

  ggplot(wc,aes(word, n)) +
  geom_col()+
  xlab(NULL) +
  coord_flip()+
  #facet_wrap( ~ Album, scales = "free_y")+
  theme_ipsum()+
  ggtitle("Most Common Words - All Faith No More Studio Albums",
          subtitle = "Excludes We Care A Lot (Introduce Yourself) & cover versions")+
    theme(legend.position = "bottom")
ggsave("2017-10-22-Most-Common-Words.png", width = 10, height =8)

wc_album <-   tidy_df %>% 
  group_by(Album) %>%
  count(word, sort = TRUE) %>%
  filter(n>10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) 


ggplot(wc_album,aes(word, n, fill=Album)) +
  geom_col(show.legend = FALSE)+
  xlab(NULL) +
  ylab(NULL)+
  coord_flip()+
  facet_wrap( ~ Album, scales = "free_y", ncol=3)+
  theme_ipsum()+
  scale_fill_ipsum()+
  ggtitle("Most Common Words - All Faith No More Studio Albums",
          subtitle = "Frequency >10. Excludes We Care A Lot (Introduce Yourself)")+
  theme(legend.position = "bottom")

ggsave("2017-10-22-Most-Common-Words-by-Album.png",width = 11,height=6)
  

## word frequency by song - 
#first - not removing stop words

song_words <- data %>%
  unnest_tokens(word, lyrics) %>%
  count(Title, word, sort = TRUE) %>%
  filter(n>30) %>% 
  ungroup()

ggplot(song_words,aes(reorder(word, n),n, fill=Title)) +
  geom_col()+
  xlab(NULL) +
  ylab(NULL)+
  coord_flip()+
  theme_ipsum()+
  scale_fill_ipsum()+
  #scale_fill_viridis(option ="C", discrete = TRUE)+
  ggtitle("Most Common Words - All Faith No More Studio Songs",
          subtitle = "Frequency >30. Including common stop words")+
  theme(legend.position = "bottom")
ggsave("2017-10-22-Most-Common-Words-by-Song-Including-Stop-Words.png",width = 7, height = 5)

## now remove stop words and the letters from
# "Be Aggressive"

song_words_filtered <-  data %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>% 
  filter(stringr::str_detect(word,"[a-z`]$"),
         !word %in% stop_words$word) %>% 
  filter(word %notin% c("b","e","a","g","r","s","i","v","la")) %>% 
  count(Title, word, sort = TRUE) %>%
  filter(n>20) %>% 
  ungroup()
  
ggplot(song_words_filtered,aes(reorder(word, n),n, fill=Title)) +
  geom_col()+
  xlab(NULL) +
  ylab(NULL)+
  coord_flip()+
  theme_ipsum()+
  scale_fill_ipsum()+
  ggtitle("Most Common Words - All Faith No More Studio Songs",
          subtitle = "Frequency >30. Excluding common stop words")+
  theme(legend.position = "bottom") 
ggsave("2017-10-22-Most-Common-Words-by-Song-Excluding-Stop-Words.png",width = 7, height = 5)
  
  







  