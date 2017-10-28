#overall album sentiment  
albumsentiment<- text_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(Album, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

albumsentiment$img <- c("2017-10-22-WCAL.jpg","2017-10-22-IY.jpg",
                        "2017-10-22-TRT.jpg","2017-10-22-AD.jpg","2017-10-22-KFAD.jpg",
                        "2017-10-22-AOTY.jpg","2017-10-22-SI.jpg")



ggplot(albumsentiment, aes(Album, sentiment, fill = Album)) +
  #geom_col(show.legend = FALSE) +
  theme_ipsum()+
  ggExtra::removeGrid()+
  coord_flip()+
  geom_image(aes(image=img), size=.15)+
  ggtitle("Overall Sentiment Score by Album",
          subtitle = "Faith No More Studio Albums - Excludes cover versions")+
  labs(x = NULL, y = NULL)+
  geom_text(aes(label=sentiment), hjust=0, nudge_y=7)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave("2017-10-22-Sentiment-by-Album.png")

images <- data.frame(lineup=c("2017-10-22-WCAL-lineup.jpg","2017-10-22-IY-lineup.jpg",
                              "2017-10-22-TRT-lineup.jpg","2017-10-22-AD-lineup.jpg",
                              "2017-10-22-KFAD-lineup.jpg",
                              "2017-10-22-AOTY-lineup.jpg","2017-10-22-SI-lineup.jpg"))
album_data <- albumsentiment %>%
  inner_join(album_track_counts) 
album_data$images <- images$lineup



# number of tracks analysed by album
ggplot(album_data,aes(Album,n, fill=Album))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)+
  theme_ipsum()+
  scale_fill_ipsum()+
  ylab("# Tracks")+
  ggtitle("Number of tracks analysed - by Album")+
  geom_text(aes(label=n), hjust=0, nudge_y=1)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggExtra::removeGrid()
ggsave("2017-10-22-Tracks-Analysed-by-Album.png")



#track sentiment

tracksentiment <- text_df %>%
  group_by(Album) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(Title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(tracksentiment, aes(Title, sentiment, fill= Album)) +
  geom_col(show.legend = FALSE) +
  theme_ipsum()+
  scale_fill_ipsum()+
  labs(x=NULL,y=NULL)+
  ggtitle("Overall Sentiment by Track")+
  coord_flip() +
  facet_wrap(~Album, scales="free", ncol = 4)

ggsave("2017-10-22-Sentiment-by-track.png",width=13, height =6)



