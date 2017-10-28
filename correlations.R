library(widyr)
album_word_cors <- album_words %>% 
  pairwise_cor(Album,word,n, sort= TRUE)

album_word_cors


library(ggraph)
library(igraph)
set.seed(1234)

album_word_cors %>%
  filter(correlation > .05) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
ggsave("2017-10-22-album-word-correlations.png",width=8, height =5)
  
