library(topicmodels)
## remove the album column

raw <- data %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>% 
  filter(stringr::str_detect(word,"[a-z`]$"),
         !word %in% stop_words$word) %>% 
  filter(word %notin% c("b","e","a","g","r","s","i","v","la")) %>% 
  count(Title, word, sort = TRUE) %>%
  ungroup() %>% 
  select(Title,word,n) %>% 
  mutate(linenumber = row_number())


raw_dtm <- raw %>% 
  cast_dtm(linenumber,word,n)

raw_lda <- LDA(raw_dtm,k=10,control = list(seed = 123))

tidy_raw_lda <- tidy(raw_lda)

top_terms <- tidy_raw_lda %>% 
  group_by(topic) %>% 
  top_n(5,beta) %>% 
  ungroup() %>% 
  arrange(topic,-beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 5 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 5, scales = "free")+
  theme_ipsum()+
  scale_fill_viridis(discrete = TRUE)
ggsave("2017-10-22-Topic_Models.png", width=10, height = 5)


## try for Chuck lyrics only
chuck_raw <- chuck %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>% 
  filter(stringr::str_detect(word,"[a-z`]$"),
         !word %in% stop_words$word) %>% 
  filter(word %notin% c("b","e","a","g","r","s","i","v","la")) %>% 
  count(Title, word, sort = TRUE) %>%
  ungroup() %>% 
  select(Title,word,n) %>% 
  mutate(linenumber = row_number())

raw_dtm_chuck <- chuck_raw %>% 
  cast_dtm(linenumber,word,n)

raw_chuck_lda <- LDA(raw_dtm_chuck,k=10,control = list(seed = 123))

tidy_raw_chuck_lda <- tidy(raw_chuck_lda)

chuck_top_terms <- tidy_raw_chuck_lda %>% 
  group_by(topic) %>% 
  top_n(5,beta) %>% 
  ungroup() %>% 
  arrange(topic,-beta)

chuck_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 5 terms in each LDA topic with Chuck Mosley as vocalist",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 5, scales = "free")+
  theme_ipsum()+
  scale_fill_viridis(discrete = TRUE)
ggsave("2017-10-22-Topic_Models_Chuck.png",width = 10, height = 5)



#now do patton
patton_raw <- patton %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>% 
  filter(stringr::str_detect(word,"[a-z`]$"),
         !word %in% stop_words$word) %>% 
  filter(word %notin% c("b","e","a","g","r","s","i","v","la")) %>% 
  count(Title, word, sort = TRUE) %>%
  ungroup() %>% 
  select(Title,word,n) %>% 
  mutate(linenumber = row_number())

raw_dtm_patton <- patton_raw %>% 
  cast_dtm(linenumber,word,n)

raw_patton_lda <- LDA(raw_dtm_patton,k=10,control = list(seed = 123))

tidy_raw_patton_lda <- tidy(raw_patton_lda)

patton_top_terms <- tidy_raw_patton_lda %>% 
  group_by(topic) %>% 
  top_n(5,beta) %>% 
  ungroup() %>% 
  arrange(topic,-beta)

patton_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 5 terms in each LDA topic with Mike Patton as vocalist",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 5, scales = "free")+
  theme_ipsum()+
  scale_fill_viridis(discrete = TRUE)
ggsave("2017-10-22-Topic_Models_patton.png",width = 10, height = 5)



chuck_top_topic <- chuck_top_terms %>% 
  filter(topic <=5) %>% 
  mutate(vocals= "Chuck Mosely")

patton_top_topic <- patton_top_terms %>% 
  filter(topic <=5) %>% 
  mutate(vocals= "Mike Patton")

combined_topics <- bind_rows(chuck_top_topic, patton_top_topic) %>% 
arrange(vocals, beta)

ggplot(combined_topics,aes(reorder(term,beta),beta, fill = as.factor(vocals))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Terms in the top 5 LDA topics by vocalist",
       x = NULL, y = expression(beta)) +
  facet_wrap(vocals~ topic,ncol = 5, scales = "free_y")+
  theme_ipsum()+
  scale_fill_ipsum()
ggsave("2017-10-22-Top_Topic_Model_by_Vocalist.png",width = 10, height = 5)


#any terms in common?
common <- combined_topics %>% 
  filter(term %in% c("world","love"))

ggplot(common,aes(reorder(term,beta),beta, fill = vocals)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Common terms between vocalists",
       x = NULL, y = expression(beta)) +
  facet_wrap(~term, scales = "free_y")+
  theme_ipsum()+
  scale_fill_ipsum()
ggsave("2017-10-22-Common_Terms_between_Vocalists.png")
