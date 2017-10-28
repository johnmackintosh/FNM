
  raw <- data %>%
    filter(Album == "We_Care_A_Lot") %>% 
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
  
  raw_lda <- LDA(raw_dtm,k=5,control = list(seed = 123))
  
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
    labs(title = "Top terms in each LDA topic - We Care A Lot",
         x = NULL, y = expression(beta)) +
    facet_wrap(~ topic, ncol = 5, scales = "free")+
    theme_ipsum()+
    scale_fill_ipsum()
  ggsave("2017-10-22-Topic_Models-WCAL.png")

