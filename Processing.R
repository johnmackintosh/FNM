library(tidyverse)
library(tidytext)
library(dplyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(extrafont)
library(ggimage)
library(stringr)


`%notin%` = function(x,y) !(x %in% y)

data <- readRDS(file="FNM.Rda") 

data <-  data %>% 
  mutate(linenumber = row_number())  

#remove "We Care A Lot from Introduce Yourself 
# and the naughty one from Sol Invictus"


data <- data %>% 
  filter (!linenumber ==15) %>% 
  # filter(linenumber != 6) %>% 
  filter(!linenumber == 77)

#tidy Titles
data$Title <- stringr::str_replace(data$Title,"http://lyrics.wikia.com/wiki/Faith_No_More:","")


data$Album <- as.factor(as.character(data$Album))
data$Album <- factor(data$Album,levels(data$Album)[c(7,3,6,2,4,1,5)])

#replace special characters in song titles
data$Title[[4]] <- stringr::str_replace(data$Title[[4]],"Why_Do_You_Bother%3F","Why_Do_You_Bother?")
data$Title[[11]] <- stringr::str_replace(data$Title[[11]],"Anne%27s_Song","Anne's_Song")
data$Title[[15]] <- stringr::str_replace(data$Title[[15]],"R_N%27_R","R_N'_R")
data$Title[[22]] <- stringr::str_replace(data$Title[[22]],"Surprise!_You%27re_Dead!","Surprise!_You're_Dead!")
data$Title[[34]] <- stringr::str_replace(data$Title[[34]],"Everything%27s_Ruined","Everything's_Ruined!")


#tidy lyrics
source("lyric_tidying.R")

#remove "I started a joke"and "War Pigs"
data <- data %>% 
  filter(Title %notin% c("War_Pigs","I_Started_A_Joke","Easy")) 


chuck <- data %>% 
  filter(linenumber <=18)

patton <- data %>% 
  filter(linenumber >=19)



album_track_counts <- data %>% group_by(Album) %>% tally()




