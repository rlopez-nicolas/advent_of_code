library(tidyverse)

dat<-adventdrob::advent_input(3, 2022, parse = TRUE)

#Puzzle 1

dat1<- dat %>% 
  mutate(nchars = nchar(x)) %>% 
  mutate(compart_1 = substr(x, 1, nchars/2)) %>% 
  mutate(compart_2 = substr(x, nchars/2 + 1, nchars)) %>% 
  mutate(common_letters = map2_chr(map(strsplit(compart_1, ""), unique),
                                   map(strsplit(compart_2, ""), unique),
                                   intersect))

scores_t<- tibble(common_letters = c(letters, LETTERS), scores = c(1:52))

res1<- left_join(dat1, scores_t, by = "common_letters") %>% 
  pull(scores) %>% 
  sum()

#Puzzle 2

dat2<- dat %>% 
  mutate(let = strsplit(x, "")) %>% 
  mutate(group = rep(1:(nrow(.)/3), each = 3))


res2<- dat2 %>% 
  group_by(group) %>% 
  summarize(common_letters = reduce(let, intersect)) %>% 
  left_join(scores_t, by = "common_letters") %>% 
  pull(scores) %>% 
  sum()