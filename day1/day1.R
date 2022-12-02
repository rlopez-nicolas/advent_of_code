library(tidyverse)

dat<-adventdrob::advent_input(1, 2022, parse = TRUE)

#Puzzle 1

cal_per_elf<- dat %>% 
  mutate(elf = cumsum(is.na(x))) %>% 
  na.omit() %>% 
  group_by(elf) %>% 
  summarise(total = sum(x))

res1<- max(cal_per_elf$total)

#Puzzle 2

res2<- cal_per_elf %>% 
  arrange(desc(total)) %>% 
  slice(1:3) %>% 
  pull(total) %>% 
  sum()
