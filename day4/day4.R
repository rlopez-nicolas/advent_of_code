library(tidyverse)

dat<-adventdrob::advent_input(4, 2022, parse = TRUE)

#Puzzle 1

dat1<- dat %>% 
  mutate(first_elf = unlist(strsplit(x, ","))[c(seq(1, nrow(.)*2, by = 2))]) %>% 
  mutate(second_elf = unlist(strsplit(x, ","))[c(seq(2, nrow(.)*2, by = 2))]) %>% 
  mutate(first_elf_from = unlist(strsplit(first_elf, "-"))[c(seq(1, nrow(.)*2, by = 2))]) %>% 
  mutate(first_elf_to = unlist(strsplit(first_elf, "-"))[c(seq(2, nrow(.)*2, by = 2))]) %>% 
  mutate(second_elf_from = unlist(strsplit(second_elf, "-"))[c(seq(1, nrow(.)*2, by = 2))]) %>% 
  mutate(second_elf_to = unlist(strsplit(second_elf, "-"))[c(seq(2, nrow(.)*2, by = 2))]) %>% 
  mutate_at(c("first_elf_from", "first_elf_to", "second_elf_from", "second_elf_to"), as.numeric) %>% 
  mutate(narrower_equal = ifelse(first_elf_to - first_elf_from >= second_elf_to - second_elf_from, "second", "first")) %>% 
  mutate(fully_cont = ifelse(ifelse(narrower_equal == "first", second_elf_from <= first_elf_from & second_elf_to >= first_elf_to,
                                    first_elf_from <= second_elf_from & first_elf_to >= second_elf_to), "yes", "no"))

res1<- dat1 %>% 
  group_by(fully_cont) %>% 
  tally()


#Puzzle 2

dat2<- dat1 %>% 
  mutate(to_greater = ifelse(first_elf_to > second_elf_to, "first", "second")) %>% 
  mutate(overlap = ifelse(ifelse(to_greater == "first", first_elf_from <= second_elf_to, second_elf_from <= first_elf_to), "yes", "no"))


res2<- dat2 %>% 
  group_by(overlap) %>% 
  tally()