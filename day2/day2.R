library(tidyverse)


#Puzzle 1

dat<-adventdrob::advent_input(2, 2022, parse = TRUE) %>% 
  mutate(opponent = substr(x, 1, 1)) %>% 
  mutate(me = substr(x, 3, 3)) %>% 
  select(-x) 

matrix_scores <- matrix(c(3, 0, 6,
                          6, 3, 0,
                          0, 6, 3), 
                        byrow = TRUE,
                        ncol = 3,
                        dimnames = list(c("X", "Y", "Z"),
                                        c("A", "B", "C")))


dat1<- dat %>% 
  rowwise() %>% 
  mutate(outcome_score = matrix_scores[me, opponent]) %>% 
  mutate(shape_score = case_when(
    me == "X" ~ 1,
    me == "Y" ~ 2,
    me == "Z" ~ 3
  )) %>% 
  mutate(score = outcome_score + shape_score)


res1<- sum(dat1$score)

#Puzzle 2

matrix_decision<- matrix(c("C", "A", "B",
                           "A", "B", "C",
                           "B", "C", "A"),
                         byrow = TRUE,
                         ncol = 3,
                         dimnames = list(c("X", "Y", "Z"),
                                         c("A", "B", "C")))

dimnames(matrix_scores)<- list(c("A", "B", "C"),
                               c("A", "B", "C"))

dat2<- dat %>% 
  rename("expected_outcome" = me) %>% 
  rowwise() %>% 
  mutate(me = matrix_decision[expected_outcome, opponent]) %>% 
  mutate(outcome_score = matrix_scores[me, opponent]) %>% 
  mutate(shape_score = case_when(
    me == "A" ~ 1,
    me == "B" ~ 2,
    me == "C" ~ 3
  )) %>% 
  mutate(score = outcome_score + shape_score)

res1<- sum(dat2$score)