library(tidyverse)

dat<-adventdrob::advent_input(8, 2022, parse = TRUE)

grid<- dat %>% 
  mutate(grid = str_split(x, "", simplify = TRUE)) %>% 
  select(-x) %>% 
  pull(grid) %>% 
  as.integer() %>% 
  matrix(nrow = nrow(dat), ncol = nchar(dat$x[1]))


res_grid<-matrix(TRUE, nrow = nrow(grid), ncol = ncol(grid))
res_grid[2:(nrow(grid) - 1), 2:(ncol(grid) - 1)]<- FALSE

res_grid2<- matrix(0, nrow = nrow(grid), ncol = ncol(grid))


for (rws in 2:(nrow(grid) - 1)) {
  
  for (cls in 2:(ncol(grid) - 1)) {
    
    #Puzzle 1
    tmp_top<-grid[1:(rws-1), cls]
    tmp_bottom<-grid[(rws+1):nrow(grid), cls]
    tmp_left<-grid[rws, 1:(cls-1)]
    tmp_right<- grid[rws, (cls+1):ncol(grid)]
    tmp_value<- grid[rws, cls]

    res_grid[rws, cls]<- ifelse(max(tmp_top) < tmp_value | max(tmp_bottom) < tmp_value | max(tmp_left) < tmp_value | max(tmp_right) < tmp_value,
                                TRUE, res_grid[rws, cls])
    
    #Puzzle 2
    score_top<-detect_index(rev(tmp_top), ~ .x >= grid[rws, cls]) %>% ifelse(. == 0, length(tmp_top), .)
    score_bottom<-detect_index(tmp_bottom, ~ .x >= grid[rws, cls]) %>% ifelse(. == 0, length(tmp_bottom), .)
    score_left<-detect_index(rev(tmp_left), ~ .x >= grid[rws, cls]) %>% ifelse(. == 0, length(tmp_left), .)
    score_right<-detect_index(tmp_right, ~ .x >= grid[rws, cls]) %>% ifelse(. == 0, length(tmp_right), .)
    
    res_grid2[rws, cls]<- score_top * score_bottom * score_left * score_right
    
  }
  
}

res1<- sum(res_grid)
res2<- max(res_grid2)