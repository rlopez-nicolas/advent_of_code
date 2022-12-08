dat<-adventdrob::advent_input(7, 2022, parse = TRUE)

#Function from @drob
cd<-function(path, dir){
  
  if (!is.na(dir)){
    if (dir == ".."){
      
      return(head(path, -1))
    }
    return(c(path, paste0(tail(path, 1), "/", dir)))
  }
  return(path)
}

dat1<- dat %>% 
  extract(x, "dir", "cd (.*)", remove = FALSE) %>% 
  mutate(path = c(accumulate(dir, cd))) %>% 
  unnest(path) %>% 
  filter(str_detect(x, "\\d")) %>% 
  separate(x, c("size", "file"), " ") %>% 
  mutate(size = as.numeric(size)) %>% 
  group_by(path) %>% 
  summarise(total_size = sum(size))


#Puzzle 1

res1<- dat1 %>% 
  filter(total_size <= 100000) %>% 
  pull(total_size) %>% 
  sum()

#Puzzle 2

res2<- dat1 %>% 
  filter(total_size >= (30000000 -(70000000 - .$total_size[1]))) %>% 
  pull(total_size) %>% 
  min()