library(tidyverse)

dat<-adventdrob::advent_input(5, 2022, parse = TRUE)


moves<- dat %>% 
  filter(grepl("move", x)) %>%
  extract(x, into = c("n_moves", "from", "to"), 
          regex = "move (\\d+) from (\\d) to (\\d)",
          remove = TRUE,
          convert = TRUE)

stacks<- dat %>% 
  filter(!grepl("\\d+", x)) %>%
  na.omit() %>% 
  mutate(value = str_split_fixed(x, " ", n = 9)) %>% 
  select(-x) %>% 
  mutate(value = replace(value, c(68, 69), c("[T]", "[L]"))) %>% 
  mutate(value = gsub(" ", "", value)) %>% 
  pull(value)
  
  #The last three are wrong, sooo let's hardcode it!

stacks[4,7]<- stacks[5,8]
stacks[5,8]<- ""



stacks[1:5, 8]<- stacks[1:5, 9]
stacks[1:5, 9]<- rep("", 5)

stacks<- lapply(1:ncol(stacks), function(x) rev(stacks[,x]))
stacks<- lapply(stacks, function(x) x[nchar(x) == 3])

cratemover<- 9001

for (i in 1:nrow(moves)) {
  
  to<-moves$to[i]
  from<-moves$from[i]
  n_moves<-moves$n_moves[i]
  
  if (cratemover == 9000){
  #Puzzle 1
  tmp<-rev(tail(stacks[[from]], n_moves))
  }
  if (cratemover == 9001)   {
  #Puzzle 2
  tmp<-tail(stacks[[from]], n_moves)
  }
  stacks[[from]]<- head(stacks[[from]], -n_moves)
  stacks[[to]]<- c(stacks[[to]], tmp)
}
  
res<-unlist(lapply(stacks, tail, 1))