
dat<-adventdrob::advent_input(6, 2022, parse = TRUE)
dat<- unlist(strsplit(dat$x, ""))

target<- "message"


for (i in 1:length(dat)) {
  
  #Puzzle 1
  if (target == "packet"){  
  
  j<-i+3
  n_char<-4
  
  }
  
  #Puzzle 2
  if (target == "message"){
  
    j<-i+13
    n_char<-14
    
  }
  
  pot_markers<-dat[i:j]
  
  if (length(unique(pot_markers)) == n_char){ 
    print(j)
    break
  }

}