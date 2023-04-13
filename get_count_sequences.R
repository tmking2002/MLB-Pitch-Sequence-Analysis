library(tidyverse)
library(stringr)
library(tictoc)

setwd("~/Desktop/Projects/MLB Pitch Sequence Flowchart/")

source("makeyear.r")
source("parse.retrosheet2.pbp.r")

tic("read data")
parse.retrosheet2.pbp(2022)
setwd("./download.folder/unzipped")
data <- makeyear(2022)
toc()

pitch_sequences <- data %>% 
  mutate(PITCH_SEQ_TX = str_remove_all(PITCH_SEQ_TX,"[^[:alpha:]]+"),
         pitches = paste(str_split(PITCH_SEQ_TX,"")),
         pitches = str_remove_all(pitches, "c|\\(|\\\\|\\)|\\\"")) %>% 
  separate(pitches, paste0("pitch_",1:16), ", ") %>% 
  select(paste0("pitch_",1:16))

get_counts <- function(pitch_sequence){
  balls <- 0
  strikes <- 0
  
  pitch_sequence <- str_remove_all(na.omit(as.character(pitch_sequence)),"N")
  
  count_sequence <- as.data.frame(matrix(nrow = 1,ncol = 32))
  names(count_sequence) <- c(paste0("balls_",1:16), paste0("strikes_",1:16))
  
  for(i in 1:length(pitch_sequence)){
    curr_pitch <- pitch_sequence[i]
    
    balls <- ifelse(curr_pitch %in% c("B","I","P","V"), balls + 1, balls)
    
    if(curr_pitch %in% c("C","F","K","L","M","O","Q","R","S","T")){
      if(strikes == 2 & curr_pitch %in% c("F","R")){
        strikes = strikes
      } else{
        strikes = strikes + 1
      }
    }

    if(curr_pitch %in% c("X","Y")){
      balls <- "In"
      strikes <- "Play"
    } else if(curr_pitch == "H"){
      balls <- "Hit by"
      strikes <- "Pitch"
    }
    
    count_sequence[paste0("balls_",i)] <- balls
    count_sequence[paste0("strikes_",i)] <- strikes
  }
  
  return(count_sequence)
}

count_sequences <- data.frame()

tic()
for(i in 1:10000){
  #if(i %% 1000 == 0){toc(); print(i); tic()}
  count_sequences <- rbind(count_sequences,get_counts(pitch_sequences[i,]))
}
toc()



setwd("~/Desktop/Projects/MLB Pitch Sequence Flowchart/")
write_csv(count_sequences,"Count Sequences.csv")
write_csv(data, "2022 PBP.csv")  
