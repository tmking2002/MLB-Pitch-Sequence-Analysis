library(tidyverse)
library(naniar)

setwd("~/Desktop/Projects/MLB Pitch Sequence Flowchart")

pbp <- read_csv("2022 PBP.csv")
counts <- read_csv("Count Sequences.csv")

counts_upd <- counts %>% 
  mutate(count_0 = "0-0") %>% 
  unite(count_1,c(balls_1,strikes_1),sep = "-") %>% 
  unite(count_2,c(balls_2,strikes_2),sep = "-") %>% 
  unite(count_3,c(balls_3,strikes_3),sep = "-") %>% 
  unite(count_4,c(balls_4,strikes_4),sep = "-") %>% 
  unite(count_5,c(balls_5,strikes_5),sep = "-") %>% 
  unite(count_6,c(balls_6,strikes_6),sep = "-") %>% 
  unite(count_7,c(balls_7,strikes_7),sep = "-") %>% 
  unite(count_8,c(balls_8,strikes_8),sep = "-") %>% 
  unite(count_9,c(balls_9,strikes_9),sep = "-") %>% 
  unite(count_10,c(balls_10,strikes_10),sep = "-") %>% 
  unite(count_11,c(balls_11,strikes_11),sep = "-") %>% 
  unite(count_12,c(balls_12,strikes_12),sep = "-") %>% 
  unite(count_13,c(balls_13,strikes_13),sep = "-") %>% 
  unite(count_14,c(balls_14,strikes_14),sep = "-") %>% 
  unite(count_15,c(balls_15,strikes_15),sep = "-") %>% 
  unite(count_16,c(balls_16,strikes_16),sep = "-") %>% 
  replace_with_na_all(condition = ~.x == "NA-NA") %>% 
  select(paste0("count_",0:16))

starting_counts <- c("0-0","0-1","0-2","1-0","1-1","1-2","2-0","2-1","2-2","3-0","3-1","3-2")

find_proportions <- function(data){
  props <- data.frame()
  
  for(count in starting_counts){
    pitches <- sum(as.numeric(str_split(count,"-")[[1]]))
    
    indices <- which(data == count, arr.ind = T)
    
    filtered <- data[indices[,1],pitches + 2] %>% 
      as.data.frame() %>% 
      rename_(end_count = names(.)[1]) %>% 
      filter(!(end_count %in% c("In-Play","Hit by-Pitch",count))) %>% 
      drop_na() %>% 
      mutate(start_count = count,
             end_pitches = as.numeric(substr(end_count,1,1)) + as.numeric(substr(end_count,3,3))) %>% 
      filter(pitches < end_pitches | (substr(start_count,3,3) == 2 & pitches == end_pitches))
    
    table <- table(filtered) %>% 
      prop.table() %>% 
      as.data.frame() %>% 
      select(start_count, end_count, Freq)
    
    
    props <- rbind(props, table)
  }
  
  return(props)
}

league_average <- find_proportions(counts_upd)

by_pitcher <- data.frame()

sample <- pbp[1:10000,] %>% group_by(PIT_ID) %>% summarise(pitches = n()) %>% filter(pitches >= 100) %>% pull(PIT_ID)

binded <- cbind(counts_upd,pbp[1:10000,])

for(i in sample){
  temp <- binded %>% filter(PIT_ID == i)
  
  by_pitcher <- rbind(by_pitcher,find_proportions(temp) %>% mutate(PIT_ID = i))
}

vs_leagueaverage <- by_pitcher %>% 
  group_by(start_count,end_count) %>% 
  mutate(vs_league_average = Freq - league_average$Freq[which(league_average$start_count == start_count &
                                                                league_average$end_count == end_count)])
