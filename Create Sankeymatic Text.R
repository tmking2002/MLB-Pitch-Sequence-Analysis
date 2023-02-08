library(tidyverse)
library(naniar)

setwd("~/Desktop/Projects/MLB Pitch Sequence Flowchart")

pbp <- read_csv("2022 PBP.csv")
counts <- read_csv("Count Sequences.csv")

counts_upd <- counts %>% 
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
  replace_with_na_all(condition = ~.x == "NA-NA")


find_proportions <- function(data, count, sankey = FALSE){
  pitches <- sum(as.numeric(str_split(count,"-")[[1]]))
  
  if(count == "0-0"){
    freq <- table(data["count_1"]) %>% 
      as.data.frame() %>% 
      filter(count_1 != "0-0" & !(count_1 %in% c("In-Play","Hit by-Pitch")))
  } else{
    temp <- data %>% 
      filter(eval(parse(text = paste0("count_",pitches))) == count & 
               !(eval(parse(text = paste0("count_",pitches + 1))) %in% c(count,"In-Play","Hit by-Pitch")))
    
    freq <- table(temp[paste0("count_",pitches + 1)]) %>% 
      as.data.frame()
  }
  
  freq <- freq %>% 
    mutate(Prop = round(Freq / (sum(Freq)),3))
  
  str <- ""
  
  for(i in 1:nrow(freq)){
    str <- paste0(str, count, " [", freq$Prop[i], "] ", freq[i, paste0("count_", pitches + 1)], "\n")
    
    df <- data.frame(start = rep(count,2),
                    end = freq[,1],
                    prop = freq$Prop)
  }
  
  if(sankey == TRUE){
    return(cat(str))
  } else{
    return(df)
  }
}

data <- cbind(pbp[1:10000,],counts_upd) %>% 
  filter(PIT_ID == "sandp002")

str <- ""

for(i in c("0-0","0-1","0-2","1-0","1-1","1-2","2-0","2-1","2-2","3-0","3-1","3-2")){
  str <- paste0(str, find_proportions(data, i, sankey = TRUE))
}

props <- data.frame()

for(i in c("0-0","0-1","0-2","1-0","1-1","1-2","2-0","2-1","2-2","3-0","3-1","3-2")){
  props <- rbind(props, find_proportions(data, i))
}

