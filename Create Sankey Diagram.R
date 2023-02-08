library(tidyverse)
library(reshape2)

setwd("~/Desktop/Projects/MLB Pitch Sequence Flowchart")

data <- read_csv("2022 PBP with Counts.csv") %>% 
  mutate(across(paste0("count_",1:16), str_replace_all, "9-9", "BIP")) %>% 
  mutate(across(paste0("count_",1:16), str_replace_all, "10-10", "HBP"))

x <- as.list(data)
y <- do.call(cbind, x)
data <- as.data.frame(y)

find_proportions <- function(data, count){
  pitches <- sum(as.numeric(str_split(count,"-")[[1]]))
  
  if(count == "0-0"){
    freq <- table(data["count_1"]) %>% 
      as.data.frame()
  } else{
    indices <- which(data == count, arr.ind = T)
    
    indices[,2] <- indices[,2] + 1
    
    test <- data[indices]
    
    freq <- table(data[indices]) %>% 
      as.data.frame()
  }
  
  freq <- freq %>% 
    mutate(Prop = round(Freq / (sum(Freq)),3))
  
  for(i in 1:nrow(freq)){
    df <- data.frame(start = count,
                     end = freq[,1],
                     prop = freq$Prop)
  }
  
  return(df)
}

props <- data.frame()

counts <- c("0-0","0-1","0-2","1-0","1-1","1-2","2-0","2-1","2-2","3-0","3-1","3-2")

for(i in counts){
  props <- rbind(props, find_proportions(data, i))
}

props <- props %>% filter(!(end %in% c("HBP","BIP")))

labels <- c("0-0","1-0","0-1","2-0","1-1","0-2","3-0","2-1","1-2","0-3",
            "4-0","3-1","2-2","1-3","4-1","3-2","2-3","4-2","3-3")

coords <- data.frame(label = labels,
                     x = c(1/8, 1/4, 1/4, 3/8, 3/8, 3/8, 1/2, 1/2, 1/2, 1/2, 
                           5/8, 5/8, 5/8, 5/8, 3/4, 3/4, 3/4, 7/8, 7/8),
                     y = c(1/2, 3/5, 2/5, 7/10, 1/2, 3/10, 4/5, 3/5, 2/5, 1/5, 9/10,
                           7/10, 1/2, 3/10, 4/5, 3/5, 2/5, 7/10, 1/2))

coords$result = case_when(substr(coords$label,3,3) == "3" ~ "K",
                          substr(coords$label,1,1) == "4" ~ "BB")

plot_guide <- data.frame(start = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,12,12,13,13,16,16),
                         end =   c(2,3,4,5,5,6,7,8,8,9,9,10,11,12,12,13,13,14,15,16,16,17,18,19))

plot <- ggplot(coords, aes(x = x, y = y, label = labels)) +
  geom_point(size = 5, aes(color = result)) + 
  scale_color_manual(values = c("K" = "red",
                                "BB" = "blue",
                                "NA" = "grey")) +
  geom_text(size = 5, vjust = 2.5)

for(i in 1:nrow(plot_guide)){
  
  plot <- plot + 
    geom_segment(x = coords$x[plot_guide$start[i]],
                 xend = coords$x[plot_guide$end[i]],
                 y = coords$y[plot_guide$start[i]],
                 yend = coords$y[plot_guide$end[i]])
                 #linewidth = props$prop[which(props$start == coords$label[plot_guide$start[i]] & props$end == "1-0")] * 5)
  
}
  


  theme_void() +
  xlim(0, 1) +
  ylim(0, 1)

plot


