library(tidyverse)
library(gt)

####Create Data####

data <- read_csv("2022 PBP with Counts.csv") %>% 
  mutate(across(paste0("count_",1:16), str_replace_all, "9-9", "BIP")) %>% 
  mutate(across(paste0("count_",1:16), str_replace_all, "10-10", "HBP"))

x <- as.list(data)
y <- do.call(cbind, x)
data <- as.data.frame(y)

find_proportions <- function(data){
  
  props <- data.frame()
  
  counts <- c("0-0","0-1","0-2","1-0","1-1","1-2","2-0","2-1","2-2","3-0","3-1","3-2")
  
  for(count in counts){
    
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
                       prop = freq$Prop,
                       n = freq$Freq)
    }
  
    props <- rbind(props, df)
  }
  
  return(props %>% filter(!(end %in% c("HBP","BIP"))))
}

average_props <- find_proportions(data)


plot_sequences <- function(props, title){
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
  
  plot_guide$start_count <- coords$label[plot_guide$start]
  plot_guide$end_count <- coords$label[plot_guide$end]
  
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
                   yend = coords$y[plot_guide$end[i]],
                   linewidth = props$prop[which(props$start == plot_guide$start_count[i] & props$end == plot_guide$end_count[i])] * 5)
    
    if(substr(plot_guide$start_count[i],3,3) == "2"){
      plot <- plot +
        geom_curve(x = coords$x[plot_guide$start[i]],
                   xend = coords$x[plot_guide$start[i]],
                   y = coords$y[plot_guide$start[i]],
                   yend = coords$y[plot_guide$start[i]] - .1,
                   linewidth = props$prop[which(props$start == plot_guide$start_count[i] & props$end == plot_guide$start_count[i])] * 5,
                   curvature = 1) +
        geom_curve(x = coords$x[plot_guide$start[i]],
                   xend = coords$x[plot_guide$start[i]],
                   y = coords$y[plot_guide$start[i]] - .1,
                   yend = coords$y[plot_guide$start[i]],
                   linewidth = props$prop[which(props$start == plot_guide$start_count[i] & props$end == plot_guide$start_count[i])] * 5,
                   curvature = 1)
      
    }
  }
  
  
  plot <- plot + 
    theme_void() +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5))
  
  table <- props %>%
    mutate(diff = prop - average_props$prop[which(average_props$start == start & average_props$end == end)]) %>% 
    gt() %>% 
    tab_style(style = cell_fill(color = "#FF7276"),
              locations = cells_body(columns = prop,
                                     rows = diff > 0)) %>% 
    tab_style(style = cell_fill(color = "#26F7FD"),
              locations = cells_body(columns = prop,
                                     rows = diff < 0)) %>% 
    fmt_percent(3:4, decimals = 1) %>% 
    cols_hide(diff) %>% 
    tab_header(title = title)

  return(list(plot,table))
}


####Examples####

average_chart <- plot_sequences(average_props,"League Average")[1][[1]]
ggsave("League Average Pitch Sequence Chart.png",average_chart)

average_table <- plot_sequences(average_props,"League Average")[2][[1]]
gtsave(average_table,"League Average Pitch Sequence Table.png")

trout <- data %>% 
  filter(BAT_ID == "troum001")

trout_props <- find_proportions(trout)

trout_props <- trout_props %>% filter(!(end %in% c("HBP","BIP")))

trout_chart <- plot_sequences(trout_props,"Mike Trout")[1][[1]]
ggsave("Mike Trout Pitch Sequence Chart.png",trout_chart)

trout_table <- plot_sequences(trout_props,"Mike Trout")[2][[1]]
gtsave(trout_table,"Mike Trout Pitch Sequence Table.png")

late_game <- data %>% 
  filter(INN_CT == 9 & 
           abs(as.numeric(HOME_SCORE_CT) - as.numeric(AWAY_SCORE_CT)) <= 1)

late_game_props <- find_proportions(late_game)

late_game_props <- late_game_props %>% filter(!(end %in% c("HBP","BIP")))

late_game_chart <- plot_sequences(late_game_props,"Late Game")[1][[1]]
ggsave("Late Game Pitch Sequence Chart.png",late_game_chart)

late_game_table <- plot_sequences(late_game_props,"Late Game")[2][[1]]
gtsave(late_game_table,"Late Game Pitch Sequence Table.png")


####Platoon Effect####

RR <- find_proportions(data %>% filter(PIT_HAND_CD == "R" & BAT_HAND_CD == "R")) %>% rename("RR_prop" = prop, "RR_n" = n)
RL <- find_proportions(data %>% filter(PIT_HAND_CD == "R" & BAT_HAND_CD == "L")) %>% rename("RL_prop" = prop, "RL_n" = n)
LR <- find_proportions(data %>% filter(PIT_HAND_CD == "L" & BAT_HAND_CD == "R")) %>% rename("LR_prop" = prop, "LR_n" = n)
LL <- find_proportions(data %>% filter(PIT_HAND_CD == "L" & BAT_HAND_CD == "L")) %>% rename("LL_prop" = prop, "LL_n" = n)

platoon <- reduce(list(RR,RL,LR,LL),full_join, by = c("start","end")) %>% 
  mutate(avg = (RR_prop * RR_n + RL_prop * RL_n + LR_prop * LR_n + LL_prop * LL_n) / (RR_n + RL_n + LR_n + LL_n))

prop.test(platoon %>% filter(start == "0-0" & end == "0-1") %>% select(ends_with("prop")) * platoon %>% filter(start == "0-0" & end == "0-1") %>% select(ends_with("n")),
          platoon %>% filter(start == "0-0" & end == "0-1") %>% select(ends_with("n")))

# 0-0 to 0-1 is significant with p = 3.074e-09

prop.test(platoon %>% filter(start == "3-0" & end == "4-0") %>% select(ends_with("prop")) * platoon %>% filter(start == "3-0" & end == "4-0") %>% select(ends_with("n")),
          platoon %>% filter(start == "3-0" & end == "4-0") %>% select(ends_with("n")))

# 3-0 to 4-0 is significant with p = 0.02049