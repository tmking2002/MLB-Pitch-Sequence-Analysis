library(tidyverse)
library(gt)
library(gtExtras)

####Create Data####

# Read in data

data <- read_csv("pbp_with_counts_2022.csv") %>% 
  mutate(across(paste0("count_",1:16), str_replace_all, "9-9", "BIP")) %>%  # Replace 9-9 with BIP
  mutate(across(paste0("count_",1:16), str_replace_all, "10-10", "HBP")) %>%  # Replace 10-10 with HBP
  mutate(runner_on_first = !is.na(BASE1_RUN_ID),
         runner_on_second = !is.na(BASE2_RUN_ID),
         runner_on_third = !is.na(BASE3_RUN_ID))

average_props <- read_csv("leage_average_proportions.csv")

# Coerce data from a list to a dataframe for later on

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
      # Finds locations of the count in the dataframe
      
      indices <- which(data == count, arr.ind = T)
      
      # Adds one to the column to reflect looking at the next pitch after the given count
      
      indices[,2] <- indices[,2] + 1
      
      # Creates frequency table of next pitches
      
      freq <- table(data[indices]) %>% 
        as.data.frame()
    }
    
    # Finds proportions and rounds
    
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

#average_props <- find_proportions(data)

#write_csv(average_props,"League Average Proportions.csv")

plot_sequences <- function(props, title){
  
  # Convert props dataframe to differences from league average
  
  props <- merge(average_props %>% rename("avg_prop" = prop, "total_n" = n), props, by = c("start","end"), all = T) %>% 
    mutate(prop = ifelse(is.na(prop),0,prop),
           n = ifelse(is.na(n),0,n),
           diff = prop - avg_prop)
  
  # Creates labels of points in diagram
  
  labels <- c("0-0","1-0","0-1","2-0","1-1","0-2","3-0","2-1","1-2","0-3",
              "4-0","3-1","2-2","1-3","4-1","3-2","2-3","4-2","3-3")
  
  # Initializes coordinates of data points
  
  coords <- data.frame(label = labels,
                       x = c(1/8, 1/4, 1/4, 3/8, 3/8, 3/8, 1/2, 1/2, 1/2, 1/2, 
                             5/8, 5/8, 5/8, 5/8, 3/4, 3/4, 3/4, 7/8, 7/8),
                       y = c(1/2, 3/5, 2/5, 7/10, 1/2, 3/10, 4/5, 3/5, 2/5, 1/5, 9/10,
                             7/10, 1/2, 3/10, 4/5, 3/5, 2/5, 7/10, 1/2))
  
  # If the result is a K, node will be red, blue for BB
  
  coords$result = case_when(substr(coords$label,3,3) == "3" ~ "K",
                            substr(coords$label,1,1) == "4" ~ "BB")
  
  # Start and end points for arrows
  
  plot_guide <- data.frame(start = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,12,12,13,13,16,16),
                           end =   c(2,3,4,5,5,6,7,8,8,9,9,10,11,12,12,13,13,14,15,16,16,17,18,19))
  
  plot_guide$start_count <- coords$label[plot_guide$start]
  plot_guide$end_count <- coords$label[plot_guide$end]
  
  # Initializes plot with nodes
  
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
                   linewidth = abs(props$diff[which(props$start == plot_guide$start_count[i] & props$end == plot_guide$end_count[i])] * 20),
                   color = ifelse(props$diff[which(props$start == plot_guide$start_count[i] & props$end == plot_guide$end_count[i])] > 0, "darkgreen", "red"))
    
    if(substr(plot_guide$start_count[i],3,3) == "2"){
      plot <- plot +
        geom_curve(x = coords$x[plot_guide$start[i]],
                   xend = coords$x[plot_guide$start[i]],
                   y = coords$y[plot_guide$start[i]],
                   yend = coords$y[plot_guide$start[i]] - .1,
                   linewidth = abs(props$diff[which(props$start == plot_guide$start_count[i] & props$end == plot_guide$start_count[i])] * 20),
                   curvature = 1,
                   color = ifelse(props$diff[which(props$start == plot_guide$start_count[i] & props$end == plot_guide$start_count[i])] > 0, "darkgreen", "red")) +
        geom_curve(x = coords$x[plot_guide$start[i]],
                   xend = coords$x[plot_guide$start[i]],
                   y = coords$y[plot_guide$start[i]] - .1,
                   yend = coords$y[plot_guide$start[i]],
                   linewidth = abs(props$diff[which(props$start == plot_guide$start_count[i] & props$end == plot_guide$start_count[i])] * 20),
                   curvature = 1,
                   color = ifelse(props$diff[which(props$start == plot_guide$start_count[i] & props$end == plot_guide$start_count[i])] > 0, "darkgreen", "red"))
      
    }
  }
  
  
  plot <- plot + 
    theme_void() +
    xlim(0, 1) +
    ylim(0, 1) +
    theme(legend.position = 'none') +
    geom_label(x = .75, y = .2, label = paste0("n = ",sum(props$n, na.rm = T))) 
  
  max_diff <- max(abs(props$diff))
  
  table <- props %>% 
    select(start,end,diff) %>% 
    pivot_wider(names_from = end,
                values_from = diff) %>% 
    gt(rowname_col = "start") %>% 
    sub_missing(columns = everything(),
                missing_text = "") %>% 
    tab_header(title = "Start Count") %>% 
    gt_color_rows(2:19, palette = c("red","white","darkgreen"), domain = c(-max_diff, max_diff)) %>% 
    fmt_percent(2:19, decimals = 1) %>% 
    cols_width(everything() ~ px(32)) %>% 
    tab_options(table.font.size = px(9),
                data_row.padding = px(1))

  return(list(plot,table))
}