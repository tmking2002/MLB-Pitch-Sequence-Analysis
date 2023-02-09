setwd("~/Desktop/Projects/Pitch Sequence Project/MLB Pitch Sequence Flowchart")
source("Create Line Chart Diagram.R")

####Examples####

average_chart <- plot_sequences(average_props,"League Average")[1][[1]]
ggsave("League Average Pitch Sequence Chart.png",average_chart)

average_table <- plot_sequences(average_props,"League Average")[2][[1]]
gtsave(average_table,"League Average Pitch Sequence Table.png")

trout <- data %>% 
  filter(BAT_ID == "troum001")

trout_props <- find_proportions(trout)

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