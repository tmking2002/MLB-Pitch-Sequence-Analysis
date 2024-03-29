library(tidyverse)

pbp <- read_csv("pbp_with_counts_2022.csv") %>% 
  mutate(runner_on_first = as.numeric(!is.na(BASE1_RUN_ID)),
         runner_on_second = as.numeric(!is.na(BASE2_RUN_ID)),
         runner_on_third = as.numeric(!is.na(BASE3_RUN_ID)),
         new_runner_first = as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1),
         new_runner_second = as.numeric(RUN2_DEST_ID == 2 | RUN1_DEST_ID == 2 | BAT_DEST_ID == 2),
         new_runner_third = as.numeric(RUN3_DEST_ID == 3 | RUN2_DEST_ID == 3 | RUN1_DEST_ID == 3 | BAT_DEST_ID == 3),
         state = paste0(runner_on_first, runner_on_second, runner_on_third),
         new_state = paste0(new_runner_first, new_runner_second, new_runner_third),
         outs = OUTS_CT,
         new_outs = OUTS_CT + EVENT_OUTS_CT,
         runs = AWAY_SCORE_CT + HOME_SCORE_CT,
         half_inning = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         runs_scored = (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

half_innings <- pbp %>% 
  group_by(half_inning) %>% 
  summarise(outs_inning = sum(EVENT_OUTS_CT),
            runs_inning = sum(runs_scored),
            runs_start = first(runs),
            max_runs = runs_inning + runs_start)

pbp_upd <- inner_join(pbp, half_innings, by = "half_inning")

filtered <- pbp_upd %>% 
  filter((state != new_state | runs_scored > 0) & outs_inning == 3) %>% 
  mutate(runs_roi = max_runs - runs) %>% 
  select(runs_roi, state, outs, PITCH_SEQ_TX, EVENT_TX, paste0("count_",1:16))
  

longer <- filtered %>% 
  pivot_longer(cols = starts_with("count_")) %>% 
  filter(!(value %in% c("9-9", "10-10"))) %>% 
  drop_na(value)

runs <- longer %>% 
  group_by(state, outs, value) %>% 
  summarise(avg = mean(runs_roi),
            count = n())

