library(tidyverse)
pacman::p_load_current_gh("baseballr")
library(tictoc)

schedule <- mlb_schedule(season = 2022, level_ids = "1") %>% filter(!duplicated(game_pk))

pbp <- data.frame()


tic()
for(i in 1:nrow(schedule)){
  
  pbp <- plyr::rbind.fill(pbp, mlb_pbp(schedule$game_pk[i]) %>% mutate(id = schedule$game_pk[i]))
  
  if(i %% 100 == 0) {toc(i); tic()}
  
}
toc()

write_csv(pbp, "2023 Pitch by Pitch Data.csv")
