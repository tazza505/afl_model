library("tidyverse")
library("vroom")

#Script to count success of 2022 tips

#Download 2022 results
results <- read_csv("/Users/tazza1/Documents/r_projects/afl_model/data/afl_historical.csv") %>% 
  filter(Season == 2022) %>% 
  janitor::clean_names() %>% 
  mutate(match_id = paste0(round, "-", home_team, "-", away_team)) %>% 
  mutate(actual_winner = ifelse(margin>0, home_team, ifelse(margin<0, away_team, "draw"))) %>% 
  mutate(actual_margin = margin) %>% 
  mutate(round = as.numeric(gsub("[^0-9.-]", "", round))) #Remove R frmo round

# Get the files names
all_predictions <- paste0("/Users/tazza1/Documents/r_projects/afl_model/predictions/", list.files("/Users/tazza1/Documents/r_projects/afl_model/predictions/"))
tips <- vroom(all_predictions) %>% 
  select(-c("...1")) %>% 
  mutate(match_id = paste0("R", round, "-", home_team,"-", away_team)) 

  
# Final df

final <- results %>% 
  left_join(tips, by = c("match_id", "round", "home_team", "away_team", "venue", "season")) %>% 
  mutate(correct_tip = ifelse(actual_winner == tip, 1, 0)) 

write_csv(final,"/Users/tazza1/Documents/r_projects/afl_model/data/tip_results.csv")