library(tidyverse)
source("functions/elo_functions.R")

# Script to create tipping model
load_data <- read_csv("/Users/tazza1/Documents/r_projects/afl_model/data/afl_clean.csv") %>% 
  filter(round_type == "Regular") %>% 
  select(-X1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  mutate(outcome = ifelse(outcome == "win", 1, 0)) %>%  #Code draws as wins for the home team
  mutate(elo_diff = home_elo - away_elo,
         distance_diff_log = log(distance_diff+1)) 

# Split data into training and test data
set.seed(23)

#Split randomly
# train <- load_data %>% sample_frac(.75)
# test <- anti_join(load_data, train, by = "id")

#Split by season
train <- load_data %>% filter(season<=2017)
test <- load_data %>% filter(season>=2018)


#Build a simple logistic regression model
model_1 <- glm(outcome ~ elo_diff + distance_diff_log + venue_exp_last_3,
                   family = binomial,
                   data = train)

#Grab intercepts
coeff_intercept <- model_1$coefficients["(Intercept)"]
coeff_elo_diff <- model_1$coefficients["elo_diff"]
coeff_distance_diff_log <- model_1$coefficients["distance_diff_log"]
coeff_venue_exp_last_3<- model_1$coefficients["venue_exp_last_3"]

#function to convert logistic output to probability 
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#Calculate win probability
train <- train %>% 
  mutate(prediction_logit = coeff_intercept+
           coeff_elo_diff*elo_diff+
           coeff_distance_diff_log*distance_diff_log+
           coeff_venue_exp_last_3*venue_exp_last_3
           ) %>% 
  mutate(prediction_odds = as.numeric(lapply(prediction_logit, logit2prob))) %>% 
  mutate(prediction_outcome = ifelse(prediction_odds>0.5, 1, 0)) %>% 
  mutate(prediction_success = ifelse(prediction_outcome==outcome, 1, 0))

print(paste("The training data is successful",mean(100*train$prediction_success, na.rm = T), " % of the time"))

#Now run model on test data
test <- test %>% 
  mutate(prediction_logit = coeff_intercept+
           coeff_elo_diff*elo_diff+
           coeff_distance_diff_log*distance_diff_log+
           coeff_venue_exp_last_3*venue_exp_last_3
  ) %>% 
  mutate(prediction_odds = as.numeric(lapply(prediction_logit, logit2prob))) %>% 
  mutate(prediction_outcome = ifelse(prediction_odds>0.5, 1, 0)) %>% 
  mutate(prediction_success = ifelse(prediction_outcome==outcome, 1, 0))

print(paste("The test data is successful",mean(100*test$prediction_success, na.rm = T), " % of the time"))

model_coefficients <- coef(model_1) %>% 
  as.data.frame()
write.csv(model_coefficients,"/Users/tazza1/Documents/r_projects/afl_model/data/model_coefficents.csv")

