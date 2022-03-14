# Commonly used ELO functions

#---------------------------------------------------------------------------------- 
#   Create elo a
#---------------------------------------------------------------------------------- 
margin_to_elo <- function(margin){
  1/(1+exp(margin*-5/100))
}
elo_to_margin <- function(elo.prob){
  -100/5*log((1/elo.prob)- 1)
}
# Elo rating difference to prob and vice versa
elo_differential_to_prob <- function(elo.rating.difference){
  1 / (1 + 10^((-elo.rating.difference)/400))
}
elo_prob_to_differential <- function(elo.prob){
  400*log10(-elo.prob/(elo.prob - 1))/(log10(2) + log10(5))
}

margin_to_differential <- function(margin){
  (elo.prob.to.differential(margin.to.elo(margin)))
}
  
#function to convert logistic output to probability 
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}



#---------------------------------------------------------------------------------- 
#       Create ELO dataframes
#---------------------------------------------------------------------------------- 
elo_dataframe <- function(elo_name=elo,venue_margin=0.09902038,distance_margin=0.000002707917, 
                          carry_over=0.05, k_val=80 ){
afl_data <<- afl_data %>%  
  mutate(venue_adjustment=margin_to_differential(venue_margin)*Relative_Ground_Experience3,
           distance_adjustment=margin_to_differential(distance_margin)*Distance_Diff
           )  

  elo_model<- elo_run(
    margin_to_elo(Home_Points - Away_Points)~  #<- This is used to determine ELO results in each match
      adjust(Home_Team, venue_adjustment+distance_adjustment) +  # Add HGA to each home team's elo score
      Away_Team+
      group(Round_Identifier)+
      regress(Start_Season, 1500, carry_over),
    k=k_val,
    data=afl_data)
  elo_df<- as_data_frame(elo_model) %>%
    cbind_data_frame(afl_data)
  elo_df <- elo_df %>%
    mutate(margin_actual=Home_Points-Away_Points,
           p_B=1-p_A,
           Home_Win=ifelse(margin_actual>0,TRUE,FALSE),
           elo_A_prior=elo_A-update_A,
           elo_B_prior=elo_B-update_B,
           margin_model = as_numeric(lapply(elo_df$p_A,elo_to_margin)),
           error=margin_model-margin_actual,
           error_sqr = error^2,
           a_win=ifelse(margin_actual>0,1,0),
           correct_winner = ifelse(margin_model>0&margin_actual>0|margin_model<0&margin_actual<0, 1,0),
           elo_difference=elo_A_prior-elo_B_prior) %>%
    rename(elo_A_post = elo_A,
           elo_B_post = elo_B) %>% 
    arrange(Season, Round_Number)
  elo_timeseries <<- elo_df %>%
    select(Date,Season, Round, team_A, team_B, elo_A_post, elo_B_post) %>%
    pivot_longer(cols=c("elo_A_post","elo_B_post"), values_to = "Elo") %>%
    pivot_longer(cols=c("team_A", "team_B"), values_to = "Team", names_to = "home_away") %>%
    mutate(team=as_character(Team)) %>%
    filter((name=="elo_A_post"&home_away=="team_A")|(name=="elo_B_post"&home_away=="team_B")) %>%
    select(Season, Round, Date, Team, Elo) %>%
    mutate(Date=as_Date(Date),
           Season_Round = paste0(Season,Round))

#Assign name to Elo DF
assign(deparse(substitute(elo_name)), elo_df, envir=.GlobalEnv)
}
#---------------------------------------------------------------------------------- 
#      Evaluate ELO model
#---------------------------------------------------------------------------------- 
evaluate_elo <-function(elo_name=elo_df, start=1897){
  tip_season <<- elo_name %>% 
    group_by(Season) %>% 
    summarise(Tips=sum(correct_winner),
              Tip_Percentage=100*mean(correct_winner))
  tip_percentage <<- elo_name %>% 
    filter(Season>=start) %>% 
    summarise(tips=100*mean(correct_winner))
elo_percentage <<- tip_percentage[1,1]
print(paste0("Overall tip percentage: ",round(elo_percentage,1),"%"))
}
  


