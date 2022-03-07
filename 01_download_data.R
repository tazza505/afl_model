library("fitzRoy")


#Download 1897 to 2021 AFL game results
temp_df <- list()
for (season in 1897:2021){
  download <- fetch_results_afltables(season)
  temp_df[[season]] <-download 
}
load_data <- do.call(rbind, temp_df)
save(load_data, file = "/Users/tazza1/Documents/r_projects/afl_model/data/afl_historical.rda")
