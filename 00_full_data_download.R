#Download 1897 to 2021 AFL game results
temp_df <- list()
for (season in 1897:2021){
  download <- fetch_results_afltables(season)
  temp_df[[season]] <-download 
  print(paste("downloading", season))
}
load_data <- do.call(rbind, temp_df)

write_csv(load_data, file = "/Users/tazza1/Documents/r_projects/afl_model/data/load_raw_data.csv")

