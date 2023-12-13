filter_and_count_touchdowns <- function(data, home_team, away_team, td_team, date) {
  filtered_data = data %>%
    mutate(touchdown_occurred = ifelse(!is.na(td_player_name) | !is.na(td_team), "Yes", "No"))
  
  return(filtered_data)
}

