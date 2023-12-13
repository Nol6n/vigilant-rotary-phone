library(tidyverse)
library(nflreadr)

Whole_data <- load_pbp()


Whole_data <- Whole_data |>
  select(game_date, home_team,	away_team, defteam, yrdln, play_type, td_team,	td_player_name,
         receiver_player_name, rusher_player_name, fixed_drive_result)

write_csv(x = Whole_data, file = 'data/whole_data.csv')

Whole_data |>
  filter((home_team == "CHI" | away_team == "CHI") & td_team == "CHI") %>%
  group_by(player_name = coalesce(receiver_player_name, rusher_player_name)) %>%
  summarize(total_touchdowns = sum(!is.na(td_player_name))) |>
  arrange(desc(total_touchdowns)) |>
  ggplot() +
  aes(x= reorder(player_name, -total_touchdowns), y = total_touchdowns, fill = player_name) |>
  geom_bar(stat = 'identity') +
  theme_bw()
  
  

