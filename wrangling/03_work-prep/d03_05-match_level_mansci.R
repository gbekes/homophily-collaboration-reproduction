
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams

# d03_05-match_level_mansci.R
#
# This script prepares the data at the team*season-half level aggregate data

# v2.1 2024-11-19
#######################################################



# reads in work data, aggregated as season-half level

data= read_parquet(file = paste0(data_work, "anon_work-player-pair_sh.parquet"),
                   as_data_frame = TRUE)
teams_season_half=read_csv(paste0(data_tidy_created,"anon_teams_season_half.csv"))

#################################################################################################
# team* season-half level aggregation
#################################################################################################
# aggregate to team * season-half

data_tbl<-as_tibble(data)
teams_season_half_aggregated<-data_tbl  %>%
  group_by(anon_teamid, season_name_half) %>%
  summarise(
    across(c(pass_count, minutes_shared, 
             passes_in_shared_mins, passes_in_shared_mins1, passes_in_shared_mins2), sum),
    across(c(value_p1, value_p2), mean),
    across(c(season_length), mean),
    across(c(pass_per_min), mean),
    across(c(season_name, league_name, season_half,
             league_ctry, league_ctry_f, league_season, league_season_f ), first)
  ) 

glimpse(teams_season_half_aggregated)

# join            
teams_season_half_aggregated  %<>%
  left_join(teams_season_half, 
            by=c("league_season","season_half" , "anon_teamid"))



## add new vars
teams_season_half_aggregated  %<>%
  mutate(
    ln_value_p1=log(value_p1 ),
    ln_value_p2=log(value_p2 ),
    ln_pass_count=log(pass_count),
    ln_pass_count_per_game=log(pass_count/season_length),
    ln_minutes_shared=log(minutes_shared ),
    ln_passes_in_shared_mins=log(passes_in_shared_mins),
    ln_passes_in_shared_mins1=log(passes_in_shared_mins1),
    ln_passes_in_shared_mins2=log(passes_in_shared_mins2),
    pass_per_min2=pass_count/minutes_shared,
    ln_pass_per_min=ln_pass_count-ln_minutes_shared,
    points_per_game=points/season_length,
    ln_points=log(points)
  ) 


# keep variables only that we use later
teams_season_half_aggregated %<>% select(league_season, season_name_half, anon_teamid, ln_pass_count_per_game,
                                         points_per_game,  ln_value_p1, season_half)

# save the dataset
write_parquet(teams_season_half_aggregated, paste0(data_work, "anon_teams_aggregated.parquet"))

