
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d02_01_player-maker-anon.R

# creates the player dataset
# uses public data

# This version: v2.1: 2024-11-18
#######################################################


###################################################
# load
###################################################



players<- read_parquet(paste0(data_tidy, "anon_players.parquet")            )

player_info <- read_parquet(paste0(data_tidy, "anon_player_info.parquet")      )

market_values<- read_parquet(paste0(data_tidy, "anon_player_values.parquet"))

###################################################
# players
###################################################


players = players %>% 
  left_join(.,
            player_info,
            by = c("anon_other_player_id" = "anon_other_player_id")
            )
# garbage collection
gc()

write_parquet(players, paste0(data_tidy,"anon_players.parquet"))

###################################################
# market value
###################################################


market_values <- market_values %>%
  mutate(year=year(date),
         month=month(date))

market_values <- market_values %>%
  clean_names() 

# table(market_values$year)
market_values <- market_values %>%
  filter(year>=2006)

# table(market_values$month)
market_values <- market_values %>%
  mutate(season =if_else(month>4, year, year-1, missing=year)) 
#   mutate(season_half=if_else(month>=11 | month<=4, 2,1, missing=NULL))  %>%



# create season_half: NOW 1 January
market_values <- market_values %>%
  mutate(dt= as_date(date),
         season_cutoff=make_date(year=season+1, month=1, day=1),
         season_diff = lubridate::as.duration(season_cutoff %--% dt) / ddays(1),
         season_half=ifelse(season_diff>=0, 2,1)
  )



# create season value ---could be useful later
market_values_season <- market_values %>%
  mutate(c=1)  %>%
  group_by(season, season_half, anon_other_player_id) %>%
  summarise(count=sum(c),
            value=mean(value)) %>%
  arrange(anon_other_player_id, season, season_half)

# create annual value, as first period value
market_values_season <- market_values_season %>%
  mutate(c=1)  %>%
  group_by(season, anon_other_player_id)  %>%
  summarise(count=sum(c),
            value = value[which.min(season_half)]) %>%
  arrange(anon_other_player_id, season)%>%
  select(-count) 

write_parquet(market_values_season, paste0(data_tidy_created,"anon_market_values_season.parquet"))

# drop data
rm(market_values, market_values_season)
rm(player_info, players)