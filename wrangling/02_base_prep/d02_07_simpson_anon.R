
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d02_07_simpson_anon.R

# creates the simpson index per match-team (not used in paper eventually)

# This version: v2.1: 2024-11-18
#######################################################


# load files

match_participation = read_parquet(paste0(data_tidy,"anon_lineups.parquet"))

players = read_parquet(paste0(data_tidy,"anon_players.parquet"))

matches = read_parquet(paste0(data_tidy,"anon_matches.parquet"))
 

# chars
match_participation %<>% mutate(anon_player_id = as.character(anon_player_id)) # as character
                                
# only consider starting XI
starters = match_participation %>% 
  filter(position != "Sub")


# There are some instances where there were not 11 players starting for some reason.

# mistakes: 
bad_matches = starters %>% 
  group_by(wh_match_id, side) %>% 
  filter(n_distinct(anon_player_id) != 11) %>% 
  distinct(wh_match_id)
aux = match_participation %>% 
  inner_join(., bad_matches, by = c("wh_match_id" = "wh_match_id", 
                                    "side" = "side")
  ) %>% 
  arrange(wh_match_id, side, shirt_no)

# I calculate the Simpson index manually according to the following definition
# $$D^S_{m,t} = 1 - \sum_{i = 1}^R \frac{n_i (n_i - 1)}{N (N - 1)}$$
#  where $i$ denotes citizenship of $R$ nations, $n_i$ denotes the number of players with a citizenship.

simpson_index = starters %>%
  rename(field = side) %>% 
  # join citizenship
  left_join(., players %>% 
              mutate(anon_player_id = as.character(anon_player_id)) %>% 
              group_by(anon_player_id) %>% 
              summarise(citizenship_0 = max(`citizenship-0`)),
            by = c("anon_player_id")) %>% 
  group_by(wh_match_id, field, citizenship_0) %>%
  # count nationalities
  summarise(n_i = n_distinct(anon_player_id)) %>% # count by nationalities the number of same nationality passes
  ungroup() %>% 
  group_by(wh_match_id, field) %>% 
  # create index
  summarise(num = sum(n_i * (n_i - 1)),
            denom = sum(n_i) * (sum(n_i) - 1),
            match_team_countries = n()
  ) %>% 
  mutate(div_simpson_starting =  num / denom) %>% 
  select(-num,-denom)

#Use the matches dataset to join home and away team_id's

matches %<>% 
  select(wh_match_id, anon_home_teamid, anon_away_teamid)

simpson_index = simpson_index %>% 
  left_join(., matches,
            by = c("wh_match_id" = "wh_match_id")
  ) %>% 
  mutate(anon_teamid = case_when(field == "home" ~ anon_home_teamid,
                            TRUE ~ anon_away_teamid)
  ) %>% 
  select(wh_match_id, anon_teamid, match_team_countries, div_simpson_starting, field)

Hmisc::describe(simpson_index$div_simpson_starting)


# save the data
write_csv(simpson_index, 
          paste0(data_tidy_created,"anon_simpson_index.csv"))

#drop the data
rm(match_participation)
rm(players, matches, starters, bad_matches, aux, simpson_index)
