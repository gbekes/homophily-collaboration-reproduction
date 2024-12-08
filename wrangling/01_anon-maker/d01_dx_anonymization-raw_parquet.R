
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#
# d01_dx_anonymization-raw_parquet.R

# creates anonymized versions of the raw data

# This version: v2.1: 2024-11-18
#######################################################


source(paste0("wrangling/01_anon-maker/","league-filter.R"))             # data_dir must be first defined 



## translator tables
teamid_anon = read_parquet(paste0(anon_tables, "teamid_anon.parquet"))

wh_player_id_anon = read_parquet(paste0(anon_tables, "wh_player_id_anon_ext.parquet"))
wh_player_id_anon %<>% filter(!is.na(wh_player_id)) # 1 obs 

tm_player_id_anon = read_parquet(paste0(anon_tables, "tm_player_id_anon.parquet"))

# drop flag
wh_player_id_anon %<>% select(-flag) 


#######################x
# matches
#######################x
matches <- read_parquet(file = matches.parquet,as_data_frame = TRUE) 

# anonymize: home_teamid, away_teamid 
matches = matches %>%
  left_join(teamid_anon,
            by = c("home_teamid" = "teamid")
  ) %>% 
  select(anon_teamid, everything()) %>% 
  rename(anon_home_teamid = anon_teamid) %>% 
  left_join(teamid_anon,
            by = c("away_teamid" = "teamid")
  ) %>% 
  select(anon_teamid, everything()) %>% 
  rename(anon_away_teamid = anon_teamid)

# replace missing: when anon_team is missing replace it with teamid both for home and away
matches %<>%
  mutate(anon_home_teamid = ifelse(is.na(anon_home_teamid), home_teamid, anon_home_teamid),
         anon_away_teamid = ifelse(is.na(anon_away_teamid), away_teamid, anon_away_teamid))

# drop what we don't need
matches = matches %>% 
  select(-referee_officialid, -home_managername, -away_managername, -home_teamid, -away_teamid)

# save
write_parquet(matches, paste0(data_tidy, "anon_matches.parquet"))

#######################x
# players
#######################x

players<- read_parquet(file = players_coref.parquet,as_data_frame = TRUE)

# change tm_player_id to numeric from char
players$tm_player_id = as.numeric(players$tm_player_id)
tm_player_id_anon$tm_player_id = as.numeric(tm_player_id_anon$tm_player_id)

# anonymize: wh_player_id, tm_player_id
players = players %>%
  left_join(wh_player_id_anon,
            by = c("wh_player_id" = "wh_player_id")
  ) %>% 
  select(anon_player_id, everything()) %>% 
  left_join(tm_player_id_anon,
            by = c("tm_player_id" = "tm_player_id")
  ) %>% 
  select(anon_other_player_id, everything())

# Hmisc::describe(players$anon_player_id)

# replace missing: when wh_player_id_anon is missing replace it with wh_player_id
players %<>%
  mutate(anon_other_player_id = ifelse(is.na(anon_other_player_id), tm_player_id, anon_other_player_id),
         anon_player_id       = ifelse(is.na(anon_player_id)      , wh_player_id, anon_player_id))

# drop what we don't need
players = players %>% 
  select(-wh_player_id, -tm_player_id)

# save
write_parquet(players, paste0(data_tidy,"anon_players.parquet"))

#######################x
# players_info
#######################x

player_info <- read_parquet(file = player_info.parquet,as_data_frame = TRUE)
# change tm_player_id to numeric from char
player_info$tm_player_id = as.numeric(player_info$tm_player_id)


# anonymize tm_player_id
player_info = player_info %>%
  left_join(tm_player_id_anon,
            by = c("tm_player_id" = "tm_player_id")
  ) %>% 
  select(anon_other_player_id, everything())


player_info %<>%
  mutate(anon_other_player_id = ifelse(is.na(anon_other_player_id), tm_player_id, anon_other_player_id))


# drop what we don't need
player_info = player_info %>% 
  select(-full_name, -name, -tm_player_id, -preferred_foot)


# save
write_parquet(player_info, paste0(data_tidy,"anon_player_info.parquet"))


#######################x
# market values
#######################x

market_values<- read_parquet(file = player_values.parquet,as_data_frame = TRUE)
# change tm_player_id to numeric from char
market_values$tm_player_id = as.numeric(market_values$tm_player_id)

# anonymize tm_player_id
market_values = market_values %>%
  left_join(tm_player_id_anon,
            by = c("tm_player_id" = "tm_player_id")
  ) %>% 
  select(anon_other_player_id, everything())

market_values %<>%
  mutate(anon_other_player_id = ifelse(is.na(anon_other_player_id), tm_player_id, anon_other_player_id))

#drop what we don't need
market_values = market_values %>% 
  select(-tm_player_id)



# save
write_parquet(market_values, paste0(data_tidy,"anon_player_values.parquet"))


################
# transfers
################
transfers <- read_parquet(file = player_transfers.parquet,as_data_frame = TRUE)

# change tm_player_id, left, joined -->to numeric from char  
transfers$tm_player_id = as.numeric(transfers$tm_player_id)
transfers$left = as.numeric(transfers$left)
transfers$joined = as.numeric(transfers$joined)

# anonymize joined and left with teamid_anon
transfers = transfers %>%
  left_join(teamid_anon,
            by = c("left" = "teamid")
  ) %>% 
  select(anon_teamid, everything()) %>% 
  rename(anon_left = anon_teamid) %>% 
  left_join(teamid_anon,
            by = c("joined" = "teamid")
  ) %>% 
  select(anon_teamid, everything()) %>% 
  rename(anon_joined = anon_teamid)

# anonymize tm_player_id
transfers = transfers %>%
  left_join(tm_player_id_anon,
            by = c("tm_player_id" = "tm_player_id")
  ) %>% 
  select(anon_other_player_id, everything())


transfers %<>%
  mutate(anon_other_player_id = ifelse(is.na(anon_other_player_id), tm_player_id, anon_other_player_id),
         anon_joined = ifelse(is.na(anon_joined), joined, anon_joined),
         anon_left = ifelse(is.na(anon_left), left, anon_left))


# drop what we don't need
transfers = transfers %>% 
  select(-tm_player_id, -joined, -left)

# save
write_parquet(transfers, paste0(data_tidy,"anon_player_transfers.parquet"))


################
# team relations
################

team_relations <- read_parquet(file = team_relations.parquet,as_data_frame = TRUE)
# change teamid to numeric from char
team_relations$parent_team_id = as.numeric(team_relations$parent_team_id)
team_relations$child_team_id = as.numeric(team_relations$child_team_id)


# anonymize parent_teamid, child_teamid
team_relations = team_relations %>%
  left_join(teamid_anon,
            by = c("parent_team_id" = "teamid")
  ) %>% 
  select(anon_teamid, everything()) %>% 
  rename(anon_parent_team_id = anon_teamid) %>% 
  left_join(teamid_anon,
            by = c("child_team_id" = "teamid")
  ) %>% 
  select(anon_teamid, everything()) %>% 
  rename(anon_child_team_id = anon_teamid)

team_relations %<>%
  mutate(anon_parent_team_id = ifelse(is.na(anon_parent_team_id), parent_team_id, anon_parent_team_id),
         anon_child_team_id  = ifelse(is.na(anon_child_team_id) , child_team_id, anon_child_team_id))

# drop what we don't need
team_relations %<>%
  select(-parent_team_id, -child_team_id)

# save
write_parquet(team_relations, paste0(data_tidy,"anon_team_relations.parquet"))


################
# team info
################

team_info <- read_parquet(file = team_info.parquet,as_data_frame = TRUE)
# change teamid to numeric from char
team_info$team_id = as.numeric(team_info$team_id)

# anonymize
team_info = team_info %>%
  left_join(teamid_anon,
            by = c("team_id" = "teamid")
  ) %>% 
  select(anon_teamid, everything()) 

team_info %<>%
  mutate(anon_teamid = ifelse(is.na(anon_teamid), team_id, anon_teamid))

# drop what we don't need
team_info = team_info %>% 
  select(-name, -address, -team_id)

# save
write_parquet(team_info, paste0(data_tidy,"anon_team_info.parquet"))



################
# formation_uses
################

formation_use = read_parquet(formation_use.parquet)

# anonymize spot values. Each spot value (spot_1, spot_2, ... spot_11) is a player, encoded as wh_player_id
# anonymize wh_player_id


# Function to replace IDs for a given spot
anonymize_spot <- function(data, spot, mapping) {
  spot_anon <- paste0(spot, "_anon")
  data %>%
    rename(wh_player_id = !!spot) %>%
    left_join(mapping, by = "wh_player_id") %>%
    rename(!!spot := anon_player_id) %>%
    select(-wh_player_id)
}

# Apply the anonymization function to each spot
spots <- paste0("spot_", 1:11)
for(spot in spots) {
  formation_use <- anonymize_spot(formation_use, spot, wh_player_id_anon)
}

# save
write_parquet(formation_use, paste0(data_tidy,"anon_formation_use.parquet"))


################
# lineups
################
lineups = read_parquet(lineups.parquet)

# anonymize wh_player_id
lineups = lineups %>%
  left_join(wh_player_id_anon,
            by = c("wh_player_id" = "wh_player_id")
  ) %>% 
  select(anon_player_id, everything())

# anonymize teamid
lineups = lineups %>%
  left_join(teamid_anon,
            by = c("wh_team_id" = "teamid")
  ) %>% 
  select(anon_teamid, everything())

# anonymize tm_player_id

# change tm_player_id to numeric from char
lineups$tm_player_id = as.numeric(lineups$tm_player_id)

lineups = lineups %>%
  left_join(tm_player_id_anon,
            by = c("tm_player_id" = "tm_player_id")
  ) %>% 
  select(anon_other_player_id, everything())

skim(lineups)

# fillin missing
lineups %<>%
  mutate(anon_other_player_id = ifelse(is.na(anon_other_player_id), tm_player_id, anon_other_player_id),
         anon_player_id = ifelse(is.na(anon_player_id), wh_player_id, anon_player_id))

# drop what we don't need
lineups = lineups %>% 
  select(-wh_player_id, -tm_player_id, -wh_team_id)

# save
write_parquet(lineups, paste0(data_tidy,"anon_lineups.parquet"))


################
# all_player_minutes

# NB this is generated i think somewhere 
################


all_player_minutes = read_parquet(paste0(data_tidy_temp,"all_player_minutes.parquet"))

# anonymize wh_player_id
all_player_minutes = all_player_minutes %>%
  left_join(wh_player_id_anon,
            by = c("wh_player_id" = "wh_player_id")
  ) %>% 
  select(anon_player_id, everything())

# anonymize teamid
all_player_minutes = all_player_minutes %>%
  left_join(teamid_anon,
            by = c("teamid" = "teamid")
  ) %>% 
  select(anon_teamid, everything()) 

# Hmisc::describe(all_player_minutes$anon_player_id)
# Hmisc::describe(all_player_minutes$wh_player_id)

all_player_minutes %<>%
  mutate(anon_player_id = ifelse(is.na(anon_player_id), wh_player_id, anon_player_id))

# drop what we don't need
all_player_minutes = all_player_minutes %>% 
  select(-wh_player_id, -teamid)


# save
write_parquet(all_player_minutes, paste0(data_tidy,"anon_all_player_minutes.parquet"))



################
# player_passcounts_shared_minutes_major_leagues
################

skeleton = read_parquet(paste0(data_tidy_temp, "player_passcounts_shared_minutes_major_leagues.parquet"))

# anonymize wh_player_id1 and wh_player_id2 as anon_player_id1 and anon_player_id2
skeleton = skeleton %>%
  left_join(wh_player_id_anon,
            by = c("wh_player_id1" = "wh_player_id")
  ) %>% 
  select(anon_player_id, everything()) %>% 
  rename(anon_player_id1 = anon_player_id) %>% 
  left_join(wh_player_id_anon,
            by = c("wh_player_id2" = "wh_player_id")
  ) %>% 
  select(anon_player_id, everything()) %>% 
  rename(anon_player_id2 = anon_player_id)

# Hmisc::describe(skeleton$wh_player_id1)
# Hmisc::describe(skeleton$anon_player_id1)

# drop 
skeleton = skeleton %>% 
  select(-wh_player_id1, -wh_player_id2)

# save
write_parquet(skeleton, paste0(data_tidy,"anon_player_passcounts_shared_minutes_major_leagues.parquet"))



###############
# all_events_agg_major_leagues
###############


all_events_match = read_parquet(paste0(data_tidy_temp,"all_events_agg_major_leagues.parquet"))

# anonymize wh_player_id1 and wh_player_id2 as anon_player_id1 and anon_player_id2
all_events_match = all_events_match %>%
  left_join(wh_player_id_anon,
            by = c("wh_player_id1" = "wh_player_id")
  ) %>% 
  select(anon_player_id, everything()) %>% 
  rename(anon_player_id1 = anon_player_id) %>% 
  left_join(wh_player_id_anon,
            by = c("wh_player_id2" = "wh_player_id")
  ) %>% 
  select(anon_player_id, everything()) %>% 
  rename(anon_player_id2 = anon_player_id)

# Hmisc::describe(all_events_match$wh_player_id1)
# Hmisc::describe(all_events_match$anon_player_id1)

all_events_match %<>%
  mutate(anon_player_id1 = ifelse(is.na(anon_player_id1), wh_player_id1, anon_player_id1),
         anon_player_id2 = ifelse(is.na(anon_player_id2), wh_player_id2, anon_player_id2))


# anonymize teamid
all_events_match = all_events_match %>%
  left_join(teamid_anon,
            by = c("teamid" = "teamid")
  ) %>% 
  select(anon_teamid, everything()) 


# drop what we don't need
all_events_match = all_events_match %>% 
  select(-wh_player_id1, -wh_player_id2, -teamid)

# save
write_parquet(all_events_match, paste0(data_tidy,"anon_all_events_agg_major_leagues.parquet"))

###########################
## culture groups
###########################


###############################################
## ANONYMIZE CULTURE GROUPS
###############################################


all_culture_season_half = read_parquet(paste0(data_tidy_temp, "all_culture_season_half.parquet"))

all_culture_season_half = all_culture_season_half %>% 
  left_join(wh_player_id_anon,
            by = c("wh_player_id1" = "wh_player_id")
  ) %>% 
  rename(anon_player_id1 = anon_player_id) %>% 
  left_join(wh_player_id_anon,
            by = c("wh_player_id2" = "wh_player_id")
  ) %>%
  rename(anon_player_id2 = anon_player_id
  ) %>% 
  left_join(teamid_anon,
            by = c("teamid" = "teamid")
  ) %>% 
  select(-wh_player_id1,
         -wh_player_id2,
         -teamid
  ) %>% 
  select(anon_teamid, anon_player_id1, anon_player_id2, everything())

write_parquet(all_culture_season_half, paste0(data_tidy, "anon_all_culture_season_half.parquet"))
