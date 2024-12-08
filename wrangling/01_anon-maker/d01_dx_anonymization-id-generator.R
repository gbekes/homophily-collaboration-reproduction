
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#
# d01_dx_anonymization-id-generator.R

# creates anonymized player and team IDs

# This version: v2.1: 2024-11-18
#######################################################


####################
## ANONYMIZATION
####################

all_events_season = read_parquet(paste0(data_tidy_temp, "all_events_agg_major_leagues.parquet"))
#skim(all_events_season)
player_native_languages = read_parquet(paste0(data_tidy_temp, "players.parquet"))

player_languages_tm_list = read_parquet(paste0(data_tidy_temp, "players.parquet")) %>% 
  pull(tm_player_id) %>% unique()

player_languages_wh_list = read_parquet(paste0(data_tidy_temp, "players.parquet")) %>% 
  pull(wh_player_id) %>% unique()


## create the player ID-s randomly

wh_player_id_list1 = all_events_season %>% pull(wh_player_id1) %>% unique()
wh_player_id_list2 = all_events_season %>% pull(wh_player_id2) %>% unique()
wh_player_id_list3 = player_native_languages %>% pull(wh_player_id) %>% unique()
tm_player_id_list1 = player_native_languages %>% pull(tm_player_id) %>% unique()

wh_player_id_list = unique(c(wh_player_id_list1, wh_player_id_list2, wh_player_id_list3, player_languages_wh_list))
teamid_list1 = all_events_season %>% pull(teamid) %>% unique()

teamid_list2 = read_csv(paste0(data_tidy, "teams_season_half.csv")) %>% pull(teamid) %>% unique()

tm_player_id_list = unique(c(player_languages_tm_list, tm_player_id_list1))
teamid_list = unique(c(teamid_list1, teamid_list2))
# create random order of the ID-list, and assign the index as value
set.seed(123)
wh_player_id = sample(wh_player_id_list %>% as.data.frame(.) %>% arrange(.))
wh_player_id_anon = as.data.frame(wh_player_id)
wh_player_id_anon = wh_player_id_anon %>% 
  mutate(anon_player_id = row_number() + 100000000)

wh_player_id_anon %<>%    rename(wh_player_id_anon, wh_player_id=`.`)

set.seed(234)
teamid = sample(teamid_list %>% as.data.frame(.) %>% arrange(.))
teamid_anon = as.data.frame(teamid)
teamid_anon = teamid_anon %>% 
  mutate(anon_teamid = row_number() + 100000000)

teamid_anon %<>%    rename(teamid_anon, teamid=`.`)

set.seed(345)
tm_player_id = sample(tm_player_id_list %>% as.data.frame(.) %>% arrange(.))
tm_player_id_anon = as.data.frame(tm_player_id)
tm_player_id_anon = tm_player_id_anon %>% 
  mutate(anon_other_player_id = row_number() + 1000000000) 

tm_player_id_anon %<>%    rename(tm_player_id_anon, tm_player_id=`.`)

## translator tables
write_parquet(wh_player_id_anon, paste0(anon_tables, "wh_player_id_anon.parquet"))
write_parquet(tm_player_id_anon, paste0(anon_tables, "tm_player_id_anon.parquet"))
write_parquet(teamid_anon, paste0(anon_tables, "teamid_anon.parquet"))

# corrections
formation_use = read_parquet(formation_use.parquet)

# Gather all spot columns into a single column and then get unique values
unique_ids <- formation_use %>%
  select(starts_with("spot_")) %>%  # Select only columns that start with "spot_"
  pivot_longer(cols = everything()) %>%  # Transform to long format
  distinct(value) %>%  # Get distinct/unique player IDs
  pull(value)  # Extract the IDs as a vector


# Step 1: Create a data frame with unique player IDs and temp=1
unique_ids_df <- formation_use %>%
  select(starts_with("spot_")) %>%
  pivot_longer(cols = everything()) %>%
  distinct(value) %>%
  rename(wh_player_id = value) %>%
  mutate(temp = 1)

# Step 2: Join with wh_player_id_anom
joined_df <- unique_ids_df %>%
  left_join(wh_player_id_anon, by = "wh_player_id")

# add flag when anon missing
joined_df <- joined_df %>%
  mutate(flag = if_else(is.na(anon_player_id), 1, 0))

# Step 3: Replace missing anon_player_id with wh_player_id where temp=1
final_df <- joined_df %>%
  mutate(anon_player_id = if_else(is.na(anon_player_id) & temp == 1, wh_player_id, anon_player_id)) %>%
  select(wh_player_id, anon_player_id, flag) 
skim(final_df)

write_parquet(final_df, paste0(anon_tables, "wh_player_id_anon_ext.parquet"))

# drop data
rm(all_events_season, player_native_languages, player_languages_tm_list, player_languages_wh_list, 
   wh_player_id_list1, wh_player_id_list2, wh_player_id_list3, tm_player_id_list1, 
   wh_player_id_list, teamid_list1, teamid_list2, tm_player_id_list, teamid_list, 
   wh_player_id, wh_player_id_anon, teamid, teamid_anon, tm_player_id, 
   tm_player_id_anon, formation_use, unique_ids, unique_ids_df, joined_df, final_df)
