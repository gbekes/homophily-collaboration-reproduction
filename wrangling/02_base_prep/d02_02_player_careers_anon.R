
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#
# d02_02_player_careers_anon.R

# creates anonymized player and team IDs

# (complicated stuff, so more comments than usual)
# In this script we create a comprehensive table of player transfer history on a semiannual basis, with an observation characterized by a player ID, a season, and a season half. Season half focal points are 15th October and 15th February. 
# 1. Based on transfer histories, we create a frame dataset for each player, a career starting at the date of the first transfer (most often transfer from a youth team to another youth team, or a professional team). The career ends at retirement, or we choose an endpoint at 31-12-2021. 
# 2. We distinguish between real transfers (acquisitions of player rights) and loans, and we also mark transfers which are across different levels of the same franchise (A-level, B-level, U19 etc.).
# 3. We calculate spells with teams 3 ways: current spell with given team, all spells with given team, all spells with given franchise.
# 4. We calculate the number of clubs in the same country, and the number of previous clubs in other countries.

# This version: v2.1: 2024-11-18
#######################################################


################
# Load datasets 
################


transfers <- read_parquet(paste0(data_tidy,"anon_player_transfers.parquet"))

players = read_parquet(paste0(data_tidy,"anon_players.parquet")) 

matches <- read_parquet(paste0(data_tidy,"anon_matches.parquet"))
  
market_values <- read_parquet(paste0(data_tidy,"anon_player_values.parquet"))

team_relations <- read_parquet(paste0(data_tidy,"anon_team_relations.parquet"))

team_info <- read_parquet(paste0(data_tidy,"anon_team_info.parquet"))

#####################################
# setting up the global frame
#####################################


# A global frame dataset for the periods
# start year for the global frame: 1960
# end year for the global frame: 2021
# 10-15 and 02-15 are the dates we join

years_list = seq(1899, 2021, by = 1) %>% as.data.frame() %>% mutate(joinon = 1)
names(years_list) = c("years", "joinon")
halfyear_list = c("02-15", "10-15") %>% as.data.frame() %>% mutate(joinon = 1)
names(halfyear_list) = c("halfyeardates", "joinon")
halfyear_list = halfyear_list %>% 
  mutate(season_half = if_else(halfyeardates == "02-15", "2", "1"))

players_global_begin_end = years_list %>% 
  left_join(., halfyear_list,
            by = "joinon")

players_global_begin_end = players_global_begin_end %>% 
  mutate(season = as.character(case_when(season_half == "1" ~ years,
                            TRUE ~ years - 1)),
         day_id = as.Date(paste0(years, "-", halfyeardates))
         ) %>% 
  filter(season >= 1899,
         season <= 2021) %>% 
  select(-halfyeardates)

#### Start from the transfers table. Start of a career is first transfer.
# End of a career is joining "club" 123, indicating retirement
# For every other player without retirement, end date is going to be 2021.12.31.


players_begin_end = transfers %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(anon_other_player_id) %>%
  filter((date == min(date)) |
           (date == max(date) & anon_joined == "123")
           ) %>% 
  summarise(mindate = min(date),
            maxdate = max(date)
            )

players_begin_end = players_begin_end %>% 
  mutate(maxdate = if_else(maxdate == mindate, 
                           as.Date("2021-12-31"), maxdate))

players_global = transfers %>%
  group_by(anon_other_player_id) %>%
  summarise(joinon = 1)

frame = players_global %>% 
  left_join(., players_global_begin_end, "joinon") %>% 
  select(-joinon) %>% 
  left_join(., players_begin_end, "anon_other_player_id") %>% 
  filter(day_id >= mindate,
         day_id <= maxdate)

#####################################
# player/season-half data 
#####################################


# From value data, and create the player/season-half level dataset.

market_values <- market_values %>%
  mutate(year = year(date),
         month = month(date)
  )
market_values <- market_values %>%
  clean_names() 
table(market_values$year)
market_values = market_values %>%
  filter(year <= 2021) # there are some future values

table(market_values$month)

# Just corrections
market_values = market_values %>%
  mutate(day = day(date))
# here we also use the theoretical season-half starts   
market_values = market_values %>%
  mutate(season_half = case_when((month * 100 + day < 215) ~ 1,
                                 (month * 100 + day >= 1015) ~ 1,
                                 TRUE ~ 2
                                 ),
         season = case_when(season_half == 2 ~ year - 1,
                            (season_half == 1) & ((month * 100 + day) < 215) ~ year - 1,
                            TRUE ~ year
                            )
         )

# create season value 
market_values_halfseason = market_values %>%
  ungroup() %>% 
  mutate(season = as.character(season),
         season_half = as.character(season_half) # otherwise duplicates
  ) %>% 
  group_by(season, season_half, anon_other_player_id) %>%
  summarise(count= n(),
            value = mean(value),
            year = max(year)) %>% 
  
  mutate(day_id = as.Date(paste0(as.character(year), # year, not season, as season indicates the starting year only!!
                                 case_when(season_half == 1 ~ "-10-15",
                                           TRUE ~ "-02-15")
  )
  )
  ) %>%
  arrange(anon_other_player_id, season, season_half, day_id) %>% 
  select(-year) %>% 
  select(anon_other_player_id, season, season_half, day_id, everything())


# join the market values as well when available (For future use)
market_values_halfseason = frame %>%
  select(-mindate, -maxdate) %>% 
  left_join(.,
            market_values_halfseason,
            by = c("anon_other_player_id" = "anon_other_player_id",
                   "day_id" = "day_id",
                   "season" = "season",
                   "season_half" = "season_half"
                   )
            )

##############################################
# create vars re transfers 
##############################################


# Starting with the transfers data, create the necessary variables for construction (dates, counts etc.) 
# To be able to join the datasets, use end of 2021!!

transfers = transfers %>%
  mutate(date = as.Date(date)) %>% 
  group_by(anon_other_player_id) %>% 
  mutate(next_date = dplyr::lead(date, 1)) %>% 
  rename(anon_other_player_id_tr = anon_other_player_id) %>% 
  mutate(next_date = if_else(is.na(next_date), as.Date("2021-12-31"), next_date)
         )


## TEAM RELATIONS

child_groups = team_relations %>% 
  group_by(anon_child_team_id) %>% 
  summarise(group_id = min(anon_parent_team_id))

youth_teams = team_relations %>% 
  left_join(.,
            child_groups,
            by = "anon_child_team_id"
            )

transfers = transfers %>% 
  left_join(., team_info,
            by = c("anon_left" = "anon_teamid")) %>% 
  rename(country_left = country
  ) %>%
  left_join(., team_info,
            by = c("anon_joined" = "anon_teamid")) %>% 
  rename(country_joined = country
         )



### Join 
transfers = transfers %>% 
  left_join(., youth_teams,
            by = c("anon_left" = "anon_child_team_id",
                   "anon_joined" = "anon_parent_team_id"
                   )
            ) 

  transfers = transfers %>% 
    mutate(oneway = 1 * (!is.na(child_team_name))) %>% 
  select(-child_team_name) %>% 
  left_join(., youth_teams,
            by = c("anon_joined" = "anon_child_team_id",
                   "anon_left" = "anon_parent_team_id"
            )
  ) %>% 
  mutate(otherway = 1 * (!is.na(child_team_name))) %>% 
  select(-child_team_name,
         -contains("index_level")) %>% 
  mutate(intraclubgroup = 1 * ((oneway + otherway) > 0)) %>% # either is > 0, then it is > 0
  select(-oneway, -otherway) %>% 
  # specific not real trades: retirement, ban, pause
  mutate(not_real_transfers = 1*(anon_joined %in% c("75", "2113", "123", "515", "2077"))
         ) %>% 
  rename(group_id_left = group_id.x,
         group_id_joined = group_id.y) %>% 
  left_join(., child_groups %>% rename(group_id_aux = group_id),
            by = c("anon_left" = "anon_child_team_id")) %>% 
  mutate(group_id_left = if_else(is.na(group_id_left), group_id_aux, group_id_left)
         ) %>% 
  select(-group_id_aux) %>% 
  left_join(., child_groups %>% rename(group_id_aux = group_id),
            by = c("anon_joined" = "anon_child_team_id")) %>% 
  mutate(group_id_joined = if_else(is.na(group_id_joined), group_id_aux, group_id_joined)
         ) %>% 
  select(-group_id_aux)


# as transfers are between teams, row_numbers show the number of clubs he is in 
transfers = transfers %>% 
  ungroup() %>%
  arrange(anon_other_player_id_tr, date) %>%
  group_by(anon_other_player_id_tr) %>% 
  mutate(min_date = min(date)) %>% 
  ungroup() %>% 
  group_by(anon_other_player_id_tr, country_joined) %>% 
  mutate(country_mindate = min(date),
         n_clubs_same_country = as.numeric(cumsum((!duplicated(anon_joined)) &
                                                    ((intraclubgroup == 0)) &
                                                    (not_real_transfers == 0)
                                                  
                                                  )
                                           
                                           )
         ) %>% 
  ungroup()

# correct for those, when play in youth in the same country, so all 
transfers = transfers %>% 
  group_by(anon_other_player_id_tr, country_joined) %>%
  mutate(n_clubs_same_country = case_when((min(n_clubs_same_country) == 0) & (!is.na(country_joined)) ~ n_clubs_same_country + 1,
                                          TRUE ~ n_clubs_same_country
                                          )
         )
  


transfers = transfers %>% 
  ungroup() %>%
  group_by(anon_other_player_id_tr) %>% 
  mutate(n_clubs_total =  as.numeric(cumsum((!duplicated(anon_joined)) &
                                              (intraclubgroup == 0) &
                                              (not_real_transfers == 0)
                                            ) + 1)) %>% #+1 as there is always an initial club
  ungroup() %>% 
  mutate(n_clubs_other_country = n_clubs_total - n_clubs_same_country)
  

# calculate spells -----------------------------

# Calculate the team-level minimum for long time spell variables

transfers = transfers %>% 
  arrange(anon_other_player_id_tr, date) %>% 
  group_by(anon_other_player_id_tr, anon_joined) %>% 
  mutate(min_join_date = min(date)) %>%  # first join of the team
  ungroup() %>% 
  group_by(anon_other_player_id_tr, group_id_joined) %>% 
  mutate(min_join_date_group = min(date)) %>%  # first join of the "franchise"
  ungroup()


##################################################
# join the current teams to the season-halves.
##################################################


# Use SQL and between join to join the current teams to the season-halves.

# Goal is to identify for each player the team that they play for at the start 
# of a half-season, which means that for players that had at least one transfer,
# the focus date (day_id) of the season must be after the date that he left the previous team (date -> join_date),
# and before the date he joined the next team (next_date, which is the lead value)
# within the same player_id.

# The variables needed (number of clubs, countries played) had to be produced
# using the entire tranfers dataset earlier as the time frame itself
# starts before the market value dataset does.


sql_statement = "SELECT 
anon_other_player_id  as anon_other_player_id
, season
, season_half
, day_id      as day_id


, anon_joined      as teamid
, country_joined  as country_id

, date        as join_date
, next_date
, min_join_date
, group_id_joined
, min_join_date_group

, is_loan     as is_loan
, is_end_of_loan  as is_end_of_loan
, intraclubgroup
, not_real_transfers

, n_clubs_same_country
, n_clubs_other_country
, value

FROM market_values_halfseason
LEFT JOIN transfers
  ON anon_other_player_id = anon_other_player_id_tr
  AND day_id BETWEEN date and next_date"
MV_transf_joint = sqldf::sqldf(sql_statement)

# Turn it into Date format as it changes
MV_transf_joint = MV_transf_joint %>% 
  mutate(join_date = zoo::as.Date(join_date)) %>% 
  arrange(anon_other_player_id, day_id)

# NB with the new method there cannot be missings


#####################################
# add player ID 
#####################################

# Join players ID.

players%<>% 
  distinct(anon_other_player_id, anon_player_id)


MV_transf_joint %<>% 
  left_join(., players ,
            by = c("anon_other_player_id" = "anon_other_player_id"))



#####################################
# Calculate the time distances.
#####################################

# Note to self: this is slow...
MV_transf_joint = MV_transf_joint %>%
  group_by(anon_other_player_id) %>% 
  mutate(min_career_date = min(join_date),
         prev_day_id = case_when(is.na(dplyr::lag(teamid)) ~ min_career_date,
                                 teamid == dplyr::lag(teamid) ~ dplyr::lag(day_id),
                                 teamid != dplyr::lag(teamid) ~ join_date
                                 ),
         period_length = difftime(day_id, 
                                  prev_day_id, units = "days")
         ) %>% 
  ungroup() %>% 
  mutate(w_club_days = as.numeric(difftime(day_id, join_date, units = "days")),
         career_length = as.numeric(difftime(day_id, min_career_date, units = "days"))
         ) %>%
  ungroup() %>% 
  group_by(anon_other_player_id, teamid) %>% 
  mutate(w_club_days_long = cumsum(as.numeric(period_length))
           ) %>% 
  ungroup() %>% 
  group_by(anon_other_player_id, group_id_joined) %>% 
  mutate(w_club_days_group = cumsum(as.numeric(period_length))
         ) %>% 
  ungroup()

# it is already the anon version trough anon_join
MV_transf_joint %<>%
  rename(anon_teamid = teamid) 

###############################
# Select the needed variables.
###############################

MV_transf_joint %<>% 
  group_by(anon_other_player_id, day_id) %>% 
  arrange(anon_other_player_id, day_id, desc(w_club_days)) %>% 
  filter(row_number() == 1)

MV_transf_joint %<>% ungroup() 

  
#####################################
# Save the dataset 
#####################################

write_parquet(MV_transf_joint, 
              paste0(data_tidy_created,"anon_MV_transf_joint.parquet"))

# remove all datasets
for(obj_name in ls()) {
  # Check if the object is a data frame
  if(is.data.frame(get(obj_name))) {
    # Remove the data frame
    rm(list = obj_name)
  }
}



