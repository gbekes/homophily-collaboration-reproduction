
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#
# d01_01_match_passes.R

# Creates team and match level datasets 
# Raw data used. 

# This version: v2.1: 2024-11-18
#######################################################




###############################
# LOAD DATA FROM PARQUET FILESS
###############################

matches = read_parquet(file = matches.parquet,
                  as_data_frame = TRUE)

seasons = read_parquet(file = seasons.parquet,
                       as_data_frame = TRUE)

formation_uses = read_parquet(file = formation_use.parquet,
                              as_data_frame = TRUE)

formation_shapes= read_parquet(file = formations.parquet,
                               as_data_frame = TRUE)


###############################
# CREATE POINTS
###############################


seasons<-seasons %>%
      mutate(in_sample=1) %>%
filter(wh_season_id!="argentina_superliga_2016-2017") 
  


# drop argentina 2016 - weird, two seasons in a year! https://en.wikipedia.org/wiki/Argentine_Primera_Divisi%C3%B3n
# drop cup games, drop weird countries
matches <- matches %>%
  left_join(seasons, by="wh_season_id") %>%
  filter(in_sample==1) %>%
  select(home_teamid, away_teamid, home_goals_ft, away_goals_ft, 
         wh_season_id, wh_match_id, datetime) %>%
  mutate(season_id=wh_season_id) %>% 
  separate(wh_season_id, c("country", "league_name", "season_name"), sep = "_") %>%
  mutate(season=as.numeric(str_sub(season_name, 1, 4))) 

table(matches$season)
table(matches$country)
table(matches$league_name)

# create season_half: Feb 15 (changed 2021.12.14, to harmonize with the other season half definition)
matches %<>%
  mutate(dt= as_date(datetime),
         season_cutoff=make_date(year=season+1, month=2, day=15),
         season_diff = lubridate::as.duration(season_cutoff %--% dt) / ddays(1),
         season_half=ifelse(season_diff>=0, 2,1)
         ) 
# win: 3, draw 1
matches <- matches %>%
  mutate(home_points = case_when(home_goals_ft>away_goals_ft ~ 3, 
                             home_goals_ft==away_goals_ft ~ 1,
                             home_goals_ft<away_goals_ft ~ 0)) %>%
  mutate(away_points = case_when(home_goals_ft<away_goals_ft ~ 3, 
                                 home_goals_ft==away_goals_ft ~ 1,
                                 home_goals_ft>away_goals_ft ~ 0)) 
  
table(matches$home_points)

# creat basic match info
matches_info <- matches %>%
  select(wh_match_id, datetime, home_teamid, away_teamid, home_points, away_points,
         dt, season, season_half
)

matches %>%
  filter(league_name=="laliga") %>%
  group_by(season) %>%
  summarise(ave =mean(home_points))


######################################
# CREATE TEAM POINTS PER SEASON
######################################

glimpse(matches)

teams_home <-matches %>%
  group_by(season_id, season_half, home_teamid) %>%
  summarise(points_home=sum(home_points),
            teamid=mean(home_teamid)) %>%
  ungroup() %>%
  select(-home_teamid)

teams_away <-matches %>%
  group_by(season_id, season_half, away_teamid) %>%
  summarise(points_away=sum(away_points),
            teamid=mean(away_teamid)) %>%
  ungroup() %>%
  select(-away_teamid)

teams_season_half <-teams_home %>%
  left_join(teams_away, by=c("season_id","season_half" , "teamid")) %>%
  mutate(points=points_home+points_away,
         league_season=season_id) %>%
  separate(season_id, c("country", "league_name", "season_name"), sep = "_") %>%
  mutate(season=as.numeric(str_sub(season_name, 1, 4))) %>% 
  drop_na(points) 


glimpse(teams_season_half)


# save files -----------------

write_csv(matches_info, 
          paste0(data_tidy,"matches_info.csv"))

write_csv(teams_season_half, 
          paste0(data_tidy,"teams_season_half.csv"))


# drop data tables
rm(matches, seasons, formation_uses, formation_shapes, matches_info, 
   teams_season_half, teams_home, teams_away)

