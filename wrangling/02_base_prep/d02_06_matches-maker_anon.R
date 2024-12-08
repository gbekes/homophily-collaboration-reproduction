
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d02_06_matches-maker_anon.R

# team quality and match information maker (more league*season that used in paper)

# This version: v2.1: 2024-11-18
#######################################################



###############################
# LOAD DATA FROM PARQUET FILESS
###############################

matches = read_parquet(paste0(data_tidy, "anon_matches.parquet")) 

# from raw data 
data_wh     <- paste(data_raw,"t2_raw/wh/", sep = "/")
seasons = read_parquet(paste0(data_wh, "seasons.parquet"))


###############################
# CREATE POINTS
###############################

# includes all kinds of leagues, no bother.
seasons<-seasons %>%
      mutate(in_sample=1) %>%
filter(wh_season_id!="argentina_superliga_2016-2017") #drop argentina 2016 - weird, two seasons in a year! https://en.wikipedia.org/wiki/Argentine_Primera_Divisi%C3%B3n

# drop cup games, drop weird countries
matches <- matches %>%
  left_join(seasons, by="wh_season_id") %>%
  filter(in_sample==1) %>%
  select(anon_home_teamid, anon_away_teamid, home_goals_ft, away_goals_ft, 
         wh_season_id, wh_match_id, datetime) %>%
  mutate(season_id=wh_season_id) %>% 
  separate(wh_season_id, c("country", "league_name", "season_name"), sep = "_") %>%
  mutate(season=as.numeric(str_sub(season_name, 1, 4))) 

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
  

# creat basic match info
matches_info <- matches %>%
  select(wh_match_id, datetime, anon_home_teamid, anon_away_teamid, home_points, away_points,
         dt, season, season_half
)


######################################
# CREATE TEAM POINTS PER SEASON
######################################

teams_home <-matches %>%
  group_by(season_id, season_half, anon_home_teamid) %>%
  summarise(points_home=sum(home_points),
            anon_teamid=mean(anon_home_teamid)) %>%
  ungroup() %>%
  select(-anon_home_teamid)

teams_away <-matches %>%
  group_by(season_id, season_half, anon_away_teamid) %>%
  summarise(points_away=sum(away_points),
            anon_teamid=mean(anon_away_teamid)) %>%
  ungroup() %>%
  select(-anon_away_teamid)

teams_season_half <-teams_home %>%
  left_join(teams_away, by=c("season_id","season_half" , "anon_teamid")) %>%
  mutate(points=points_home+points_away,
         league_season=season_id) %>%
  separate(season_id, c("country", "league_name", "season_name"), sep = "_") %>%
  mutate(season=as.numeric(str_sub(season_name, 1, 4))) %>% 
  drop_na(points) 



# save files -----------------

write_csv(matches_info, 
          paste0(data_tidy_created,"anon_matches_info.csv"))

write_csv(teams_season_half, 
          paste0(data_tidy_created,"anon_teams_season_half.csv"))

# drop data
rm(matches, matches_info, teams_home, teams_away, teams_season_half, seasons)