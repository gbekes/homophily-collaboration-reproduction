
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d02_09_seasonhalf_events_anon.R

# player data maker
# We create seasonhalf level aggregates here


# This version: v2.1: 2024-11-18
#######################################################

 

# Load datasets

all_player_minutes= read_parquet(paste0(data_tidy,"anon_all_player_minutes.parquet"))
all_player_minutes %<>%
  ungroup()

# all_player_minutes %<>% mutate(wh_match_id = as.numeric(wh_match_id)) 
  

matches_info = read_csv(paste0(data_tidy_created,"anon_matches_info.csv"))
matches_info %<>% ungroup()
matches_info %<>%
  mutate(wh_match_id = as.character(wh_match_id)) 



market_values_season =read_parquet(paste0(data_tidy_created,"anon_market_values_season.parquet"))
market_values_season %<>% ungroup()

players=read_parquet(paste0(data_tidy,"anon_players.parquet"))


MV_transf_joint = read_parquet(paste0(data_tidy_created,"anon_MV_transf_joint.parquet"))
MV_transf_joint %<>% ungroup() 


teams_season = read_csv(paste0(data_tidy_created,"anon_teams_season_half.csv"))

# this is the augmented country language distance matrix
ling_web_augmented = read_csv(paste0(data_tidy_created,"ling_web_augmented.csv"))


## shared minutes passes
skeleton = read_parquet(paste0(data_tidy, "anon_player_passcounts_shared_minutes_major_leagues.parquet"))
 
skeleton %<>%
  mutate(wh_match_id = as.character(wh_match_id)) 

# created in d1_10  
df_shared= read_parquet(file = paste0(data_tidy_created, "anon_df_shared_time_teams.parquet"),
                        as_data_frame = TRUE)



############################################################
## I. all_events_season-half_major.parquet
############################################################

library(tictoc)
# we do it separately for each major league, and then conjoin. may need to do it in two parts
major_leagues
for(i in 1: length(major_leagues)){
  
  print(major_leagues[i])

  tic("start")
  
    all_events_match = read_parquet(paste0(data_tidy,"anon_all_events_agg_major_leagues.parquet"))
  
  all_events_match = all_events_match %>% 
    filter(paste0(country, "__", league_name) %in% major_leagues[i])
  
  relevant_matches = all_events_match %>% distinct(wh_match_id) %>% pull()
  
  # Hmisc::describe(all_events_match$season_name)
  # 380*


  #join minutes
  ######################################
  ######## shared minutes correction: 13/01/2022, we create a full panel, given there was a pass ever
  ######################################
  minutes_tmp1 = all_player_minutes %>%
    filter(wh_match_id %in% relevant_matches) %>% 
    select(wh_match_id, anon_player_id1 = anon_player_id, 
           min_minute1 = min_minute,
           max_minute1 = max_minute)
  
  minutes_tmp2 = all_player_minutes %>%
    filter(wh_match_id %in% relevant_matches) %>%
    select(wh_match_id, anon_player_id2 = anon_player_id, 
           min_minute2 = min_minute,
           max_minute2 = max_minute)
  
  minutes_tmp = minutes_tmp1 %>% 
    left_join(minutes_tmp2,
              by = "wh_match_id")
  # eliminate surplus
  rm(minutes_tmp1, minutes_tmp2)
  
  ## create the shared minutes for everyone
  minutes_tmp = minutes_tmp %>% 
    filter(anon_player_id1 != anon_player_id2) %>%
    mutate(minutes_player1=max_minute1-min_minute1, 
           minutes_player2=max_minute2-min_minute2) %>%
    mutate(minutes_shared= pmin(max_minute1, max_minute2) - pmax(min_minute1,min_minute2)) %>%
    mutate(minutes_shared = ifelse(minutes_shared<=0, 0, minutes_shared))
  
  # create the minutes for season-half level
  minutes_tmp = minutes_tmp %>% 
    left_join(matches_info %>% select(wh_match_id, season, season_half),
              by = "wh_match_id"
    ) %>% 
    group_by(season, season_half, anon_player_id1, anon_player_id2) %>% 
    summarise(games = sum(minutes_shared > 0), ## how many games did they have shared minutes?,
              minutes_shared = sum(minutes_shared, na.rm = TRUE),
              minutes_player1 = sum(minutes_player1, na.rm = TRUE),
              minutes_player2 = sum(minutes_player2, na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  # Hmisc::describe(event_player$minutes_shared)
  #write_csv(event_player, paste0(data_tidy,"all_event_player.csv"))
  
  all_events_match <- all_events_match %>%                                          
    mutate(season=as.numeric(sapply(strsplit(season_name, "-"), "[[", 1))) %>%
    arrange(country, league_name, season, anon_teamid, anon_player_id1,anon_player_id2 ) 
  
  # add player-year market values
  to_app = "_p1"
  temp_p1<-players %>%
    select(anon_player_id, anon_other_player_id) %>%
    setNames(paste0(names(.), to_app)) %>%
    rename(anon_player_id1=anon_player_id_p1)
  
  all_events_match <- all_events_match %>%
    left_join(temp_p1, by="anon_player_id1") 
  
  to_app = "_p2"
  temp_p2<-players %>%
    select(anon_player_id, anon_other_player_id) %>%
    setNames(paste0(names(.), to_app)) %>%
    rename(anon_player_id2=anon_player_id_p2)
  
  all_events_match <-all_events_match %>%
    left_join(temp_p2, by="anon_player_id2")
  
  temp_p1<-market_values_season %>%
    rename(anon_other_player_id_p1= anon_other_player_id,
           value_p1=value)
  
  all_events_match <-all_events_match %>%
    left_join(temp_p1, by=c("season", "anon_other_player_id_p1"))
  
  temp_p2<-market_values_season %>%
    rename(anon_other_player_id_p2= anon_other_player_id,
           value_p2=value)
  
  all_events_match <-all_events_match %>%
    left_join(temp_p2, by=c("season", "anon_other_player_id_p2"))
  
  
  
  # just a check
  all_events_match %<>%  
    filter(season_name!="2020-2021" ) %>%    
    filter(season_name!="2011-2012")  
  
  
  # THIS IS just a built in check 
  # should yield no action
  teams_error<-all_events_match %>%
    group_by(anon_teamid) %>%
    tally() 
  
  # Hmisc::describe(teams_error$n)
  
  nrow(all_events_match)
  all_events_match <- all_events_match %>%
    left_join(teams_error, by="anon_teamid") %>%
    filter(n>100) %>%
    select(-n)
  nrow(all_events_match)
  

  ###################################################
  # p1_var-prep-match.R
  ###################################################
  
  all_events_match<-all_events_match %>% 
    mutate(anon_teamid_f =          as.factor(anon_teamid),
           country =           as.factor(country),
           league_name =       as.factor(league_name), 
           season_name =       as.factor(season_name)
    )
  
  all_events_match %<>%
    mutate(
      league_ctry=paste(country, league_name, sep = "_"), 
      league_ctry_f=as.factor(league_ctry), 
      league_season=paste(country, league_name, season_name, sep = "_"), 
      league_season_f=as.factor(league_season)
    )
  
  # take all events
  all_events_match %<>%
    left_join(matches_info %>% select(-season), by="wh_match_id")

    
  temp_id1 <-  MV_transf_joint %>%
    select(-anon_other_player_id, -day_id) %>%
    rename_at(vars(-season, -season_half, -anon_player_id, -country_id), paste0, "_id1") %>%
    rename_at(vars(anon_player_id, country_id), paste0, "1") 
  
  temp_id2 <-  MV_transf_joint %>%
    select(-anon_other_player_id, -day_id) %>%
    rename_at(vars(-season, -season_half, -anon_player_id, -country_id), paste0, "_id2") %>%
    rename_at(vars(anon_player_id, country_id), paste0, "2") 
  
  temp_id1 %<>% 
    mutate(season=as.numeric(season),
           season_half=as.numeric(season_half))

  temp_id2 %<>% 
    mutate(season=as.numeric(season),
           season_half=as.numeric(season_half))
  
    
  all_events_match %<>%
    left_join(temp_id1, by=c("season", "season_half", "anon_player_id1"))
  
  all_events_match %<>%
    left_join(temp_id2, by=c("season", "season_half", "anon_player_id2"))

# drop when anon_teamid_id1 or id2 missing (not needed)
#  nrows(all_events_match)
#  all_events_match %<>%
#    filter(!is.na(anon_teamid_id1) & 
#           !is.na(anon_teamid_id2))
#  nrows(all_events_match)

  toc()
  
  #########################################################
  # create sum of passes dataset
  #########################################################
  
  tic("passes")
  
#  skim(all_events_match)
  match_level_passes<-all_events_match %>%
    group_by(wh_match_id, anon_teamid ) %>%
    summarise(match_pass_count=sum(total_pass_count)
    )
  
  
  
  # new broad summary for weather project
    # Variables to sum
  sum_vars <- c("total_pass_count", "pass_risky")
  
  # Variables to average
  avg_vars <- c("value_p1", "forwardness", "avg_pass_length", "anon_home_teamid", "anon_away_teamid", "home_points", "away_points")
  
  aggregated_data <- all_events_match %>%
    group_by(wh_match_id, anon_teamid) %>%
    summarize(
      # Sum variables
      across(
        all_of(sum_vars) | contains("pass_sequence_count") | contains("pass_sequence_count_forward"),
        sum,
        na.rm = TRUE,
        .names = "{col}_sum"
      ),
      # Average variables
      across(
        all_of(avg_vars),
        mean,
        na.rm = TRUE,
        .names = "{col}_avg"
      )
    )
  

  toc()

  write_parquet(match_level_passes, paste0(data_tidy_created, "anon_match_level_passes", major_leagues[i] , ".parquet"))
  
  # save
  # glimpse(all_events_match)
  all_events_match = all_events_match %>% as.data.frame()
  
  ############################### 
  # Create and filter
  ############################### 
  
  tic("filter")
  
  #skeleton %<>%
  #  rename(anon_player_id1=ID1n, anon_player_id2=ID2n)
  
  ## add shared minute passes
  all_events_match %<>%
    left_join(skeleton, by=c("wh_match_id", "anon_player_id1", "anon_player_id2"))
  
  all_events_match %<>%
    mutate(passes_in_shared_mins1 = if_else(is.na(passes_in_shared_mins1), 0L, passes_in_shared_mins1),
           passes_in_shared_mins2 = if_else(is.na(passes_in_shared_mins2), 0L, passes_in_shared_mins2)
           )
  
  ##########################################
  ## p3-aggreg_seasonhalf_01.R
  ##########################################
  
  #######################################
  # PART I
  # 
  # aggregate to create p1 -p2 -season-half level dataset
  #######################################
  
  all_events_match %<>%  
    mutate(season_name_half=paste(season_name,season_half, sep="_h")
    )
  # create season length
  all_events_match %<>% 
    group_by(anon_teamid, season_name_half) %>%
    mutate(season_length=n_unique(wh_match_id) )%>%
    ungroup()
  
  
  # table(all_events_match$sh_length, all_events_match$league_ctry)
  # table(all_events_match$season_length)
  # glimpse(all_events_match)
  
  
  #################################################################################################
  # player1-player2-team-game level aggregation
  #################################################################################################
  
  
  all_events_match %<>%
    mutate(avg_pass_length=if_else(avg_pass_length<0.1, 0.1,avg_pass_length),
           w_club_days_id1 = replace(w_club_days_id1, w_club_days_id1 == 0 | is.na(w_club_days_id1) , 1),
           w_club_days_id2 = replace(w_club_days_id2, w_club_days_id2 == 0 | is.na(w_club_days_id2) , 1),
    )
  # Hmisc::describe(all_events_match$minutes_player1)
  
  all_events_match %<>%
    mutate(ln_avg_pass_length=log(avg_pass_length)) 
  
  all_events_match  %<>%
    mutate(
      ln_pass_count=log(total_pass_count)
    )
  
  
  all_events_match %<>%
    mutate(passes_in_shared_mins=passes_in_shared_mins1+passes_in_shared_mins2,
           ln_passes_in_shared_mins=log(passes_in_shared_mins))
  
  all_events_match = all_events_match %>% 
    group_by(anon_teamid, season) %>%
    mutate( value_p1= replace(value_p1, is.na(value_p1), mean(value_p1, na.rm=TRUE)),
            value_p2= replace(value_p2, is.na(value_p2), mean(value_p2, na.rm=TRUE)),
            value_p1 = ifelse(value_p1==0, 100000, value_p1),
            value_p2 = ifelse(value_p2==0, 100000, value_p2)
    ) %>%
    ungroup()
  
  
  
  ################################
  # MAY NEED TO ADD LN(PASS SEQUENCE)
  #################################
    data2dt<-data.table::data.table(all_events_match)
    rm(all_events_match)
    
      
  # to check whether minutes are correct
  # data2dt %>% 
  #   filter((anon_player_id1 == 845 & anon_player_id2 == 1443) | 
  #            (anon_player_id1 == 1443 & anon_player_id2 == 845),
  #          season == 2012, season_half == 1
  #   ) %>% 
  #   select(season, season_half, wh_match_id, anon_teamid, anon_player_id1, anon_player_id2, contains("minutes"))
  
  # not used now
  # ln_minutes_shared_geom=mean(ln_minutes_shared),
  # ln_pass_per_min_geom=mean(ln_pass_per_min), 
  
    toc()
    
    tic("tidytable")
    
  # cutting bits
  data3dt<-data2dt %>%
    tidytable::summarize(pass_count=sum(total_pass_count),
               passes_in_shared_mins=sum(passes_in_shared_mins),
               passes_in_shared_mins1=sum(passes_in_shared_mins1),
               passes_in_shared_mins2=sum(passes_in_shared_mins2),
               ln_pass_count_geom=mean(ln_pass_count),
               #across.(c(pass_per_min), mean),
               across(c(cross_pass, key_pass, attack, pass_neargoal, pass_risky), sum),
               across(c(forwardness, avg_pass_length, ln_avg_pass_length), mean),
               across(starts_with("pass_sequence"), sum),
               across(starts_with("n_clubs"), mean),
               across(c(value_p1, value_p2), mean),               
               across(c(w_club_days_id1, w_club_days_id2, w_club_days_group_id1, w_club_days_group_id2,
                         w_club_days_long_id1, w_club_days_long_id2), mean),
               across(starts_with(c("young", "new", "unexperience" )), max),
               across(starts_with(c("league", "season", "is_loan", "joined")), first),
               across(starts_with(c("anon_other_player_id_p1", "anon_other_player_id_p2")), first),
               # games=n_distinct.(wh_match_id),
               .by = c(anon_teamid, anon_player_id1, anon_player_id2, season, season_half, season_name_half) 
    ) %>% 
    tidytable::left_join(.,
              minutes_tmp,
              .by = c("season", "season_half", "anon_player_id1", "anon_player_id2")
              )
  
  toc()
  tic("variable creation")
  # variable creation
  data3dt  %<>%
    mutate(
      ln_w_club_p1=log(w_club_days_id1),
      ln_w_club_p2=log(w_club_days_id2),
      ln_pass_count=log(pass_count),
      ln_minutes_shared=log(minutes_shared ),
      ln_passes_in_shared_mins1=log(passes_in_shared_mins1),
      ln_passes_in_shared_mins2=log(passes_in_shared_mins2),
      ln_passes_in_shared_mins=log(passes_in_shared_mins),
      ln_pass_per_min = ln_pass_count-ln_minutes_shared,
      ln_minutes_shared_avg= log(minutes_shared/games),
      pass_per_min = pass_count / minutes_shared
    ) 

  # check
# replace -Inf with 0
#  data3dt = data3dt %>% 
#    mutate(across(where(is.numeric), ~replace(., is.infinite(.), 0)))
  # write_parquet(all_events_match, paste0(data_tidy,"all_events_match_major_leagues.parquet"))

    write_parquet(data3dt, paste0(data_tidy_created,"all_events_season-half_", major_leagues[i], ".parquet"))
  
  toc()
  
  # delete unused objects
  rm(data2dt)
  rm(data3dt)
  
  
}  


##################################
## Now aggregate them
##################################

all_events_season_half = data.frame()
for(i in 1:length(major_leagues)){
  all_events_season_half_tmp = read_parquet(paste0(data_tidy_created,"all_events_season-half_", major_leagues[i], ".parquet"))
  all_events_season_half = rbind(all_events_season_half,
                                 all_events_season_half_tmp
  )
  
}
i
###############################################
## Combine with shared time in past teams
###############################################

df_shared %<>% 
  mutate(season=as.numeric(season),
         season_half=as.numeric(season_half)
  )

all_events_season_half = all_events_season_half %>%
  left_join(df_shared,
            by = c("anon_other_player_id_p1" = "anon_other_player1",
                   "anon_other_player_id_p2" = "anon_other_player2",
                   "season", "season_half"
            )
  ) 


all_events_season_half = all_events_season_half %>%
  mutate(shared_teams_count=ifelse(is.na(shared_teams_count),1,shared_teams_count),
         shared_time_hs_count=ifelse(is.na(shared_time_hs_count),1,shared_time_hs_count)
  )


###############################################
# save the dataset
###############################################

write_parquet(all_events_season_half, paste0(data_tidy_created, "all_events_season-half_major.parquet"))

# drop the data
rm(df_shared)
rm(all_events_season_half)
rm(all_events_season_half_tmp)

rm(aggregated_data,match_level_passes, 
   minutes_tmp, temp_id1, temp_id2, teams_error,  temp_p1, temp_p2)
