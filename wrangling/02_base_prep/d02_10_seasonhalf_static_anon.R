
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d02_10_seasonhalf_static_anon.R

# We create seasonhalf level data adding many new variables

# run time warning: takes time

# This version: v2.1: 2024-11-18
#######################################################




# load the data
players=read_parquet(paste0(data_tidy,"anon_players.parquet"))

all_events_season = read_parquet(paste0(data_tidy_created,"all_events_season-half_major.parquet"))

######################################
# combination ofwith player data
######################################

# take all the player-player-season connections from the events dataset. 
# player-languages table, rotate it to wide, attach to the player-player-season table, and then calculate the common language variable.

for(pp in c("1", "2")){
  
  to_app = paste0("_p", pp)
  temp = players %>%
    select(broad_position,specific_position,  anon_player_id, `citizenship-0`, `citizenship-1`, national_team_nation,
           dob, height, place_of_birth_country ) %>%
    rename(citizenship_v0=`citizenship-0`,
           citizenship_v1=`citizenship-1`,
           citizenship_v2 = place_of_birth_country
           ) %>%
    setNames(paste0(names(.), to_app))
  
  if(pp == "1"){
    wh_name = "anon_player_id1"
  }
  else{
    wh_name = "anon_player_id2"
  }
  
  names(temp)[names(temp) == paste0("anon_player_id_p", pp)] = wh_name

  all_events_season <- all_events_season %>%
    left_join(temp, by=wh_name) 
  rm(temp)
  
  
}

nrow1=nrow(all_events_season)
# drop when missing first nat
all_events_season %<>%
  filter(!is.na(anon_other_player_id_p1) & !is.na(anon_other_player_id_p2) ) %>%
  filter(anon_player_id1!=0, anon_player_id2!=0)
nrow2=nrow(all_events_season)

#dropped due to missing player ID
print (nrow1-nrow2)

#################################
## Create other variables
#################################

# some additinal vars  
all_events_season<-all_events_season %>%
  mutate(start=as.Date(paste(season, 09, 01, sep = "-")),
         age_p1_y= as.duration(dob_p1  %--% start) / dyears(1),
         age_p2_y= as.duration(dob_p2  %--% start) / dyears(1)
  )

all_events_season %<>%
  mutate_at(vars(height_p1, height_p2),
            ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))



# create sample means by teams and seasons and replace missing + 0
all_events_season = all_events_season %>% 
  group_by(anon_teamid, season) %>%
  mutate( age_p1_y= replace(age_p1_y, is.na(age_p1_y), mean(age_p1_y, na.rm=TRUE)),
          age_p2_y= replace(age_p2_y, is.na(age_p2_y), mean(age_p2_y, na.rm=TRUE))
  ) %>%
  ungroup()


# ensure no missing. Days is OK, some values missing in country
all_events_season<-all_events_season %>% 
  mutate(
    w_club_days_id1 = replace(w_club_days_id1, is.na(w_club_days_id1), 699),
    w_club_days_id2 = replace(w_club_days_id2, is.na(w_club_days_id2), 699), 
    w_club_days_id1 = replace(w_club_days_id1, is.na(w_club_days_id1), 699),
    w_club_days_id2 = replace(w_club_days_id2, is.na(w_club_days_id2), 699), 
    
    n_clubs_same_country_id1 = replace(n_clubs_same_country_id1, is.na(n_clubs_same_country_id1), 1),
    n_clubs_same_country_id2 = replace(n_clubs_same_country_id2, is.na(n_clubs_same_country_id2), 1), 
    n_clubs_other_country_id1 = replace(n_clubs_other_country_id1, is.na(n_clubs_other_country_id1), 0),
    n_clubs_other_country_id2 = replace(n_clubs_other_country_id2, is.na(n_clubs_other_country_id2), 0), 
    
    )


# position edits
all_events_season<-all_events_season %>% 
  mutate(
    broad_position_p1 = replace(broad_position_p1, is.na(broad_position_p1), "Midfielder"),
    broad_position_p2 = replace(broad_position_p2, is.na(broad_position_p2), "Midfielder"))


all_events_season<-all_events_season %>% 
  mutate(
    specific_position_p1 = replace(specific_position_p1, 
                                   (is.na(specific_position_p1) & broad_position_p1=="Goalkeeper"), "Goalkeeper"),
    specific_position_p1 = replace(specific_position_p1, 
                                   (is.na(specific_position_p1) & broad_position_p1!="Goalkeeper"), "Central Midfield"),
    specific_position_p2 = replace(specific_position_p2, 
                                   (is.na(specific_position_p2) & broad_position_p2=="Goalkeeper"), "Goalkeeper"),
    specific_position_p2 = replace(specific_position_p2, 
                                   (is.na(specific_position_p2) & broad_position_p2!="Goalkeeper"), "Central Midfield")
    
  )


# variable creation
all_events_season %<>%
  mutate(
    age_diff=abs(age_p1_y -age_p2_y),
    joined_recent_p1=ifelse(w_club_days_id1<180,1,0),
    joined_recent_p2=ifelse(w_club_days_id2<180,1,0),
    joined_long_p1=ifelse(w_club_days_id1>700,1,0),
    joined_long_p2=ifelse(w_club_days_id2<700,1,0),
  ) 

# creating logs
all_events_season %<>%
  mutate(
    ln_pass_count=log(pass_count),
    ln_minutes_shared=log(minutes_shared ),
    ln_pass_permin = ln_pass_count-ln_minutes_shared,
    ln_value_p1=log(value_p1 ),
    ln_value_p2=log(value_p2 ),
    ln_w_club_p1=log(w_club_days_id1+1),
    ln_w_club_p2=log(w_club_days_id2+1)
  ) 



all_events_season = all_events_season %>% 
  mutate(broad_position_p1 = as.factor(broad_position_p1),
         broad_position_p2 = as.factor(broad_position_p2)
  )
glimpse((all_events_season))

# show freq of 1
all_events_season %<>%
  group_by(season_name_half, anon_player_id1) %>%
  mutate(nnn_partners_p1_new=n_distinct(anon_player_id2)) %>%
  ungroup()
tabyl(all_events_season$nnn_partners_p1_new)

################################x
# Nationality
################################x

# Deal with missings

all_events_season = all_events_season %>% 
  
  mutate(citizenship_v0_p1 = case_when(is.na(citizenship_v0_p1) ~ "missing p1",
                                       TRUE ~ citizenship_v0_p1
                                       ),
         citizenship_v1_p1 = case_when(is.na(citizenship_v1_p1) ~ "missing p1",
                                       TRUE ~ citizenship_v1_p1
                                       ),
         citizenship_v2_p1 = case_when(is.na(citizenship_v2_p1) ~ "missing p1",
                                       TRUE ~ citizenship_v2_p1
                                       ),
         # player 2
         citizenship_v0_p2 = case_when(is.na(citizenship_v0_p2) ~ "missing p2",
                                       TRUE ~ citizenship_v0_p2
                                       ),
         citizenship_v1_p2 = case_when(is.na(citizenship_v1_p2) ~ "missing p2",
                                       TRUE ~ citizenship_v1_p2
                                       ),
         citizenship_v2_p2 = case_when(is.na(citizenship_v2_p2) ~ "missing p2",
                                       TRUE ~ citizenship_v2_p2
                                       )
         )

##################################
# create same nat variables
##################################

# We create adjusted nationalities, by merging countries. 
# join country chars for Team GB

# v0
all_events_season %<>%
  mutate(citizenship_adj_v0_p1=citizenship_v0_p1) %>%
  mutate(citizenship_adj_v0_p1= case_when(
    citizenship_v0_p1=="Scotland" ~ "GB",
    citizenship_v0_p1=="England"  ~ "GB",
    citizenship_v0_p1=="Wales"    ~ "GB",
    TRUE ~ citizenship_adj_v0_p1)
  ) %>%
  mutate(citizenship_adj_v0_p2=citizenship_v0_p2) %>%
  mutate(citizenship_adj_v0_p2= case_when(
    citizenship_v0_p2=="Scotland" ~ "GB",
    citizenship_v0_p2=="England"  ~ "GB",
    citizenship_v0_p2=="Wales"    ~ "GB",
    TRUE ~ citizenship_adj_v0_p2)
  )

#v1
all_events_season %<>%
  mutate(citizenship_adj_v1_p1=citizenship_v1_p1) %>%
  mutate(citizenship_adj_v1_p1= case_when(
    citizenship_v1_p1=="Scotland" ~ "GB",
    citizenship_v1_p1=="England"  ~ "GB",
    citizenship_v1_p1=="Wales"    ~ "GB",
    TRUE ~ citizenship_adj_v1_p1)
  ) %>%
  mutate(citizenship_adj_v1_p2=citizenship_v1_p2) %>%
  mutate(citizenship_adj_v1_p2= case_when(
    citizenship_v1_p2=="Scotland" ~ "GB",
    citizenship_v1_p2=="England"  ~ "GB",
    citizenship_v1_p2=="Wales"    ~ "GB",
    TRUE ~ citizenship_adj_v1_p2)
  )

#v2
all_events_season %<>%
  mutate(citizenship_adj_v2_p1=citizenship_v2_p1) %>%
  mutate(citizenship_adj_v2_p1= case_when(
    citizenship_v2_p1=="Scotland" ~ "GB",
    citizenship_v2_p1=="England"  ~ "GB",
    citizenship_v2_p1=="Wales"    ~ "GB",
    TRUE ~ citizenship_adj_v2_p1)
  ) %>%
  mutate(citizenship_adj_v2_p2=citizenship_v2_p2) %>%
  mutate(citizenship_adj_v2_p2= case_when(
    citizenship_v2_p2=="Scotland" ~ "GB",
    citizenship_v2_p2=="England"  ~ "GB",
    citizenship_v2_p2=="Wales"    ~ "GB",
    TRUE ~ citizenship_adj_v2_p2)
  )

### home country
all_events_season %<>%
  mutate(league_ctry2 = league_ctry) %>%
  separate(league_ctry2, c("country", "league"), sep = "_") %>%
  select(-league)


# NOTE
# all what comes based on same_nat_any and not same_nat_any2 (ie grouped countries)


############################### 
# young, inexperienced
############################### 

# define young
young_cutoff1=22 # often used cutoff in industry 22 or 23

all_events_season %<>%
  mutate(young_id1 = if_else(age_p1_y<young_cutoff1,1,0),
         young_id2 = if_else(age_p2_y<young_cutoff1,1,0)
  ) %>% 
  mutate( young_either1 =if_else((young_id1==1 | young_id2==1 ),1,0),
          young_both1   =if_else((young_id1==1 & young_id2==1 ),1,0)
  )


# define inexperienced
experience_cutoff1=100 # new, coming in the recent window for the season #
experience_cutoff2=300 # newish, came in now or previous window #

all_events_season %<>%
  mutate(new1_id1= if_else(w_club_days_id1<experience_cutoff1,1,0),
         new1_id2= if_else(w_club_days_id2<experience_cutoff1,1,0),
         new2_id1= if_else(w_club_days_id1<experience_cutoff2,1,0),
         new2_id2= if_else(w_club_days_id2<experience_cutoff2,1,0)
  )

all_events_season %<>%
  mutate(
    unexperience_either1 =if_else((
      w_club_days_id1<experience_cutoff1 | 
        w_club_days_id2<experience_cutoff1),1,0),
    unexperience_both1 =if_else((
      w_club_days_id1<experience_cutoff1 & 
        w_club_days_id2<experience_cutoff1),1,0),
    unexperience_either2 =if_else((
      w_club_days_id1<experience_cutoff2 | 
        w_club_days_id2<experience_cutoff2),1,0),
    unexperience_both2 =if_else((
      w_club_days_id1<experience_cutoff2 & 
        w_club_days_id2<experience_cutoff2),1,0),
    unexperience_both12 =if_else((
      w_club_days_id1<experience_cutoff2 & 
        w_club_days_id2<experience_cutoff2 &
        (w_club_days_id1>=experience_cutoff1 | 
           w_club_days_id2>=experience_cutoff1)),1,0),
  )


all_events_season %<>%
  mutate(unexperience_both1long =if_else((
    w_club_days_long_id1<experience_cutoff1 & 
      w_club_days_long_id2<experience_cutoff1),1,0),
    unexperience_both2long =if_else((
      w_club_days_long_id1<experience_cutoff2 & 
        w_club_days_long_id2<experience_cutoff2),1,0),
    
    unexperience_both1group =if_else((
      w_club_days_group_id1<experience_cutoff1 & 
        w_club_days_group_id2<experience_cutoff1),1,0),
    unexperience_both2group =if_else((
      w_club_days_group_id1<experience_cutoff2 & 
        w_club_days_group_id2<experience_cutoff2),1,0)
  )

all_events_season %<>%
  mutate(unexperience_both12group =if_else((
    w_club_days_group_id1<experience_cutoff2 & 
      w_club_days_group_id2<experience_cutoff2 &
      (w_club_days_group_id1>=experience_cutoff1 | 
         w_club_days_group_id2>=experience_cutoff1)),1,0),
  )

## Save end file

write_parquet(all_events_season, paste0(data_tidy_created, "anon_all_events_season-half_major_full.parquet"))

# drop the data
rm(all_events_season, players)

