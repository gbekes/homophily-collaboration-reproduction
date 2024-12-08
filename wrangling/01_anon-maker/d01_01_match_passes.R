
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#
# d01_01_match_passes.R

# reads in parquet
# saves tidy tables for analysis

# All raw data used. 
# Runtime warning: 20 mins

# This version: v2.1: 2024-11-18
#######################################################



### PLAYERS file, needed for ID generation
players <- read_parquet(file = players_coref.parquet,
                        as_data_frame = TRUE)

player_info <- read_parquet(file = player_info.parquet,
                            as_data_frame = TRUE)

players = players %>% 
  left_join(.,
            player_info,
            by = c("tm_player_id" = "tm_player_id")
  )

#write_csv(players, paste0(data_tidy_temp,"players.csv"))
write_parquet(players, paste0(data_tidy_temp,"players.parquet"))

############### 
# EVENT
############### 

pattern_all = c('*', '*') # just a placeholder

listpq <- dir(data_events, pattern=paste0(pattern_all, collapse="|")) 
listpq2 <- listpq
listpm   <- gsub("^", "player-minutes_", listpq)
listagg  <- gsub("^", "events-agg_", listpq)
listcsv1  <- gsub(".parquet", "", listpq)
listcsv1

for (i in 1:length(listpq)){
  filename <- paste0("events")
  filename_i = listpq[i]
  
  assign(filename, read_parquet(file = paste0(data_events, filename_i),
                                as_data_frame = TRUE))
  
  a=nrow(events)
  print(listpq[i])
  
  # create mins
  player_minutes <-  events %>%
    group_by(wh_match_id, wh_player_id) %>%
    summarise(
      min_minute=min(minute),
      max_minute=max(minute),
      teamid=mean(teamid)
    ) %>%
    mutate(total_minutes=max_minute-min_minute)
  
write_parquet(player_minutes, (file.path(data_tidy_temp, listpm[i])))
  
  
  events <- events %>% 
    filter(type=="Pass",
           outcometype=="Successful",
           # clearing, 2021.04.08.
           !is.na(wh_player_id), # not missings!
           !is.na(player_with_next_touch), # not missings!
           wh_player_id != 0,
           player_with_next_touch != 0,
           wh_player_id != player_with_next_touch
           
    ) %>% 
    select(period, type, outcometype, event_side, zone,
           wh_match_id, id, eventid, minute, second, teamid, 
           x,y, istouch, wh_player_id, passendx, passendy, player_with_next_touch,
           keypass, bigchancecreated, intentionalassist, intentionalgoalassist, shotassist,
           throughball, cross,
           forwardness, length, ## added 15/08/2021: forward passes and pass lengths
           distance_from_opp_goal ## added 08/02/2023
    )
  
  
  a=nrow(events)
  listpq[i]
  
    #################################
    # PASS SEQUENCES 
    #################################
  
  
  ## First: calculate different types of pass sequences on the unit level (names are s.t. ABA = A -> B -> A)
  events <- events %>%
    mutate(ABA_pass = case_when(dplyr::lag(player_with_next_touch, 1) == wh_player_id & 
                                  player_with_next_touch == dplyr::lag(wh_player_id, 1) &
                                  wh_player_id != player_with_next_touch &              # pass to himself will not count, that should not be a pass
                                  dplyr::lag(wh_player_id, 1) != dplyr::lag(player_with_next_touch, 1) # saját maga ne számítson
                                ~ 1L,
                                TRUE ~ 0L
    ),
    ABAB_pass = case_when(ABA_pass == 1 & dplyr::lag(ABA_pass, 1) == 1 ~ 1L,
                          TRUE ~ 0L),
    ABABA_pass = case_when(ABA_pass == 1 & dplyr::lag(ABA_pass, 1) == 1 & 
                             dplyr::lag(ABA_pass, 2) == 1
                           ~ 1L,
                           TRUE ~ 0L),
    # correction --- if do not account for the ABABAB passes, might get negative net values sometimes
    ABABAB_pass = case_when(ABA_pass == 1 & dplyr::lag(ABA_pass, 1) == 1 & 
                              dplyr::lag(ABA_pass, 2) == 1 & dplyr::lag(ABA_pass, 3) == 1
                            ~ 1L,
                            TRUE ~ 0L),
    ABABABA_pass = case_when(ABA_pass == 1 & dplyr::lag(ABA_pass, 1) == 1 & 
                               dplyr::lag(ABA_pass, 2) == 1 & dplyr::lag(ABA_pass, 3) == 1 & dplyr::lag(ABA_pass, 4) == 1
                             ~ 1L,
                             TRUE ~ 0L),
    ABABABAB_pass = case_when(ABA_pass == 1 & dplyr::lag(ABA_pass, 1) == 1 & 
                                dplyr::lag(ABA_pass, 2) == 1 & dplyr::lag(ABA_pass, 3) == 1 & dplyr::lag(ABA_pass, 4) == 1
                              ~ 1L,
                              TRUE ~ 0L),
    ABABABABA_pass = case_when(ABA_pass == 1 & dplyr::lag(ABA_pass, 1) == 1 & 
                                 dplyr::lag(ABA_pass, 2) == 1 & dplyr::lag(ABA_pass, 3) == 1 & dplyr::lag(ABA_pass, 4) == 1 &
                                 dplyr::lag(ABA_pass, 5) == 1 & dplyr::lag(ABA_pass, 6) == 1
                               ~ 1L,
                               TRUE ~ 0L),      
    
    
    ABCA_pass = case_when(player_with_next_touch == dplyr::lag(wh_player_id, 2) &
                            wh_player_id == dplyr::lag(player_with_next_touch, 1) &
                            dplyr::lag(player_with_next_touch, 2) == dplyr::lag(wh_player_id, 1) &
                            # make sure it is a pass to someone else
                            wh_player_id != player_with_next_touch &
                            dplyr::lag(wh_player_id, 1) != dplyr::lag(player_with_next_touch, 1) &
                            dplyr::lag(wh_player_id, 2) != dplyr::lag(player_with_next_touch, 2) ~ 1L,
                          TRUE ~ 0L
    ),
    # corrected on 2021.04.05.: we have to account that A != B!= C != D as well for each of these
    ABCDA_pass = case_when(player_with_next_touch == dplyr::lag(wh_player_id, 3) &
                             wh_player_id == dplyr::lag(player_with_next_touch, 1) &
                             dplyr::lag(player_with_next_touch, 2) == dplyr::lag(wh_player_id, 1) &
                             dplyr::lag(player_with_next_touch, 3) == dplyr::lag(wh_player_id, 2) & 
                             # make sure it is a pass to someone else
                             wh_player_id != player_with_next_touch &
                             dplyr::lag(wh_player_id, 1) != dplyr::lag(player_with_next_touch, 1) &
                             dplyr::lag(wh_player_id, 2) != dplyr::lag(player_with_next_touch, 2)  & 
                             dplyr::lag(wh_player_id, 3) != dplyr::lag(player_with_next_touch, 3) &
                             
                             # make sure it is different players touching the ball
                             dplyr::lag(wh_player_id, 2) != player_with_next_touch &
                             dplyr::lag(wh_player_id, 1) != player_with_next_touch &
                             dplyr::lag(wh_player_id, 2) != dplyr::lag(player_with_next_touch, 1)
                           ~ 1L,
                           TRUE ~ 0L)
    ) %>% 
    mutate(# I add here the special type of passes
    cross_pass = as.integer(!(is.na(cross))) * 1,
    throughball = as.integer(!(is.na(throughball))) * 1,
    key_pass = as.integer(!(is.na(keypass))) * 1,
    bigchancecreated = as.integer(!(is.na(bigchancecreated))) * 1,
    intentionalassist = as.integer(!(is.na(intentionalassist))) * 1,
    intentionalgoalassist = as.integer(!(is.na(intentionalgoalassist))) * 1,
    shotassist = as.integer(!(is.na(shotassist))) * 1,
    attack = (passendx >= 81) * 1
    ) %>% 
    # COUNTING different VARIABLES 
    mutate(pass_neargoal =  1 * ((bigchancecreated + intentionalassist + intentionalgoalassist + key_pass + shotassist) > 0),
           pass_risky = 1 * ((throughball + cross_pass) > 0)
    )
  
  ###################################################
  ## Forward moving pass sequences (2023.02.08.),
  ## If a pass sequence in the end moves the ball closer to the goal
  ###################################################
  events <- events %>%
      mutate(pass_forward = 1 * (passendx >= x),
             ABA_pass_forward = 1 * (passendx >= x) * 
                 (passendx >= dplyr::lag(x, n = 1)) *
                 (ABA_pass == 1),
             ABAB_pass_forward = 1 * (passendx >= x) * 
                 (passendx >= dplyr::lag(x, n = 1)) * 
                 (passendx >= dplyr::lag(x, n = 2)) * 
                 (ABAB_pass == 1),
             ABABA_pass_forward = 1 * (passendx >= x) * 
                 (passendx >= dplyr::lag(x, n = 1)) * 
                 (passendx >= dplyr::lag(x, n = 2)) * 
                 (passendx >= dplyr::lag(x, n = 3)) * 
                 (ABABA_pass == 1),
             ABABAB_pass_forward = 1 * (passendx >= x) * 
                 (passendx >= dplyr::lag(x, n = 1)) * 
                 (passendx >= dplyr::lag(x, n = 2)) * 
                 (passendx >= dplyr::lag(x, n = 3)) * 
                 (passendx >= dplyr::lag(x, n = 4)) * 
                 (ABABAB_pass == 1),
             ABABABA_pass_forward = 1 * (passendx >= x) * 
                 (passendx >= dplyr::lag(x, n = 1)) * 
                 (passendx >= dplyr::lag(x, n = 2)) * 
                 (passendx >= dplyr::lag(x, n = 3)) * 
                 (passendx >= dplyr::lag(x, n = 4)) * 
                 (passendx >= dplyr::lag(x, n = 5)) * 
                 (ABABABA_pass == 1),
             ABABABAB_pass_forward = 1 * (passendx >= x) * 
                 (passendx >= dplyr::lag(x, n = 1)) * 
                 (passendx >= dplyr::lag(x, n = 2)) * 
                 (passendx >= dplyr::lag(x, n = 3)) * 
                 (passendx >= dplyr::lag(x, n = 4)) * 
                 (passendx >= dplyr::lag(x, n = 5)) * 
                 (passendx >= dplyr::lag(x, n = 6)) * 
                 (ABABABAB_pass == 1),
             ABABABABA_pass_forward = 1 * (passendx >= x) * 
                 (passendx >= dplyr::lag(x, n = 1)) * 
                 (passendx >= dplyr::lag(x, n = 2)) * 
                 (passendx >= dplyr::lag(x, n = 3)) * 
                 (passendx >= dplyr::lag(x, n = 4)) * 
                 (passendx >= dplyr::lag(x, n = 5)) * 
                 (passendx >= dplyr::lag(x, n = 6)) * 
                 (passendx >= dplyr::lag(x, n = 7)) * 
                 (ABABABABA_pass == 1),
             ABCA_pass_forward = 1 * (passendx >= x) * 
                 (passendx >= dplyr::lag(x, n = 1)) * 
                 (passendx >= dplyr::lag(x, n = 2)) * 
                 (ABCA_pass == 1),
             ABCDA_pass_forward = 1 * (passendx >= x) * 
                 (passendx >= dplyr::lag(x, n = 1)) * 
                 (passendx >= dplyr::lag(x, n = 2)) * 
                 (passendx >= dplyr::lag(x, n = 3)) * 
                 (ABCDA_pass == 1)
             ) %>%
      # missings are 0-s with lag not existing
      mutate(ABA_pass_forward = if_else(is.na(ABA_pass_forward), 0, ABA_pass_forward),
             ABAB_pass_forward = if_else(is.na(ABAB_pass_forward), 0, ABAB_pass_forward),
             ABABA_pass_forward = if_else(is.na(ABABA_pass_forward), 0, ABABA_pass_forward),
             ABABAB_pass_forward = if_else(is.na(ABABAB_pass_forward), 0, ABABAB_pass_forward),
             ABABABA_pass_forward = if_else(is.na(ABABABA_pass_forward), 0, ABABABA_pass_forward),
             ABABABAB_pass_forward = if_else(is.na(ABABABAB_pass_forward), 0, ABABABAB_pass_forward),
             ABABABABA_pass_forward = if_else(is.na(ABABABABA_pass_forward), 0, ABABABABA_pass_forward),
             
             ABCA_pass_forward = if_else(is.na(ABCA_pass_forward), 0, ABCA_pass_forward),
             ABCDA_pass_forward = if_else(is.na(ABCDA_pass_forward), 0, ABCDA_pass_forward)
             )
  
  events <- events %>%
    select(wh_player_id, player_with_next_touch, 
           ABA_pass, ABA_pass_forward, ABAB_pass, ABAB_pass_forward, 
           ABABA_pass, ABABA_pass_forward, ABABAB_pass, ABABAB_pass_forward,
           ABABABA_pass, ABABABA_pass_forward, 
           ABCA_pass, ABCA_pass_forward, ABCDA_pass, ABCDA_pass_forward,
           cross_pass, key_pass, attack, pass_neargoal, pass_risky,
           everything())

  
  # Second: aggregate
  events_agg <- events %>% 
    mutate(pass1 = 1) %>% 
    group_by(wh_match_id, teamid, wh_player_id, player_with_next_touch ) %>%
    summarise(pass_count = sum(pass1, na.rm = TRUE),
              pass_forward_count = sum(1 * (passendx >= x), na.rm = TRUE),
              
              ABA_pass = sum(ABA_pass, na.rm = TRUE), 
              ABAB_pass = sum(ABAB_pass, na.rm = TRUE), 
              ABABA_pass = sum(ABABA_pass, na.rm = TRUE),
              ABABAB_pass = sum(ABABAB_pass, na.rm = TRUE),
              ABABABA_pass = sum(ABABABA_pass, na.rm = TRUE),
              ABABABAB_pass = sum(ABABABAB_pass, na.rm = TRUE),
              ABABABABA_pass = sum(ABABABABA_pass, na.rm = TRUE),
              
              ABA_pass_forward = sum(ABA_pass_forward, na.rm = TRUE), 
              ABAB_pass_forward = sum(ABAB_pass_forward, na.rm = TRUE), 
              ABABA_pass_forward = sum(ABABA_pass_forward, na.rm = TRUE),
              ABABAB_pass_forward = sum(ABABAB_pass_forward, na.rm = TRUE),
              ABABABA_pass_forward = sum(ABABABA_pass_forward, na.rm = TRUE),
              ABABABAB_pass_forward = sum(ABABABAB_pass_forward, na.rm = TRUE),
              ABABABABA_pass_forward = sum(ABABABABA_pass_forward, na.rm = TRUE),              
              
              
              # New Special passes added
              cross_pass = sum(cross_pass, na.rm = TRUE),
              key_pass = sum(key_pass, na.rm = TRUE),
              attack = sum(attack, na.rm = TRUE),
              pass_neargoal = sum(pass_neargoal, na.rm = TRUE),
              pass_risky = sum(pass_risky, na.rm = TRUE),
              
              # added 15/08/2021
              forwardness = mean(forwardness > 0, na.rm = TRUE), # fraction of forward passes
              avg_pass_length = mean(length, na.rm = TRUE)
              
              # Not need for 2players
              # , ABCA_pass = sum(ABCA_pass), 
              # ABCDA_pass = sum(ABCDA_pass)
              # 
    ) %>%
    filter(wh_player_id != player_with_next_touch) %>%
    rename(wh_player_id1 = wh_player_id) %>%
    rename(wh_player_id2 = player_with_next_touch) %>%
    arrange(wh_match_id,wh_player_id1,wh_player_id1) 
  
  events_agg <- events_agg %>% 
    group_by(wh_match_id, teamid, 
             ID1n = wh_player_id1, 
             ID2n = wh_player_id2) %>% 
    summarise(pass_count = sum(pass_count),
              pass_forward_count = sum(pass_forward_count, na.rm = TRUE),
              # new passes
              ABA_pass = sum(ABA_pass), 
              ABAB_pass = sum(ABAB_pass), 
              ABABA_pass = sum(ABABA_pass),
              ABABAB_pass = sum(ABABAB_pass),
              ABABABA_pass = sum(ABABABA_pass),
              ABABABAB_pass = sum(ABABABAB_pass),
              ABABABABA_pass = sum(ABABABABA_pass),
              
              ABA_pass_forward = sum(ABA_pass_forward), 
              ABAB_pass_forward = sum(ABAB_pass_forward), 
              ABABA_pass_forward = sum(ABABA_pass_forward),
              ABABAB_pass_forward = sum(ABABAB_pass_forward),
              ABABABA_pass_forward = sum(ABABABA_pass_forward),
              ABABABAB_pass_forward = sum(ABABABAB_pass_forward),
              ABABABABA_pass_forward = sum(ABABABABA_pass_forward),              
              
              cross_pass = sum(cross_pass),
              key_pass = sum(key_pass),
              attack = sum(attack),
              pass_neargoal = sum(pass_neargoal),
              pass_risky = sum(pass_risky),
              forwardness = mean(forwardness, na.rm = TRUE), # fraction of forward passes
              avg_pass_length = mean(avg_pass_length, na.rm = TRUE)
              
    ) %>%
    ungroup() %>% 
    # correct for double counting of passes
    mutate(net_ABABABABA_pass = ABABABABA_pass,
           net_ABABABAB_pass = ABABABAB_pass - 2*net_ABABABABA_pass,
           net_ABABABA_pass = ABABABA_pass - 2*net_ABABABAB_pass - 3*net_ABABABABA_pass,
           net_ABABAB_pass = ABABAB_pass - 2 * net_ABABABA_pass - 3*net_ABABABAB_pass - 4*net_ABABABABA_pass,
           net_ABABA_pass = ABABA_pass - 2 * net_ABABAB_pass - 3 * net_ABABABA_pass - 4*net_ABABABAB_pass - 5*net_ABABABABA_pass,
           net_ABAB_pass = ABAB_pass - 2 * net_ABABA_pass - 3 * net_ABABAB_pass - 4 * net_ABABABA_pass - 5*net_ABABABAB_pass - 6*net_ABABABABA_pass,
           net_ABA_pass = ABA_pass  - 2 * net_ABAB_pass  - 3 * net_ABABA_pass - 4 * net_ABABAB_pass - 5 * net_ABABABA_pass - 6*net_ABABABAB_pass - 7*net_ABABABABA_pass,
           net_AB_pass = pass_count - 2 * net_ABA_pass  - 3 * net_ABAB_pass  - 4 * net_ABABA_pass - 5 * net_ABABAB_pass - 6 * net_ABABABA_pass - 7*net_ABABABAB_pass - 8*net_ABABABABA_pass,
           
           net_ABABABABA_pass_forward = ABABABABA_pass_forward,
           net_ABABABAB_pass_forward = ABABABAB_pass_forward - 2*net_ABABABABA_pass_forward,
           net_ABABABA_pass_forward = ABABABA_pass_forward - 2*net_ABABABAB_pass_forward - 3*net_ABABABABA_pass_forward,
           net_ABABAB_pass_forward = ABABAB_pass_forward - 2 * net_ABABABA_pass_forward - 3*net_ABABABAB_pass_forward - 4*net_ABABABABA_pass_forward,
           net_ABABA_pass_forward = ABABA_pass_forward - 2 * net_ABABAB_pass_forward - 3 * net_ABABABA_pass_forward - 4*net_ABABABAB_pass_forward - 5*net_ABABABABA_pass_forward,
           net_ABAB_pass_forward = ABAB_pass_forward - 2 * net_ABABA_pass_forward - 3 * net_ABABAB_pass_forward - 4 * net_ABABABA_pass_forward - 5*net_ABABABAB_pass_forward - 6*net_ABABABABA_pass_forward,
           net_ABA_pass_forward = ABA_pass_forward  - 2 * net_ABAB_pass_forward  - 3 * net_ABABA_pass_forward - 4 * net_ABABAB_pass_forward - 5 * net_ABABABA_pass_forward - 6*net_ABABABAB_pass_forward - 7*net_ABABABABA_pass_forward,
           net_AB_pass_forward = pass_forward_count - 2 * net_ABA_pass_forward  - 3 * net_ABAB_pass_forward  - 4 * net_ABABA_pass_forward - 5 * net_ABABAB_pass_forward - 6 * net_ABABABA_pass_forward - 7*net_ABABABAB_pass_forward - 8*net_ABABABABA_pass_forward
    ) %>%
    rename(wh_player_id1 = ID1n, wh_player_id2 = ID2n) %>%
    ungroup() %>%
    arrange(wh_match_id, teamid, wh_player_id1, wh_player_id2)  %>% 
    rename(total_pass_count = pass_count,
           total_pass_forward_count = pass_forward_count
           )
  
  # put into long format
  events_agg1 <- events_agg %>% 
    select(wh_match_id, teamid, wh_player_id1, wh_player_id2, total_pass_count, total_pass_forward_count,
           cross_pass, key_pass, attack, pass_neargoal, pass_risky, forwardness, avg_pass_length, 
           net_AB_pass, net_ABA_pass, net_ABAB_pass, net_ABABA_pass,
           net_ABABAB_pass, net_ABABABA_pass, net_ABABABAB_pass, net_ABABABABA_pass
           ) %>% 
    pivot_longer(cols = net_AB_pass:net_ABABABABA_pass,
                 names_to = "pass_sequence", values_to = "pass_sequence_count") %>% 
    mutate(pass_sequence = case_when(pass_sequence == "net_AB_pass" ~ 1,
                                     pass_sequence == "net_ABA_pass" ~ 2,
                                     pass_sequence == "net_ABAB_pass" ~ 3,
                                     pass_sequence == "net_ABABA_pass" ~ 4,
                                     pass_sequence == "net_ABABAB_pass" ~ 5,
                                     pass_sequence == "net_ABABABA_pass" ~ 6,
                                     pass_sequence == "net_ABABABAB_pass" ~ 7,
                                     pass_sequence == "net_ABABABABA_pass" ~ 8
                                     )
           )
  
  
  events_agg2 = events_agg %>% 
      select(wh_match_id, teamid, wh_player_id1, wh_player_id2, 
             net_AB_pass_forward, net_ABA_pass_forward, net_ABAB_pass_forward, net_ABABA_pass_forward,
             net_ABABAB_pass_forward, net_ABABABA_pass_forward, net_ABABABAB_pass_forward, net_ABABABABA_pass_forward
             
      ) %>% 
      pivot_longer(cols = net_AB_pass_forward:net_ABABABABA_pass_forward,
                   names_to = "pass_sequence", values_to = "pass_sequence_count") %>% 
      mutate(pass_sequence = case_when(pass_sequence == "net_AB_pass_forward" ~ 1,
                                               pass_sequence == "net_ABA_pass_forward" ~ 2,
                                               pass_sequence == "net_ABAB_pass_forward" ~ 3,
                                               pass_sequence == "net_ABABA_pass_forward" ~ 4,
                                               pass_sequence == "net_ABABAB_pass_forward" ~ 5,
                                               pass_sequence == "net_ABABABA_pass_forward" ~ 6,
                                               pass_sequence == "net_ABABABAB_pass_forward" ~ 7,
                                               pass_sequence == "net_ABABABABA_pass_forward" ~ 8
                                               )
             ) %>% 
      rename(pass_sequence_count_forward = pass_sequence_count
             )
  
  events_agg = events_agg1 %>% 
      left_join(.,
                events_agg2,
                by = c("wh_match_id" = "wh_match_id",
                       "teamid" = "teamid",
                       "wh_player_id1" = "wh_player_id1",
                       "wh_player_id2" = "wh_player_id2",
                       "pass_sequence" = "pass_sequence"
                       )
                )
  rm(events_agg1, events_agg2)

  ### corr: number of pass sequences are not allowed (few observations related)
  events_agg <- events_agg %>% 
    mutate(pass_sequence_count = if_else(pass_sequence_count<0, 0, pass_sequence_count),
           pass_sequence_count_forward = if_else(pass_sequence_count_forward<0, 0, pass_sequence_count_forward)
           )
  
  #### Put it into wide format
  events_agg %<>%
    pivot_wider(id_cols = wh_match_id:avg_pass_length,
                names_from = pass_sequence,
                values_from = c("pass_sequence_count", "pass_sequence_count_forward"), 
                names_sep = ""
    )
  
  # Add features
  events_agg <- events_agg %>% 
    mutate(fn=listcsv1[i]) %>%
    separate(fn, c("country", "league_name", "season_name"), sep = "_")
  
  write_parquet(events_agg, (file.path(data_tidy_temp, listagg[i])))
}




# decrease memory requirements
rm(events)
rm(events_agg)

###################################################
# create aggregates
###################################################

# player-minutes
pattern3 = c('player-minutes*', 
             'player-minutes*') # just a placeholder

files1 <- dir(data_tidy_temp, pattern=paste0(pattern3, collapse="|")) # get file names

files1
player_minutes_all <- files1 %>%
  map_dfr(~ read_parquet(file.path(data_tidy_temp, .))) 

write_parquet(player_minutes_all, paste0(data_tidy_temp,"all_player_minutes.parquet"))
rm(player_minutes_all)

############################################################
# merge all of them
# Major leagues
############################################################


# aggregate to event level
data_path <- "data_tidy_temp"   # path to the data


pattern4 = c('events-agg-*',
             'events-agg-*')
files1 <- dir(data_tidy_temp, pattern=paste0(pattern4, collapse="|")) # get file names
files1

events_agg_major_leagues <- files1 %>%
  map_dfr(~ read_parquet(file.path(data_tidy_temp, .)))



events_agg_major_leagues <- distinct(events_agg_major_leagues)

write_parquet(events_agg_major_leagues, paste0(data_tidy_temp,"all_events_agg_major_leagues.parquet"))

# drop datasets
rm(events_agg_major_leagues)
rm(player_minutes)
rm(players)
rm(player_info)

