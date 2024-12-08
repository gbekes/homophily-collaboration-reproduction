
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#
# d01_03_pairwise_pass_count_shared_minutes.R

# Creates pass counts
# Raw data used. 

# This version: v2.1: 2024-11-18
#######################################################


##############################
# This table contains matches and lineups per substitutions for the home and away teams. Using this we partition each match and create player couples who shared this given period on the pitch.
##############################


tictoc::tic()

formations = read_parquet(formation_use.parquet)
formations = formations %>% 
    select(wh_match_id, side, start_minute, end_minute, 
           starts_with("spot")) %>% 
    pivot_longer(cols = starts_with("spot"),
                 names_to = "position",
                 values_to = "wh_player_id")

skeleton = formations %>% 
    left_join(.,
              formations,
              by = c("wh_match_id" = "wh_match_id",
                     "side" = "side",
                     "start_minute" = "start_minute",
                     "end_minute" = "end_minute"
                     )
              ) %>% 
  filter(wh_player_id.x != wh_player_id.y)

skeleton = skeleton %>% 
  select(-starts_with("position"))


rm(formations)
skeleton = skeleton %>% 
  mutate(wh_match_id = as.integer(wh_match_id)
         ) %>% 
  rename(ID1n = wh_player_id.x,
         ID2n = wh_player_id.y)
# CHANGED: now we keep both ways of the relationship, because we are going to join on both later
skeleton = skeleton %>% 
    summarise(on_court_together = 1, 
               .by = c(wh_match_id, side, 
                       start_minute, end_minute, 
                       ID1n, ID2n)
               )

# this a large file, takes time
write_parquet(skeleton, 
              paste0(data_tidy_temp, "players_match_shared_minutes.parquet"))


rm(skeleton)



##############################
# We count all the passes for the relevant match partitions for each players.
##############################

formations = read_parquet(formation_use.parquet)
formations = formations %>% 
    select(wh_match_id, side, start_minute, end_minute, 
           starts_with("spot")) %>% 
    pivot_longer(cols = starts_with("spot"),
                 names_to = "position",
                 values_to = "wh_player_id") %>% 
  select(wh_match_id, wh_player_id, start_minute, end_minute)



############### 
# use events to count the passes by time windows.
############### 

pattern_all = c('*', 
             '*') # just a placeholder

listpq <- list()
listpq <- dir(data_events, pattern=paste0(pattern_all, collapse="|")) # get file names
listpq2 <- listpq
listagg <- gsub("^", "events-agg_match_periods_", listpq2) # Now keeps .parquet extension
listcsv1 <- gsub(".parquet", "", listpq2)  # Just for season info extraction
listpq

for (i in 1:length(listpq)){
  filename = paste0("events")
  
  # slight change to read brazil, but I save without the special character at the end
  filename_i = listpq[i]
  # which season we iterate on
  assign(filename, read_parquet(file = paste0(data_events, filename_i) ,
                                as_data_frame = TRUE))
  
  
  a=nrow(events)
  print(listpq[i])
  
  events = events %>% 
    filter(type=="Pass",
           outcometype=="Successful",
           # clearing, 2021.04.08.
           !is.na(wh_player_id), # not missings!
           !is.na(player_with_next_touch), # not missings!
           wh_player_id != 0,
           player_with_next_touch != 0,
           wh_player_id != player_with_next_touch
           
    ) %>% 
    select(wh_match_id, minute, wh_player_id
    ) %>% mutate(pass1 = 1)
  
  events_agg = events %>% 
    left_join(formations,
              by = c("wh_match_id" = "wh_match_id",
                     "wh_player_id" = "wh_player_id"
                     )
              ) %>% 
    filter(minute >= start_minute,
           minute < end_minute) %>% 
    group_by(wh_match_id, wh_player_id, start_minute, end_minute) %>% 
    summarise(pass_count_for_window = sum(pass1))
  
  # add features
  events_agg = events_agg %>% 
    mutate(fn=listcsv1[i]) %>%
    separate(fn, c("country", "league_name", "season_name"), sep = "_")
  
  # Hmisc::describe(events_agg$teamid)

  write_parquet(events_agg, (file.path(data_tidy_temp, listagg[i])))
}

# decrease memory requirements
rm(events)
rm(events_agg)


###################################################
# create aggregates
# merge all of them: CHANGE, separate major/other leagues
############################################################

# aggregate to event level
data_path <- "data_tidy_temp"   # path to the data
data_path
#########################
# Major leagues
#########################

pattern4 = c('events-agg_match_periods-*',
             'events-agg_match_periods-*')
files1 <- dir(data_tidy_temp, pattern=paste0(pattern4, collapse="|")) # get file names
files1

source(paste0("wrangling/01_anon-maker/","league-filter.R"))

events_agg_major_leagues <- files1 %>%
  map_dfr(~ read_parquet(file.path(data_tidy_temp, .)))


rm(formations)


write_parquet(events_agg_major_leagues, 
              paste0(data_tidy_temp, "events_agg_match_periods_major_leagues.parquet"))


######################################
# Final step: join back the player level pass counts onto the table storing shared time on the pitch. As only those couples are included who shared those minutes together, summing up the passes after the join will give use the sum of passes for the period they shared on the court. 
######################################

skeleton = read_parquet(paste0(data_tidy_temp, "players_match_shared_minutes.parquet"))
events_agg_major_leagues = read_parquet(paste0(data_tidy_temp, "events_agg_match_periods_major_leagues.parquet"))



events_agg_major_leagues <- events_agg_major_leagues %>%
  mutate(wh_match_id = as.integer(wh_match_id))  # Convert to integer to match skeleton

# Then do the join
skeleton = skeleton %>% 
  inner_join(.,
             events_agg_major_leagues %>% select(-league_name, -season_name, -country),
             by = c("wh_match_id",
                    "ID1n" = "wh_player_id",
                    "start_minute",
                    "end_minute")
  ) %>% 
  rename(pass_count_for_window1 = pass_count_for_window) %>% 
  inner_join(.,
             events_agg_major_leagues %>% select(-league_name, -season_name, -country),
             by = c("wh_match_id",
                    "ID2n" = "wh_player_id",
                    "start_minute",
                    "end_minute")
  ) %>% 
  rename(pass_count_for_window2 = pass_count_for_window)

skeleton[is.na(skeleton)] = 0 # where no passes occured

nrow(skeleton)
rm(events_agg_major_leagues)

######################################
# Aggregate and save the table.
######################################

skeleton = skeleton %>% 
  filter(pass_count_for_window1 + pass_count_for_window2 != 0) %>% 
  # group_by(wh_match_id, ID1n, ID2n) %>% 
  summarise(passes_in_shared_mins1 = sum(pass_count_for_window1),
            passes_in_shared_mins2 = sum(pass_count_for_window2),
            .by = c(wh_match_id, ID1n, ID2n)
            )

skeleton %<>%
  rename(wh_player_id1=ID1n, wh_player_id2=ID2n)

write_parquet(skeleton, 
              paste0(data_tidy_temp, "player_passcounts_shared_minutes_major_leagues.parquet"))

# Hmisc::describe(skeleton$passes_in_shared_mins1)

tictoc::toc()

rm(skeleton)