
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#
# d01_05_culturegroups_part2.R

# Create groups of cultures for each player
# Raw data used. 

# part 2

# NB: runs for 2hs (laptop)

# This version: v2.1: 2024-11-18
#######################################################


# match data
matches_info = read_csv(paste0(data_tidy,"matches_info.csv"))
matches_info %<>%
  select(wh_match_id, season, season_half)

##############################################
## Read on-field data, attach other info to filter out bad passes, get match ID-s 
##############################################

player_cultures_both = read_parquet(paste0(data_tidy_temp,"player_cultures_both.parquet"))

# create temp for output
output <- paste0(data_tidy_temp,"output/")
create_output_if_doesnt_exist(output)

# aggregate set up
all_culture_season_half = data.frame()

tictoc::tic()



pattern_all = c('*', 
                '*') # just a placeholder
listpq <-list()
listpq <- dir(data_events, pattern=paste0(pattern_all, collapse="|")) # get file names
listpq

for (i in 1:length(listpq)){

  
  print(listpq[i]        )
  listpq[i]
  onfield_filename_i = paste0("onfield-", listpq[i])
  filename_i = listpq[i]
  
  
  filename<-paste0("events")

  # which season we iterate on
  assign(filename, read_parquet(file = paste0(data_events, filename_i) ,
                                as_data_frame = TRUE))
  
  ### GET INFO ABOUT PASS
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
    select(unique_id, wh_match_id, teamid, wh_player_id, player_with_next_touch)
  
  
  ## GET THE ON-FIELD DATA
  assign("onfield_dat", 
         read_parquet(file = paste0(data_raw, "onfield_data/", onfield_filename_i)))
  
  ## keep only valid passes
  onfield_dat = onfield_dat %>% 
    inner_join(events,
               by = "unique_id"
    )
  
  onfield_dat = onfield_dat %>% 
    rename(wh_player_id1 = wh_player_id,
           wh_player_id2 = player_with_next_touch)
  
  # culture of passer
  onfield_dat = onfield_dat %>% 
    pivot_longer(cols = starts_with("spot"), names_prefix = "spot_",
                 names_to = "spot_number", values_to = "spot_player_id")
  
  ## attach cultures of all spots
  onfield_dat = onfield_dat %>%
    left_join(player_cultures_both,
              by = c("spot_player_id" = "wh_player_id")
    )
  
  ## missing culture
  onfield_dat = onfield_dat %>% 
    mutate(culture_all2 = case_when(culture_all1 != culture_all2 ~ culture_all2), # missing if equal
           culture_all3 = case_when(culture_all1 != culture_all3 ~ culture_all3), # missing if equal
           culture_all3 = case_when(culture_all2 != culture_all3 ~ culture_all3), # missing if equal
           
           culture_all4 = case_when(culture_all1 != culture_all4 ~ culture_all4), # missing if equal
           culture_all4 = case_when(culture_all2 != culture_all4 ~ culture_all4), # missing if equal
           culture_all4 = case_when(culture_all3 != culture_all4 ~ culture_all4), # missing if equal
           
           culture_all1 = if_else(is.na(culture_all1), paste0("missing", unique_id, spot_player_id, "_culturerank1"), culture_all1),
           culture_all2 = if_else(is.na(culture_all2), paste0("missing", unique_id, spot_player_id, "_culturerank2"), culture_all2),
           culture_all3 = if_else(is.na(culture_all3), paste0("missing", unique_id, spot_player_id, "_culturerank3"), culture_all3),
           culture_all4 = if_else(is.na(culture_all4), paste0("missing", unique_id, spot_player_id, "_culturerank4"), culture_all4)
    )
  
  ## rotate cultures to be long
  onfield_dat = onfield_dat %>% 
    pivot_longer(., cols = c("culture_all1", "culture_all2", "culture_all3", "culture_all4"), 
                 names_to = "culture_all_rank" , names_prefix = "culture_all",
                 values_to = "culture_all"
    )
  
  ## count cultures for each pass
  onfield_dat = onfield_dat %>% 
    group_by(unique_id, culture_all) %>% 
    mutate(culture_all_n = n()
    ) %>% 
    ungroup()
  
  ## rotate back
  onfield_dat = onfield_dat %>% 
    pivot_wider(., id_cols = unique_id:culture_counter,
                names_from = c("culture_all_rank"),
                values_from = c("culture_all", "culture_all_n"),
    ) 
  
  onfield_dat = onfield_dat %>%
    rename(culture_all1_n = culture_all_n_1,
           culture_all2_n = culture_all_n_2,
           culture_all3_n = culture_all_n_3,
           culture_all4_n = culture_all_n_4,
           
           culture_all1 = culture_all_1,
           culture_all2 = culture_all_2,
           culture_all3 = culture_all_3,
           culture_all4 = culture_all_4
    )
  
  ## we counted how many players there are for everyone on the field
  
  ## take care of missing
  onfield_dat = onfield_dat %>% 
    mutate(culture_all1_n = case_when(!str_detect(culture_all1, "missing") ~ culture_all1_n),
           culture_all2_n = case_when(!str_detect(culture_all2, "missing") ~ culture_all2_n),
           culture_all3_n = case_when(!str_detect(culture_all3, "missing") ~ culture_all3_n),
           culture_all4_n = case_when(!str_detect(culture_all4, "missing") ~ culture_all4_n)
    )
  
  ## we keep only the passers
  onfield_dat_passer = onfield_dat %>% 
    filter(wh_player_id1 == spot_player_id) %>% 
    select(-spot_player_id) %>% 
    rename(spot_number_p1 = spot_number,
           citizenship_v0_p1 = citizenship_v0,
           citizenship_v1_p1 = citizenship_v1,
           culture_counter_p1 = culture_counter,
           culture_all1_p1 = culture_all1,
           culture_all2_p1 = culture_all2,
           culture_all3_p1 = culture_all3,
           culture_all4_p1 = culture_all4,
           
           culture_all1_n_p1 = culture_all1_n,
           culture_all2_n_p1 = culture_all2_n,
           culture_all3_n_p1 = culture_all3_n,
           culture_all4_n_p1 = culture_all4_n
    )
  
  onfield_dat_target = onfield_dat %>% 
    filter(wh_player_id2 == spot_player_id) %>% 
    select(-spot_player_id) %>% 
    rename(spot_number_p2 = spot_number,
           citizenship_v0_p2 = citizenship_v0,
           citizenship_v1_p2 = citizenship_v1,
           culture_counter_p2 = culture_counter,
           culture_all1_p2 = culture_all1,
           culture_all2_p2 = culture_all2,
           culture_all3_p2 = culture_all3,
           culture_all4_p2 = culture_all4,
           
           culture_all1_n_p2 = culture_all1_n,
           culture_all2_n_p2 = culture_all2_n,
           culture_all3_n_p2 = culture_all3_n,
           culture_all4_n_p2 = culture_all4_n
    )
  
  
  pass_cultures_counts = onfield_dat_passer %>% 
    inner_join(onfield_dat_target,
               by = c("unique_id" = "unique_id",
                      "wh_match_id" = "wh_match_id",
                      "teamid" = "teamid",
                      "wh_player_id1" = "wh_player_id1",
                      "wh_player_id2" = "wh_player_id2"
               )
    )
  
  
  rm(onfield_dat, onfield_dat_passer, onfield_dat_target)
  
  # create what we need
  pass_cultures_counts %<>%
    rowwise %>%
    mutate(
      culture_all_n_p1=max(culture_all1_n_p1,culture_all2_n_p1, culture_all3_n_p1, culture_all4_n_p1, na.rm = TRUE),
      culture_all_n_p2=max(culture_all1_n_p2,culture_all2_n_p2, culture_all3_n_p2, culture_all4_n_p2, na.rm = TRUE)
    )%>%
    ungroup()
  tabyl(pass_cultures_counts$culture_all_n_p2,show_na = TRUE, show_missing_levels = TRUE)
  
  # must correct a few hundred missing values
  pass_cultures_counts %<>%
    mutate(
      culture_all_n_p1=ifelse(is.infinite(culture_all_n_p1),2.999,culture_all_n_p1 ),
      culture_all_n_p2=ifelse(is.infinite(culture_all_n_p2),2.999,culture_all_n_p2 )
    )
  
  tabyl(pass_cultures_counts$culture_all_n_p2)
  
  
  # just to be able to compare later
  pass_cultures_counts  %<>% 
    mutate(same_culture_new=ifelse(  culture_all1_p1==culture_all1_p2 | 
                                       culture_all1_p1==culture_all2_p2 | 
                                       culture_all1_p1==culture_all3_p2 | 
                                       culture_all1_p1==culture_all4_p2 | 
                                       
                                       culture_all2_p1==culture_all1_p2 | 
                                       culture_all2_p1==culture_all2_p2 |
                                       culture_all2_p1==culture_all3_p2 |
                                       culture_all2_p1==culture_all4_p2 |
                                       
                                       culture_all3_p1==culture_all1_p2 | 
                                       culture_all3_p1==culture_all2_p2 |
                                       culture_all3_p1==culture_all3_p2 |
                                       culture_all3_p1==culture_all4_p2 |                                           
                                       
                                       culture_all4_p1==culture_all1_p2 | 
                                       culture_all4_p1==culture_all2_p2 |
                                       culture_all4_p1==culture_all3_p2 |
                                       culture_all4_p1==culture_all4_p2 ,1,0)
    )
  tabyl(pass_cultures_counts$same_culture_new)  
  
  
  # create shares
  cut2=2
  pass_cultures_counts  %<>% 
    mutate(
      culture_inoutgroup_g0g0_cut2=ifelse(culture_all_n_p1<cut2 & culture_all_n_p2<cut2,1,0),
      culture_inoutgroup_g0g1_cut2=ifelse(culture_all_n_p1<cut2 & culture_all_n_p2>=cut2,1,0),
      culture_inoutgroup_g1g0_cut2=ifelse(culture_all_n_p1>=cut2 & culture_all_n_p2<cut2,1,0),
      culture_inoutgroup_g1g1_cut2=ifelse(culture_all_n_p1>=cut2 & culture_all_n_p2>=cut2,1,0)
    )
  
  cut3=3
  pass_cultures_counts  %<>% 
    mutate(
      culture_inoutgroup_g0g0_cut3=ifelse(culture_all_n_p1<cut3 & culture_all_n_p2<cut3,1,0),
      culture_inoutgroup_g0g1_cut3=ifelse(culture_all_n_p1<cut3 & culture_all_n_p2>=cut3,1,0),
      culture_inoutgroup_g1g0_cut3=ifelse(culture_all_n_p1>=cut3 & culture_all_n_p2<cut3,1,0),
      culture_inoutgroup_g1g1_cut3=ifelse(culture_all_n_p1>=cut3 & culture_all_n_p2>=cut3,1,0)
    )
  
  cut4=4
  pass_cultures_counts  %<>% 
    mutate(
      culture_inoutgroup_g0g0_cut4=ifelse(culture_all_n_p1<cut4 & culture_all_n_p2<cut4,1,0),
      culture_inoutgroup_g0g1_cut4=ifelse(culture_all_n_p1<cut4 & culture_all_n_p2>=cut4,1,0),
      culture_inoutgroup_g1g0_cut4=ifelse(culture_all_n_p1>=cut4 & culture_all_n_p2<cut4,1,0),
      culture_inoutgroup_g1g1_cut4=ifelse(culture_all_n_p1>=cut4 & culture_all_n_p2>=cut4,1,0)
    )
  
  
  # drop what we don't need
  pass_cultures_counts  %<>% 
    select(-citizenship_v0_p1, -citizenship_v1_p1, -citizenship_v0_p2, -citizenship_v1_p2,
           -spot_number_p1,-spot_number_p2, 
           -culture_all1_n_p1,-culture_all2_n_p1,-culture_all3_n_p1,-culture_all4_n_p1,         
           -culture_all1_n_p2,-culture_all2_n_p2,-culture_all3_n_p2,-culture_all4_n_p2, 
           -culture_all1_p1, -culture_all1_p2,          
           -culture_all2_p1, -culture_all2_p2,
           -culture_all3_p1, -culture_all3_p2,
           -culture_all4_p1, -culture_all4_p2,
           
           -culture_counter_p1, - culture_counter_p2)
  

    # sanity check
  # is it above 11 for diff culture cases -- yes, a few times
  pass_cultures_counts  %<>% 
    rowwise() %>%
    mutate(tt=(1-same_culture_new)*(culture_all_n_p1+culture_all_n_p2)) %>%
    ungroup()
  tabyl(pass_cultures_counts$tt)
  
  
#######################    
# half-season aggregate
#######################    
  
  # join back match id and date
  pass_cultures_counts %<>%
    mutate(wh_match_id=as.numeric(wh_match_id))
  
  pass_cultures_counts <- pass_cultures_counts %>% 
    mutate(league_season= str_sub(filename_i, 1, -9))
  
  # temp solution: full season --> TODO by half-season
  pass_cultures_counts %<>%
    left_join(.,
              matches_info,
              by = c("wh_match_id")
    )
  
  
  pass_cultures_counts_half_season <- pass_cultures_counts %>%
    group_by(teamid, league_season, season_half, wh_player_id1, wh_player_id2) %>%
    summarise(
      culture_all_n_p1_mean=mean(culture_all_n_p1),
      culture_all_n_p2_mean=mean(culture_all_n_p2),
      culture_all_same_culture_new=mean(same_culture_new),
      across(starts_with(c("culture_inoutgroup" )), mean)
    ) %>%
    ungroup()
  
  # save
  write_parquet(pass_cultures_counts_half_season, (file.path(output, listpq[i])))

  beep(sound=1)
}


##########################################
# loop over and combine 
##########################################

for (i in 1:length(listpq)){
  filename_i = listpq[i]
  pass_cultures_counts_half_season = read_parquet(paste0(output, filename_i))
  
  all_culture_season_half = rbind(all_culture_season_half,
                                  pass_cultures_counts_half_season  )
}

write_parquet(all_culture_season_half, paste0(data_tidy_temp, "all_culture_season_half.parquet"))

beep(sound=5)

tictoc::toc()

# drop data
rm(all_culture_season_half, pass_cultures_counts_half_season, pass_cultures_counts, events, player_cultures_both, matches_info)
