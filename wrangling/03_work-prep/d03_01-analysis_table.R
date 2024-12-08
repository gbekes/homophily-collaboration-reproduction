
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d03_01-analysis_table.R

# creates new variables + sample design 

# This version: v2.1: 2024-11-18
#######################################################





#######################################################

# in: 
# anon_all_events_season-half_major_full.parquet
# anon_team_category.parquet

# out: 
# data_player_sum.parquet
# p1_p2_flipped_zero.csv, flags_p1.csv, flags_p2.csv
# anon_work-player-pair_sh_temp2.parquet
# 
#######################################################


############### 
# set data used
############### 
all_events_season_half= read_parquet(file = paste0(data_tidy_created, "anon_all_events_season-half_major_full.parquet"), as_data_frame = TRUE)
all_culture_season_half =read_parquet(file = paste0(data_tidy, "anon_all_culture_season_half.parquet"), as_data_frame = TRUE)

#define dataset
data=all_events_season_half
data %<>%
  left_join(.,
            all_culture_season_half,
            by = c("league_season", "season_half", "anon_teamid", "anon_player_id1", "anon_player_id2")
  )


rm(all_events_season_half)


# Average value regressions
# Season-half aggregated. Number of game may vary

data %<>%
    mutate(sh=as.factor(season_name_half))

data %<>%
    relocate(league_ctry, season_name, anon_teamid, season_name_half, season_length, games, anon_player_id1, anon_player_id2, pass_count)

data <- data %<>%  
    mutate(ln_pass_count=log(pass_count)
    )

data <- data %<>%  
    mutate(league_time=paste(league_season, season_half, sep="_h" ),
           time=sh,
           p1_position=specific_position_p1,
           p2_position=specific_position_p2,
           p1_citizenship=citizenship_v0_p1,
           p2_citizenship=citizenship_v0_p2)


##################################
data = data %>%
  group_by(season_name_half, league_ctry, anon_teamid, anon_player_id1) %>%
  mutate(total_pass_by_passer_sh=sum(pass_count)) %>%
    ungroup()
            
data = data %>%
  group_by(season_name_half, league_ctry, anon_teamid, anon_player_id2) %>%
  mutate(total_reception_by_receiver_sh=sum(pass_count))  %>%
    ungroup()

#####################################################
# creating player vars, do bit of sample design
#####################################################


data %<>%
    mutate(ln_sh_pass_player_id1=log(total_pass_by_passer_sh),
           ln_sh_pass_player_id2=log(total_reception_by_receiver_sh))

data %<>%
    mutate(ln_value_both=log(value_p1/2+value_p2/2)   )

# drop when only one player pair in season. rare. 
data%<>%
    group_by(anon_teamid, sh,anon_player_id1) %>%
    mutate(pair1=n_distinct(anon_player_id2)) %>%
    ungroup() %>%
    group_by(anon_teamid, sh,anon_player_id2) %>%
    mutate(pair2=n_distinct(anon_player_id1)) %>%
    ungroup()

tabyl(data$pair1)
tabyl(data$pair2)

data %<>%
    mutate(flag_error_pair=ifelse((pair1==1 | pair2==1) ,1,0)) 

tabyl(data$flag_error_pair)

data %<>%
    filter(flag_error_pair!=1)

##############################
# create passing variables
##############################

data %<>% 
    mutate(pass_count_complex=pass_sequence_count2+pass_sequence_count3+pass_sequence_count4+pass_sequence_count5+pass_sequence_count6+pass_sequence_count7+pass_sequence_count8) %>%
    mutate(pass_count_sequences=pass_sequence_count1+pass_sequence_count2+pass_sequence_count3+pass_sequence_count4+pass_sequence_count5+pass_sequence_count6+pass_sequence_count7+pass_sequence_count8) %>%
    mutate(ln_pass_count_complex=log(pass_count_complex))

# adding forward
data %<>% 
  mutate(pass_count_forward_complex=pass_sequence_count_forward2+pass_sequence_count_forward3+pass_sequence_count_forward4+pass_sequence_count_forward5+pass_sequence_count_forward6+pass_sequence_count_forward7+pass_sequence_count_forward8) %>%
  mutate(pass_count_forward_sequences=pass_sequence_count_forward1+pass_sequence_count_forward2+pass_sequence_count_forward3+pass_sequence_count_forward4+pass_sequence_count_forward5+pass_sequence_count_forward6+pass_sequence_count_forward7+pass_sequence_count_forward8) %>%
  mutate(ln_pass_count_forward_complex=log(pass_count_forward_complex))

tabyl(data$league_ctry)
tabyl(data$season_name)
tabyl(data$league_season)
tabyl(data$league_time)
tabyl(data$time)

##################################
# 3. sample design for pairs
##################################

nrow(data)
# 632,967

# add zeros

data %<>%
    # rowwise() %>%
    mutate(id_min= pmin(anon_player_id1, anon_player_id2),
           id_max= pmax(anon_player_id1, anon_player_id2),
           id_pair=id_min*1000000+id_max
    ) %>%
    add_count(league_time, id_pair, name="n_pair") 
tabyl(data$n_pair)


# drop when moved together
data %<>%
    filter(n_pair<=2)
nrow(data)


# create temp file for cases with only one sided pass

# filter
data_temp=data %>%
    filter(n_pair==1)
glimpse(data_temp)


# flip p1 and p2
names(data_temp) <- gsub("p1" , "p3" , names(data_temp))
names(data_temp) <- gsub("id1", "id3", names(data_temp))
names(data_temp) <- gsub("player1", "player3", names(data_temp))

names(data_temp) <- gsub("p2" , "p1" , names(data_temp))
names(data_temp) <- gsub("id2", "id1", names(data_temp))
names(data_temp) <- gsub("player2", "player1", names(data_temp))

names(data_temp) <- gsub("p3" , "p2" , names(data_temp))
names(data_temp) <- gsub("id3", "id2", names(data_temp))
names(data_temp) <- gsub("player3", "player2", names(data_temp))



## Correct missing citizenship bits
tabyl(data_temp$citizenship_v1_p2,show_na = TRUE, show_missing_levels = TRUE)
data_temp = data_temp %>% 
    mutate(citizenship_v0_p1 = case_when(citizenship_v0_p1 == "missing p2" ~ "missing p1",
                                         TRUE ~ citizenship_v0_p1
                                         ),
           citizenship_v1_p1 = case_when(citizenship_v1_p1 == "missing p2" ~ "missing p1",
                                         TRUE ~ citizenship_v1_p1
           ),
           citizenship_v2_p1 = case_when(citizenship_v2_p1 == "missing p2" ~ "missing p1",
                                         TRUE ~ citizenship_v2_p1
           ),
           citizenship_v0_p2 = case_when(citizenship_v0_p2 == "missing p1" ~ "missing p2",
                                         TRUE ~ citizenship_v0_p2
           ),
           citizenship_v1_p2 = case_when(citizenship_v1_p2 == "missing p1" ~ "missing p2",
                                         TRUE ~ citizenship_v1_p2
           ),
           citizenship_v2_p2 = case_when(citizenship_v2_p2 == "missing p1" ~ "missing p2",
                                         TRUE ~ citizenship_v2_p2
           )
           
           )


# replace pass values with zero
zero0 <- function(x, na.rm = FALSE) 0
data_temp %<>% 
    mutate_at(c("pass_count", "ln_pass_count",  
                "pass_count_complex",  "pass_count_forward_complex", 
                 "ln_pass_count_complex", "ln_pass_count_forward_complex",
                "pass_count_sequences" ,"pass_count_forward_sequences" ), zero0) 

data_temp %<>%
    mutate(pass_sequence_count1=0,         pass_sequence_count2=0,         pass_sequence_count3=0,         pass_sequence_count4=0,
           pass_sequence_count5=0,         pass_sequence_count6=0,         pass_sequence_count7=0,         pass_sequence_count8=0,  
           pass_sequence_count_forward1=0,         pass_sequence_count_forward2=0,         pass_sequence_count_forward3=0,         pass_sequence_count_forward4=0,
           pass_sequence_count_forward5=0,         pass_sequence_count_forward6=0,         pass_sequence_count_forward7=0,         pass_sequence_count_forward8=0  
           
           )


# save and append new rows with zero pass 
write_csv(data_temp, 
          paste0(data_tidy,"p1_p2_flipped_zero.csv"))


nrow(data)
#678 890
data %<>%
    bind_rows(data_temp)
nrow(data)

tabyl(data$citizenship_v1_p1,show_na = TRUE, show_missing_levels = TRUE)


##################################
# 4. sample design for pairs: movers
##################################


# create temp files

# player 1
data1 = data %>%
  ungroup() %>% 
    group_by(league_time, time, anon_player_id1, anon_teamid) %>%
    summarise(passes_l=sum(pass_count)) %>%
    add_count(league_time, anon_player_id1, name="nlt_p1") 



data1 %<>%  arrange(league_time, anon_player_id1)
tabyl(data1$nlt_p1)

data1b=data  %>%
  ungroup() %>%
    group_by(time, anon_player_id1, anon_teamid) %>%
    summarise(passes_lt=sum(pass_count)) %>%
    add_count(time, anon_player_id1, name="nt_p1") 
data1b %<>%     arrange(time, anon_player_id1)
tabyl(data1b$nt_p1)


data1 %<>%
    left_join(data1b, by=c("time", "anon_player_id1", "anon_teamid"))

# create flags
flags_p1 = data1  %>%
  ungroup() %>%
    mutate(p1_drop_double_team_both = nt_p1== 2) %>% 
  arrange(time, anon_player_id1, desc(passes_l)) %>% 
  group_by(time, anon_player_id1) %>%
    mutate(rown = dplyr::row_number()) %>% ## row
    mutate(p1_drop_double_team_short = rown== 2) %>% 
    ungroup()


flags_p1 %<>%
    select(time, anon_player_id1,anon_teamid, p1_drop_double_team_both, p1_drop_double_team_short)
tabyl(flags_p1$p1_drop_double_team_short)
tabyl(flags_p1$p1_drop_double_team_both)

rm(data1b, data1)  


#player 2


data1=data  %>%
  ungroup() %>%
    group_by(league_time, time, anon_player_id2, anon_teamid) %>%
    summarise(passes_l=sum(pass_count)) %>%
    add_count(league_time, anon_player_id2, name="nlt_p2") 
data1 %<>%     arrange(league_time, anon_player_id2)
tabyl(data1$nlt_p2)

data1b=data  %>%
  ungroup() %>%
    group_by(time, anon_player_id2, anon_teamid) %>%
    summarise(passes_lt=sum(pass_count)) %>%
    add_count(time, anon_player_id2, name="nt_p2") 
data1b %<>%     arrange(time, anon_player_id2)
tabyl(data1b$nt_p2)

data1 %<>%
    left_join(data1b, by=c("time", "anon_player_id2", "anon_teamid"))

# create flags
flags_p2 = data1  %>%
  ungroup() %>%
  mutate(p2_drop_double_team_both = nt_p2== 2) %>% 
  arrange(time, anon_player_id2, desc(passes_l)) %>% 
  group_by(time, anon_player_id2) %>%
  mutate(rown = dplyr::row_number()) %>% ## row
  mutate(p2_drop_double_team_short = rown== 2) %>% 
  ungroup()

flags_p2 %<>%
    select(time, anon_player_id2,anon_teamid, p2_drop_double_team_both, p2_drop_double_team_short)
tabyl(flags_p1$p1_drop_double_team_both)
tabyl(flags_p2$p2_drop_double_team_both)

rm(data1b, data1)  

write_csv(flags_p1, 
          paste0(data_tidy,"flags_p1.csv"))
write_csv(flags_p2, 
          paste0(data_tidy,"flags_p2.csv"))

# combination

nrow(data)
tabyl(data$citizenship_v1_p1,show_na = TRUE, show_missing_levels = TRUE)


data %<>%
    left_join(flags_p1, by=c("time", "anon_player_id1", "anon_teamid"))

data %<>%
    left_join(flags_p2, by=c("time", "anon_player_id2", "anon_teamid"))

tabyl(data$p1_drop_double_team_both)
tabyl(data$p2_drop_double_team_both)
nrow(data)

#has flags
data1 =data %>%
  filter((p1_drop_double_team_short=="TRUE") | (p2_drop_double_team_short=="TRUE")) 

# drop the shorter spell for players moving teams
data %<>%
    filter(p1_drop_double_team_short!="TRUE" & p2_drop_double_team_short!="TRUE") 

nrow(data)
#669,025


##############################################
# replace when exposure variable is missing
##############################################

summary(data$passes_in_shared_mins1)

# replace total pass by average relative intensity
data %<>%
    mutate(ppz1=pass_count/passes_in_shared_mins1,
           ppz2=pass_count/passes_in_shared_mins2)

summary(data$ppz1)

data$ppz1<-  impute_mean(data$ppz1)
data$ppz2<-  impute_mean(data$ppz2)


summary(data$ppz1)

data %<>%
    mutate(passes_in_shared_mins1=ifelse(is.na(passes_in_shared_mins1),pass_count/ppz1   ,passes_in_shared_mins1),
           passes_in_shared_mins2=ifelse(is.na(passes_in_shared_mins2),pass_count/ppz2   ,passes_in_shared_mins2)
    )

summary(data$passes_in_shared_mins1)


# redo ln
data %<>% 
    mutate(ln_passes_in_shared_mins1=log(passes_in_shared_mins1),
           ln_passes_in_shared_mins2=log(passes_in_shared_mins2),)

data$ln_passes_in_shared_mins1[data$ln_passes_in_shared_mins1=="-Inf"] <- 0
data$ln_passes_in_shared_mins2[data$ln_passes_in_shared_mins2=="-Inf"] <- 0


# fill in loan
tabyl(data$is_loan_id1)
data$is_loan_id1[is.na(data$is_loan_id1)] <- "FALSE"
data$is_loan_id2[is.na(data$is_loan_id2)] <- "FALSE"


# drop unused vars
data %<>%
    select(-ppz1, -ppz2, -p1_drop_double_team_both, 
           -p1_drop_double_team_short,  -p1_drop_double_team_both, -p2_drop_double_team_short,
           -id_min, -id_max, -pair1, -pair2, -flag_error_pair)
data %<>%
  select(-starts_with(c("pass_sequence_count")),
         -id_pair,
         -n_pair,
         -specific_position_p1,
         -specific_position_p2
         )

nrow(data)
## 669,025

write_parquet(data, paste0(data_work,"anon_work-player-pair_sh_temp2.parquet"))
beep(sound=2)

