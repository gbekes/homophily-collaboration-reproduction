
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d03_04-variables_mansci.R

# This creates some additional vars (eg values, cultural distance) before regressions
# saves the file for regressions

# This version: v2.5: 2024-11-18
#######################################################



# reads in work data, aggregated as season-half level, directed pairs
df_work = read_parquet(file = paste0(data_work, "anon_work-player-pair_sh.parquet"),
                   as_data_frame = TRUE)

# create some key vars
# for WMS
wvs_wave7_bekes <- read.csv(paste0(data_raw,"/culture/wvs-wave7-bekes_euc.txt"))


# 1. Add WVS
# 2. define culture vars
# 3. create additional variables
# 4. create variables for the model
# 5. rename for better notation in regression
# 6. keep only relevant vars




################################
# 1. Add WVS
################################


# country re edits re WVS
pairs <- list(c("England", "Scotland"), 
              c("England", "Wales"),
              c("Northern Ireland", "Ireland"),
              c("Kosovo", "Albania")
)

for (pair in pairs) {
  wvs_wave7_bekes %<>%
    mutate(vws_flag_exact_match = ifelse(
      (citizenship_v0_p1 == pair[1] & citizenship_v0_p2 == pair[2]) | 
        (citizenship_v0_p1 == pair[2] & citizenship_v0_p2 == pair[1]), 
      1, vws_flag_exact_match))
}

# replace with sample mean
cols_to_update <- c("avr_binary", "avr_non_binary")
wvs_wave7_bekes <- wvs_wave7_bekes %>% 
  mutate_at(vars(cols_to_update), 
            funs(ifelse(citizenship_v0_p1 != citizenship_v0_p2 & vws_flag_exact_match == 0 & . == 0, 
                        mean(.), .)))

wvs_wave7_bekes %<>%
  rename(citizenship_p1=citizenship_v0_p1,
         citizenship_p2=citizenship_v0_p2)


skimr::skim(wvs_wave7_bekes)

#############################################

for (i in 0:2) {
  for (j in 0:2) {
    assign(paste0("wvs_wave7_bekes_", i, j), wvs_wave7_bekes %>%
             rename(setNames(c("avr_binary", "avr_non_binary", "vws_flag_exact_match"),
                             c(paste0("vws_avr_binary", i, j), paste0("vws_avr_non_binary", i, j), paste0("vws_flag_exact_match", i, j))))
    )
  }
}

# join 
v_values <- c("v0", "v1")
p_values <- c("p1", "p2")

for(v in v_values) {
  for(p in p_values) {
    join_data <- get(paste0("wvs_wave7_bekes_", substr(v, 2, 2), as.integer(substr(p, 2, 2)) - 1))
    
    varname1 = paste0("citizenship_", v, "_", p_values[1])
    varname2 = paste0("citizenship_", v, "_", p_values[2])
    
    df_work["tmp_p1"] = df_work[varname1]
    df_work["tmp_p2"] = df_work[varname2]
    
    df_work <- df_work %>% left_join(.,
                                         join_data %>% select(citizenship_p1, citizenship_p2, starts_with("vws")), 
                                         by = c("tmp_p1" = "citizenship_p1",
                                                "tmp_p2" = "citizenship_p2"
                                                )
                                         )
  }
}

df_work %<>%
  select(-tmp_p1, -tmp_p2)


# create minimum
df_work %<>%
  mutate(min_vws_avr_binary = pmin(vws_avr_binary00, vws_avr_binary01, vws_avr_binary10, vws_avr_binary11, na.rm = TRUE),
         min_vws_avr_non_binary = pmin(vws_avr_non_binary00, vws_avr_non_binary01, vws_avr_non_binary10, vws_avr_non_binary11, na.rm = TRUE)
  )

Hmisc::describe(df_work$min_vws_avr_binary)
Hmisc::describe(df_work$vws_flag_exact_match00)


#  Create vars for regression


df_work %<>%
  mutate(wvs_distance00= (vws_avr_binary00+ vws_avr_non_binary00)/2)
m=median(df_work$wvs_distance00[df_work$wvs_distance00>0])
df_work %<>%
  mutate(wvs_distance00_cat=case_when(
    wvs_distance00==0  ~"same country",
    wvs_distance00<=m & wvs_distance00>0  ~"similar values",
    wvs_distance00>m  ~"different values",
    TRUE ~"missing" 
  ))
tabyl(df_work$wvs_distance00_cat)


df_work %<>%
  mutate(wvs_distance_min= (min_vws_avr_binary+ min_vws_avr_non_binary)/2)
m=median(df_work$wvs_distance_min[df_work$wvs_distance_min>0])
df_work %<>%
  mutate(wvs_distance_min_cat=case_when(
    wvs_distance_min==0  ~"same country",
    wvs_distance_min<=m & wvs_distance_min>0  ~"similar values",
    wvs_distance_min>m  ~"different values",
    TRUE ~"missing" 
  ))
tabyl(df_work$wvs_distance_min_cat)



#################################################x
# 2. define culture vars
#################################################x

df_work %<>%
  mutate(same_ex_federation = pmax(same_cit_unioneast, same_cit_unionbrit)  )


df_work %<>%
  mutate(same_federation_only = ifelse(same_ex_federation==1 & same_cit_any==0   ,   1,0),
         same_colony_only    =ifelse(same_colony_any==1 & same_ex_federation==0 & same_cit_any==0  ,   1,0 ),
         same_language_only  =ifelse(common_language_native==1 & same_colony_any==0  & same_cit_any==0 & same_ex_federation==0,   1,0)
  )


df_work %<>%
  mutate(same_cult_any =ifelse((same_cit_any==1 | same_colony_only==1 | common_language_native==1 | same_federation_only==1) ,1,0  ))

percent.table(df_work$same_cult_any)

table(df_work$same_colony_only, df_work$same_language_only)
percent.table(df_work$similar_lang_any_bin, df_work$same_cit_any)

df_work %<>%
  mutate(similar_lang_any_bin_notcult =ifelse(similar_lang_any_bin==1 & same_cult_any==0  ,   1,0 ),
         similar_location_notcult     =ifelse(similar_location==1 & same_cult_any==0  ,   1,0 ),
         similar_location_notcultnotlang     =ifelse(similar_location==1 & same_cult_any==0 & similar_lang_any_bin_notcult==0  ,   1,0 )
         )

df_work %<>%
  mutate(similar_lang_cat =ifelse(similar_lang_any_bin==1 & same_cult_any==0  ,   1,0 ))
         
df_work %<>%
  mutate(similar_lang_cat=case_when(
    same_cit_any==1  ~"same country",
    same_cit_any==0 & common_language_native==1  ~"diff country, same language",
    same_cit_any==0 & common_language_native==0 & similar_lang_any_bin==1  ~"dff country, similar language",
    same_cit_any==0 & common_language_native==0 & similar_lang_any_bin==0  ~"dff country, diff language",
    TRUE ~"missing" 
  ))
tabyl(df_work$similar_lang_cat)

df_work %<>%
  mutate(similar_location_notsimlang     =ifelse(similar_location==1 & 
                                                   same_cit_any==0 & common_language_native==0 & similar_lang_any_bin==0  ,   1,0 )
  )


tabyl(df_work$similar_lang_any_bin_notcult)  #6.6%       
tabyl(df_work$similar_location_notcult)  #10.1%       
tabyl(df_work$similar_location_notcultnotlang)  #7.5%       


#################################################x
# 3. create additional variables
#################################################x

# height difference

df_work %<>%
  mutate(height_difference=abs(height_p1-height_p2))



# value difference in values: below above median =3.5
df_work %<>%
  mutate(
    value_diff = case_when(if_else(value_p1/value_p2 > value_p2/value_p1, value_p1/value_p2, value_p2/value_p1) >= 100 ~ 100,
                           TRUE ~ if_else(value_p1/value_p2 > value_p2/value_p1, value_p1/value_p2, value_p2/value_p1)
    ),
    same_value=ifelse(value_diff<3.5,1,0)
    
  )

# experience
tabyl(df_work$new1_id1)

df_work %<>% 
  dplyr::rowwise() %>%
  mutate(experience1=1-new1_id1,
         experience2=1-new1_id2,
         experience_shared1=1-max(new1_id1, new1_id2),
         experience_shared2=1-max(new2_id1, new2_id2),
         experience_days=min(w_club_days_id1,w_club_days_id2),
         ln_experience_days=log(min(w_club_days_id1,w_club_days_id2))
  )


# age: Q1 vs Q2-Q4
Hmisc::describe(df_work$age_p1_y)
bincat_age21=0.25
df_work$age_p1_y_bin2 <- with(df_work, cut(age_p1_y, 
                                               breaks=quantile(age_p1_y, probs=c(0, bincat_age21 ,1 ), na.rm=TRUE), 
                                               include.lowest=TRUE,
                                               labels=c("Q1","Q2-Q4")))
tabyl(df_work$age_p1_y_bin2)


# by experience
## Days with club, version 1: no interruption 
Hmisc::describe(df_work$w_club_days_id1)
df_work$w_club_p1_y_bin1 <- with(df_work, cut(w_club_days_id1, 
                                                  breaks=quantile(w_club_days_id1, probs=c(0, 0.25, 0.75,1 ), na.rm=TRUE), include.lowest=TRUE, 
                                                  labels=c("Q1","Q2-Q3","Q4")))
tabyl(df_work$w_club_p1_y_bin1)

df_work$w_club_p1_y_bin2 <- with(df_work, cut(w_club_days_id1, 
                                                  breaks=quantile(w_club_days_id1, probs=c(0, 0.5 ,1 ), na.rm=TRUE), include.lowest=TRUE, 
                                                  labels=c("H1","H2")))
tabyl(df_work$w_club_p1_y_bin2)


# shared experience before
df_work %<>%
  mutate(experience_other_team=ifelse(shared_teams_count>1,1,0))
tabyl(df_work$experience_other_team)


# define values
experience_low_cut= 215
experience_high_cut= 364
experience_end_cut =729


df_work$w_days_shared_p1_y_bin1 <- with(df_work, cut(experience_days, 
                                                         breaks=c(0,experience_low_cut,6000), include.lowest=TRUE, 
                                                         labels=c("newish","experienced")))
tabyl(df_work$w_days_shared_p1_y_bin1)


df_work$w_days_shared_p1_y_bin2 <- with(df_work, cut(experience_days, 
                                                         breaks=c(0,experience_high_cut,6000), include.lowest=TRUE, 
                                                         labels=c("newish","experienced")))
tabyl(df_work$w_days_shared_p1_y_bin2)



df_work %<>%
  mutate(new_median_id1=ifelse(w_club_days_id1>experience_low_cut,1,0),
         new_median_id2=ifelse(w_club_days_id1>experience_high_cut,1,0)
  )

# create long pass
df_work %<>% mutate(long_pass=ifelse(ln_avg_pass_length>2.791,1,0))


# group size 
summary(df_work$culture_all_n_p1_mean)
df_work %<>%
  mutate(culture_all_n_p1_mean=ifelse(is.na(culture_all_n_p1_mean) ,5, culture_all_n_p1_mean),
         culture_all_n_p2_mean=ifelse(is.na(culture_all_n_p2_mean) ,5, culture_all_n_p2_mean))
datasummary(data=df_work, culture_all_n_p1_mean*league_ctry ~ mean  )
tabyl(df_work$league_ctry)

# large group cutoff at 4
df_work %<>%
  mutate(culture_group_large_p1_c4=ifelse(culture_all_n_p1_mean>=4,1,0) )
tabyl(df_work$culture_group_large_p1_c4)


# Does receiver quality matter? 
Hmisc::describe(df_work$ln_value_p2)

# top higher valued players
high_ln_value=16.52 #15 mill
df_work %<>%
  mutate(ln_value_p2_bin_nom=ifelse(ln_value_p2>=high_ln_value,1,0))
tabyl(df_work$ln_value_p2_bin_nom) # 14.6%

# pick top two by passer
df_work %<>%
  group_by(anon_teamid, time, anon_player_id1) %>%
  mutate(ln_value_p2_bin_top2 = ln_value_p2 %in% head(sort(ln_value_p2, decreasing = TRUE), 2))

tabyl(df_work$ln_value_p2_bin_top2) # 12%


# review group to group at cutoff of 3
summary(df_work$culture_inoutgroup_g0g0_cut3)
summary(df_work$culture_inoutgroup_g0g1_cut3)
summary(df_work$culture_inoutgroup_g1g0_cut3)
summary(df_work$culture_inoutgroup_g1g1_cut3)

# corrections
df_work %<>%
  mutate(culture_inoutgroup_g0g0_cut3=ifelse(culture_inoutgroup_g0g0_cut3==1 & same_cult_any==1,0,culture_inoutgroup_g0g0_cut3),
         culture_inoutgroup_g0g1_cut3=ifelse(culture_inoutgroup_g0g1_cut3==1 & same_cult_any==1,0,culture_inoutgroup_g0g1_cut3),
         culture_inoutgroup_g1g0_cut3=ifelse(culture_inoutgroup_g1g0_cut3==1 & same_cult_any==1,0,culture_inoutgroup_g1g0_cut3),
         culture_inoutgroup_g1g1_cut3=ifelse(culture_inoutgroup_g1g1_cut3==1 & same_cult_any==1,0,culture_inoutgroup_g1g1_cut3),
  )



####################################
# 4. create variables for the model
####################################


# so tau = T_o,d / P_o 
df_work %<>%
  mutate(ln_tau_od= ln_passes_in_shared_mins1-ln_sh_pass_player_id1
         )

df_work %<>%
  mutate(ln_total_pass_by_passer_sh=log(total_pass_by_passer_sh),
         ln_total_reception_by_receiver_sh=log(total_reception_by_receiver_sh))

Hmisc::describe (df_work$ln_total_pass_by_passer_sh)


# near goal
summary(df_work$pass_count) 
Hmisc::describe(df_work$pass_neargoal) #0.39/15.9 =2.5%
data_near= df_work
data_near %<>% filter(pass_neargoal>0)


#############################################
# 5. rename for better notation in regression
#############################################

df_work %<>%
  rename(half_season=time,
         league_half_season=league_time,
         team=anon_teamid) 
df_work %<>%
  rename(passer=anon_player_id1,
         receiver=anon_player_id2,
         passer_position=p1_position,
         receiver_position=p2_position,
         passer_nationality=p1_citizenship,
         receiver_nationality=p2_citizenship,
         passer_position2=broad_position_p1,
         receiver_position2=broad_position_p2)

df_work %<>%
  mutate(passer_position_3G=passer_position2,
         receiver_position_3G=receiver_position2
  )


# Combine similar positions into 3 groups

# Define column names and value mapping
columns <- c("passer_position_3G", "receiver_position_3G")
value_map <- c("attack" = "Forward", 
               "Goalkeeper" = "Defender", 
               "midfield" = "Midfielder")


# Define old values and new values
old_values <- names(value_map)
new_values <- value_map

#library(plyr)
# Apply mapvalues function to each column
for (col in columns){
  df_work[[col]] <- mapvalues(df_work[[col]], from = old_values, to = new_values)
}


# Summary of passer_position_3G column
tabyl(df_work$passer_position_3G)
tabyl(df_work$receiver_position_3G)


df_work %<>%
  mutate(pos3G=paste(passer_position_3G, receiver_position_3G, sep="_")         )
tabyl(df_work$pos3G)
summary(df_work$shared_teams_count)



#############################################
# 6. keep only relevant vars
#############################################



vars_to_keep = c("league_ctry", "season_name", "season_name_half", "team", 
                 "league_half_season",
                 "half_season", "passer", "receiver",
                 "ln_sh_pass_player_id1", "ln_sh_pass_player_id2",
                 "ln_avg_pass_length", "forwardness",
                 "ln_sh_pass_player_id1", "ln_sh_pass_player_id2",
                 "height_difference", "value_diff", "same_cit_eu",
                 "passer_position", "receiver_position", 
                 "passer_nationality", "receiver_nationality", "culture_all_same_culture_new",
                 "citizenship_v0_p1", "citizenship_v0_p2",
                 "citizenship_v1_p1", "citizenship_v1_p2",
                 "passer_position2", "receiver_position2", 
                 "passer_position_3G", "receiver_position_3G",
                 "ln_value_p1", "ln_value_p2",
                 "age_p1_y", "age_p2_y", "w_club_days_id1", "w_club_days_id2",
                 "height_p1", "height_p2", "is_loan_id1", "is_loan_id2",
                 "citizenship_v0_p1", "pos3G",
                 "age_p1_y_bin2", 
                 "ln_tau_od", "culture_all_n_p1_mean", "culture_all_n_p2_mean",
                 "culture_group_large_p1_c4",
                 "culture_inoutgroup_g0g0_cut3", "culture_inoutgroup_g0g1_cut3",
                 "culture_inoutgroup_g1g0_cut3", "culture_inoutgroup_g1g1_cut3",
                 "same_cult_any",  "passes_in_shared_mins", "ln_pass_permin",
                 "ln_passes_in_shared_mins", "passes_in_shared_mins1",
                 "pass_count", "ln_total_pass_by_passer_sh", "ln_total_reception_by_receiver_sh",
                 "ln_passes_in_shared_mins1", "experience_shared1", "experience_other_team",
                 "long_pass", "similar_lang_cat", "wvs_distance_min_cat","wvs_distance00", "wvs_distance00_cat",
                 "pass_count_sequences", "pass_count_complex", "pass_count_forward_sequences",
                 "pass_count_forward_complex", "pass_neargoal", 
                 "ln_value_p2_bin_top2", "w_days_shared_p1_y_bin1", "similar_location",
                 "same_cult_any", "same_language_only", "same_ex_federation", "same_federation_only",
                 "same_cit_any", "same_colony_only", "same_colony", "same_colony_any",
                 "same_cit_unioneast", "same_cit_unionbrit",
                 "same_colony_sibling", "new_median_id1", "new_median_id2",
                 "w_club_days_id1", "w_days_shared_p1_y_bin2",
                 "new1_id1"
                 )
  
df_work = df_work %>% select(all_of(vars_to_keep))



# Saving on object in RData format
saveRDS(df_work, file=paste0(data_work,"anon_work-regressions.RData"))
beep(sound=2)


