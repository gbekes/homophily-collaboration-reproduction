
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d03_03-language_join_mansci.R

# creates culture and nationality related variables at the player-pair level

# This version: v2.1: 2024-11-18
#######################################################






#######################################################
# in: 
# anon_work-player-pair_sh_temp2.parquet
# anon_player_combos_season_native_language.csv
# ling_web_augmented.csv

# out: 
# anon_work-player-pair_sh.parquet

#######################################################


############### 
# set data dir, data used
############### 
rm(player_combos_season_native, all_events_season)


#######################################
## Conjoin
#######################################

#pair level
data= read_parquet(file = paste0(data_work, "anon_work-player-pair_sh_temp2.parquet"),
                   as_data_frame = TRUE)

players_combos_common_language <-read_parquet(paste0(data_work,"anon_player_combos_season_native_language.parquet"))

# temporary
ling_web_augmented = read_csv(paste0(data_tidy_created,"ling_web_augmented.csv"))


tabyl(players_combos_common_language$common_language_native, show_missing_levels = TRUE  )

data=data %>%
  left_join(.,players_combos_common_language,
            by = c("anon_player_id1" = "anon_player_id1",
                   "anon_player_id2" = "anon_player_id2",
                   "season"="season"))
tabyl(data$common_language_native, show_missing_levels = TRUE  )



## 1. pairwise nationality variables
## 2. join ling_web things to get shared colonial past etc
## 3. creating EU variable by football law
## 4. creating federal state variables
## 5. key definitions 
## 6. final cut: no single partner player
## 7. save



##################################################################
##1. pairwise nationality variables
##################################################################

# corrections for birth country
data %<>% mutate(
  citizenship_v2_p1=ifelse(citizenship_v2_p1=="UdSSR", "Russia", citizenship_v2_p1),
  citizenship_v2_p1=ifelse(citizenship_v2_p1=="DDR", "Germany", citizenship_v2_p1),
  citizenship_v2_p1=ifelse(citizenship_v2_p1=="CSSR", "Czech Republic", citizenship_v2_p1),
  citizenship_v2_p1=ifelse(citizenship_v2_p1=="Yugoslavia (Republic)" | citizenship_v2_p1=="Jugoslawien (SFR)", "Serbia", citizenship_v2_p1),
  
  citizenship_v2_p2=ifelse(citizenship_v2_p2=="UdSSR", "Russia", citizenship_v2_p2),
  citizenship_v2_p2=ifelse(citizenship_v2_p2=="DDR", "Germany", citizenship_v2_p2),
  citizenship_v2_p2=ifelse(citizenship_v2_p2=="CSSR", "Czech Republic", citizenship_v2_p2),
  citizenship_v2_p2=ifelse(citizenship_v2_p2=="Yugoslavia (Republic)" | citizenship_v2_p2=="Jugoslawien (SFR)", "Serbia", citizenship_v2_p2)
  
)


tabyl(data$citizenship_v2_p1,show_na = TRUE, show_missing_levels = TRUE)


data %<>%
  ungroup() %>% 
  mutate(
    same_cit_any=case_when(
      citizenship_v0_p1== citizenship_v0_p2  ~"1",
      citizenship_v0_p1== citizenship_v1_p2  ~"1",
      citizenship_v0_p1== citizenship_v2_p2  ~"1",
      
      citizenship_v1_p1== citizenship_v0_p2  ~"1",
      citizenship_v1_p1== citizenship_v1_p2  ~"1",
      citizenship_v1_p1== citizenship_v2_p2  ~"1",
      
      citizenship_v2_p1== citizenship_v0_p2  ~"1",
      citizenship_v2_p1== citizenship_v1_p2  ~"1",
      citizenship_v2_p1== citizenship_v2_p2  ~"1",
      TRUE ~"0"  )
  )

data %<>%
  mutate (same_cit0  = ifelse(citizenship_v0_p1== citizenship_v0_p2,1, 0))

data %<>%
  group_by(league_time, anon_player_id1) %>%
  mutate(n_partners_p1=n_distinct(anon_player_id2),
         n_cit_any_p1=n_distinct(same_cit_any)
  ) %>%
  ungroup()


data %<>%
  mutate(same_cit_any=as.numeric(same_cit_any))

# moved from p1
data %<>%
  group_by(league_time, anon_player_id1) %>%
  mutate(n_partners_p1=n_distinct(anon_player_id2),
         n_cit_any_p1=n_distinct(same_cit_any)
  ) %>%
  ungroup()


glimpse(data)

###############################
## 2. join ling_web things to get shared colonial past etc
###############################

citizenship_types = c("0", "1", "2")
c_vars = c("col", "cle", "contig", "colony", "sibling")

for(p1types in citizenship_types){
  for(p2types in citizenship_types){
  
    varname_p1 = paste0("citizenship_v", p1types, "_p1")
    varname_p2 = paste0("citizenship_v", p2types, "_p2")
    
    data["country_o"] = data[varname_p1]
    data["country_d"] = data[varname_p2]
    
    #v0-v0
    data %<>%
      left_join(.,
                ling_web_augmented,
                by = c("country_o",
                       "country_d"
                       )
                )
    # keep what matters and generate ctry-pair based vars
    for(vars in c_vars){
      data[paste0(vars, "_c", p1types, p2types)] = data[vars]
    }
 
#  cutting unused vars
    data %<>% 
      select(-csl, -cnl, -prox1, -prox2, -lp1, -lp2, -cl, -starts_with("comlang_ethno") , -starts_with("comlang_off") ,
             -serb_yugo_o, -serb_yugo_d, -full_yugo_o, -full_yugo_d, -iso_o, -iso_d,
             -col, -cle, -contig, -colony, -sibling 
             )  
    
  }
  
}


glimpse(data)


rm(ling_web_augmented)
rm(players_combos_common_language)



## Create pairwise variables

## missings are 0-s
versions = c("c00", "c01", "c02", "c10", "c11", "c12", "c20", "c21", "c22")

for(l in c_vars){
  for(version in versions){
    varname = paste0(l, "_", version)
    data[varname] = if_else(is.na(data[varname]), 0, data[varname] %>% pull())
  }
}

# rename the colony sibling
names(data)[str_detect(names(data), "sibling")] = str_replace_all(names(data)[str_detect(names(data), "sibling")], "sibling", "colony_sibling")


############################################
# 3. creating EU variable by football law
############################################

data %<>%
  mutate(citizenship_eu_p1 = case_when(
    ((citizenship_v0_p1 %in% eu_standard) | 
       (citizenship_v1_p1 %in% eu_standard) | 
       (citizenship_v2_p1 %in% eu_standard) 
    ) ~ "yes",
    country == "Spain" & 
      ((citizenship_v0_p1 %in% eu_extra_spain) | 
         (citizenship_v1_p1 %in% eu_extra_spain) | 
         (citizenship_v2_p1 %in% eu_extra_spain)
      ) ~ "yes",
    country == "Spain" &
      ((citizenship_v0_p1 %in% eu_maybe_spain) | 
         (citizenship_v1_p1 %in% eu_maybe_spain) |
         (citizenship_v2_p1 %in% eu_maybe_spain)
      ) ~ "maybe",
    country == "France" & 
      ((citizenship_v0_p1 %in% eu_extra_france) | 
         (citizenship_v1_p1 %in% eu_extra_france) | 
         (citizenship_v2_p1 %in% eu_extra_france)
      ) ~ "yes",
    TRUE ~ "no"
  )
  )


data %<>%
  mutate(citizenship_eu_p2 = case_when(
    ((citizenship_v0_p2 %in% eu_standard) | 
       (citizenship_v1_p2 %in% eu_standard) | 
       (citizenship_v2_p2 %in% eu_standard) 
    ) ~ "yes",
    country == "Spain" & 
      ((citizenship_v0_p2 %in% eu_extra_spain) | 
         (citizenship_v1_p2 %in% eu_extra_spain) | 
         (citizenship_v2_p2 %in% eu_extra_spain)
      ) ~ "yes",
    country == "Spain" &
      ((citizenship_v0_p2 %in% eu_maybe_spain) | 
         (citizenship_v1_p2 %in% eu_maybe_spain) |
         (citizenship_v2_p2 %in% eu_maybe_spain)
      ) ~ "maybe",
    country == "France" & 
      ((citizenship_v0_p2 %in% eu_extra_france) | 
         (citizenship_v1_p2 %in% eu_extra_france) | 
         (citizenship_v2_p2 %in% eu_extra_france)
      ) ~ "yes",
    TRUE ~ "no"
  )
  )
tabyl(data$citizenship_eu_p2)

data%<>%
  mutate(same_cit_eu = ifelse(citizenship_eu_p1=="yes" & citizenship_eu_p2=="yes",1,0 ))
tabyl(data$same_cit_eu)


############################################
# 4. creating federal state variables
############################################

# yugo7
data %<>%
  mutate(
    citizenship_yugo7_p1 = case_when(
      ((citizenship_v0_p1 %in% yugo7) | 
       (citizenship_v1_p1 %in% yugo7) | 
       (citizenship_v2_p1 %in% yugo7) ) ~ "yes",   TRUE ~ "no"),
    citizenship_yugo7_p2 = case_when(
      ((citizenship_v0_p2 %in% yugo7) | 
       (citizenship_v1_p2 %in% yugo7) | 
       (citizenship_v2_p2 %in% yugo7) ) ~ "yes",   TRUE ~ "no")
  )
data%<>%
  mutate(same_cit_yugo7 = ifelse(citizenship_yugo7_p1=="yes" & 
                                   citizenship_yugo7_p2=="yes" & same_cit_any==0,1,0 ))
tabyl(data$citizenship_yugo7_p2)



#ussr
data %<>%
  mutate(
    citizenship_ussr_p1 = case_when(
      ((citizenship_v0_p1 %in% ussr) | 
         (citizenship_v1_p1 %in% ussr) | 
         (citizenship_v2_p1 %in% ussr) ) ~ "yes",   TRUE ~ "no"),
    citizenship_ussr_p2 = case_when(
      ((citizenship_v0_p2 %in% ussr) | 
         (citizenship_v1_p2 %in% ussr) | 
         (citizenship_v2_p2 %in% ussr) ) ~ "yes",   TRUE ~ "no")
  )
data%<>%
  mutate(same_cit_ussr = ifelse(citizenship_ussr_p1=="yes" & citizenship_ussr_p2=="yes"  & same_cit_any==0,1,0 ))
tabyl(data$citizenship_ussr_p2)



#czsk
data %<>%
  mutate(
    citizenship_czsk_p1 = case_when(
      ((citizenship_v0_p1 %in% czsk) | 
         (citizenship_v1_p1 %in% czsk) | 
         (citizenship_v2_p1 %in% czsk) ) ~ "yes",   TRUE ~ "no"),
    citizenship_czsk_p2 = case_when(
      ((citizenship_v0_p2 %in% czsk) | 
         (citizenship_v1_p2 %in% czsk) | 
         (citizenship_v2_p2 %in% czsk) ) ~ "yes",   TRUE ~ "no")
  )
data%<>%
  mutate(same_cit_czsk = ifelse(citizenship_czsk_p1=="yes" & citizenship_czsk_p2=="yes"  & same_cit_any==0,1,0 ))
tabyl(data$citizenship_czsk_p2)

#uki
data %<>%
  mutate(
    citizenship_uki_p1 = case_when(
      ((citizenship_v0_p1 %in% uk_irish) | 
         (citizenship_v1_p1 %in% uk_irish) | 
         (citizenship_v2_p1 %in% uk_irish) ) ~ "yes",   TRUE ~ "no"),
    citizenship_uki_p2 = case_when(
      ((citizenship_v0_p2 %in% uk_irish) | 
         (citizenship_v1_p2 %in% uk_irish) | 
         (citizenship_v2_p2 %in% uk_irish) ) ~ "yes",   TRUE ~ "no")
  )
data%<>%
  mutate(same_cit_uki = ifelse(citizenship_uki_p1=="yes" & citizenship_uki_p2=="yes"  & same_cit_any==0,1,0 ))
tabyl(data$same_cit_uki)

tabyl(data$same_cit_czsk)
tabyl(data$same_cit_yugo7)
tabyl(data$same_cit_ussr)
tabyl(data$same_cit_uki)


data%<>%
  mutate(same_cit_unioneast = ifelse(same_cit_yugo7==1 | same_cit_ussr==1  | same_cit_czsk==1  ,1,0 ),
         same_cit_unionbrit = ifelse(same_cit_uki==1  ,1,0 )
         ) %>% 
  select(-citizenship_yugo7_p1, -citizenship_yugo7_p1, -same_cit_yugo7,
         -citizenship_uki_p1, -citizenship_uki_p2, -same_cit_uki,
         -citizenship_ussr_p1, -citizenship_ussr_p2, -same_cit_ussr,
         -citizenship_czsk_p1, -citizenship_czsk_p2, -same_cit_ussr
         )

tabyl(data$same_cit_unioneast)
tabyl(data$same_cit_unionbrit)


########################################
## 5. key definitions 
########################################

# eventually changed to 0-1, ie 1 even if in a country multiple off language
tabyl(data$common_language_native)
data %<>%
  mutate(common_language_native=ifelse(common_language_native>0.5, 1, common_language_native) )
tabyl(data$common_language_native)


# colony etc definitions based on CEPII -- for any citizenship pairs
data %<>%
  mutate(same_colony = 1 * ((colony_c00 + colony_c01 + colony_c02 + colony_c10 + colony_c11 + colony_c12 + colony_c20 + colony_c21 + colony_c22) > 0),
         same_colony_sibling = 1 * ((colony_sibling_c00 + colony_sibling_c01 + colony_sibling_c02 + colony_sibling_c10 + colony_sibling_c11 + colony_sibling_c12 + colony_sibling_c20 + colony_sibling_c21 + colony_sibling_c22) > 0),
         similar_lang_any_bin= 1* (pmax(cle_c00,cle_c01,cle_c02,cle_c10,cle_c11,cle_c12,cle_c20,cle_c21,cle_c22)>0.5),
         similar_location = 1 * ((contig_c00 + contig_c01 + contig_c02 + contig_c10 + contig_c11 + contig_c12 + contig_c20 + contig_c21 + contig_c22) > 0)  )

# same colony any = either ruler and colony or shared ruler
data %<>%
  mutate(same_colony_any=pmax(same_colony, same_colony_sibling))

# drop not needed vars
data %<>% select(-starts_with(c("col_", "cle_", "contig_", "colony_", "comlang_ethno", "comlang_off")) ) 


# add aggregates
data %<>%
  group_by(season_name_half, anon_player_id1) %>%
  mutate(match_player1_has_same=any(same_cit_any==1), 
         match_player1_has_diff=any(same_cit_any==0), 
         match_player2_has_same=any(same_cit_any==1), 
         match_player2_has_diff=any(same_cit_any==0)) %>% 
  ungroup()


# define multi-cultural players (not used just to check)
data %<>% mutate(
  multi_cult_p1=ifelse(citizenship_v1_p1!="missing p1"  & citizenship_v1_p1!="missing p2",1,0),
  multi_cult_p2=ifelse(citizenship_v1_p2!="missing p1" & citizenship_v1_p2!="missing p2",1,0),
  multi_cult_both=ifelse(multi_cult_p1==1 & multi_cult_p2==1,1,0)
)

##############################
## 6. final cut: no single partner player
##############################

data %<>%
  group_by(league_time, anon_player_id1) %>%
  mutate(n_partners_p1_new=n_distinct(anon_player_id2)) %>%
  ungroup()
tabyl(data$n_partners_p1_new)
data %<>% 
  filter(n_partners_p1_new>1) %>% 
  select(-n_partners_p1_new)
nrow(data)


data %<>%
  select(    -sh,         -country_o,         -country_d         )#



##############################
## 7 save 
##############################
nrow(data)
# 669022

write_parquet(data, paste0(data_work,"anon_work-player-pair_sh.parquet"))
beep(sound=2)

