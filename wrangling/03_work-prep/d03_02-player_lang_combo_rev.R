
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d03_02-player_lang_combo_rev.R

# Creates player pairs with common language vars

# This version: v2.1: 2024-11-18
#######################################################




#######################################################
# new language metric

# in: 
# anon_work-player-pair_sh_temp2.parquet
# anon_player_native_languages.csv

# out: 
# anon_player_combos_season_native_language.csv

#######################################################


rm(all_culture_season_half,data, data_temp, data1, flags_p1, flags_p2)

###################################################
## CREATE players_combos_common_language
###################################################

all_events_season = read_parquet(paste0(data_work,"anon_work-player-pair_sh_temp2.parquet"))
player_native_languages = read.csv2(paste0(data_tidy_created, "anon_player_native_languages.csv"))


player_combos_season = all_events_season %>% 
  distinct(anon_player_id1, anon_player_id2, season)

## Common nationality and birth country

player_combos_season_birth = all_events_season %>%
  group_by(anon_player_id1, anon_player_id2) %>% 
  summarise(citizenship_adj_v0_p1 = first(citizenship_adj_v0_p1),
            citizenship_adj_v1_p1 = first(citizenship_adj_v1_p1),
            citizenship_adj_v2_p1 = first(citizenship_adj_v2_p1),
            
            citizenship_adj_v0_p2 = first(citizenship_adj_v0_p2),
            citizenship_adj_v1_p2 = first(citizenship_adj_v1_p2),
            citizenship_adj_v2_p2 = first(citizenship_adj_v2_p2)
  )%>%
  ungroup() %>%   
  mutate(common_nationality = case_when(citizenship_adj_v0_p1 == citizenship_adj_v0_p2 ~ 1,
                                        citizenship_adj_v1_p1 == citizenship_adj_v0_p2 ~ 1,
                                        citizenship_adj_v2_p1 == citizenship_adj_v0_p2 ~ 1,
                                        
                                        citizenship_adj_v0_p1 == citizenship_adj_v1_p2 ~ 1,
                                        citizenship_adj_v1_p1 == citizenship_adj_v1_p2 ~ 1,
                                        citizenship_adj_v2_p1 == citizenship_adj_v1_p2 ~ 1,
                                        
                                        citizenship_adj_v0_p1 == citizenship_adj_v2_p2 ~ 1,
                                        citizenship_adj_v1_p1 == citizenship_adj_v2_p2 ~ 1,
                                        citizenship_adj_v2_p1 == citizenship_adj_v2_p2 ~ 1,
                                        TRUE ~ 0
  ),
  common_birthcountry = case_when(citizenship_adj_v2_p1 == citizenship_adj_v2_p2 ~ 1, ## v2 is birthcountry
                                  TRUE ~ 0
  ),
  common_nation_birthcountry = common_nationality * common_birthcountry
  ) 


###########################
## National languages
############################

player_native_languages = player_native_languages %>% 
  rename(Languages = Language, 
         Weights = Weight)


player_combos_season_native = player_combos_season %>%
  group_by(anon_player_id1, anon_player_id2) %>% 
  summarise(n = 1) %>% 
  select(-n) %>%
  left_join(., player_native_languages %>% 
              select(anon_player_id, Languages, anon_other_player_id, Weights),
            by = c("anon_player_id1" = "anon_player_id"
            )
  ) %>% 
  ungroup()


player_combos_season_native = player_combos_season_native %>%
  rename(anon_other_player_id1 = anon_other_player_id,
         Weights1 = Weights
  )

skim(player_combos_season_native)
tabyl(player_combos_season_native$Weights1)

# finding when common language
player_combos_season_native = player_combos_season_native %>% 
  left_join(., player_native_languages %>% 
              select(anon_player_id, Languages, anon_other_player_id, Weights),
            by = c("anon_player_id2" = "anon_player_id",
                   "Languages" = "Languages"
            )
  )

player_combos_season_native = player_combos_season_native %>%
  rename(anon_other_player_id2 = anon_other_player_id,
         Weights2 = Weights
  )

nrow(player_combos_season_native)

# keep no common language
player_combos_season_native = player_combos_season_native %>% 
  filter(!is.na(Weights2))
nrow(player_combos_season_native)


rm(player_native_languages)


# Then finally we calculate the common language as a simple product of the language weights.

player_combos_season_native = player_combos_season_native %>% 
  select(anon_other_player_id1, anon_player_id1, 
         anon_other_player_id2, anon_player_id2,
         Languages_native = Languages, 
         Weights1_native = Weights1, 
         Weights2_native = Weights2
  ) %>%
  mutate(common_language_native = Weights1_native * Weights2_native) %>% 
  group_by(anon_player_id1, anon_player_id2) %>% 
  summarise(common_language_native = max(common_language_native, na.rm = TRUE)) %>% 
  ungroup()


# Join them onto the birth table

player_combos_season_native = player_combos_season_birth %>% 
  left_join(player_combos_season_native,
            by = c("anon_player_id1" = "anon_player_id1",
                   "anon_player_id2" = "anon_player_id2"
            )
  ) %>% 
  mutate(common_language_native = case_when(is.na(common_language_native) ~ 0.0,
                                            TRUE ~ common_language_native)
  )


# Then aggregate to couple level.
# For the native ones first we create the necessary variables, then aggregate, then join to season level.

player_combos_season_native = player_combos_season_native %>% 
  group_by(anon_player_id1, 
           anon_player_id2) %>% 
  summarise(common_nationality = max(common_nationality, na.rm = TRUE),
            common_birthcountry = max(common_birthcountry, na.rm = TRUE),
            common_language_native = max(common_language_native, na.rm = TRUE),
            common_nation_birthcountry=max(common_nation_birthcountry, na.rm = TRUE)
  ) %>% 
  ungroup()

rm(player_combos_season_birth)

player_combos_season_native = player_combos_season %>% 
  left_join(.,
            player_combos_season_native,
            by = c("anon_player_id1",
                   "anon_player_id2"
                   )
            )
rm(player_combos_season)

write_parquet(player_combos_season_native,
              paste0(data_work, "anon_player_combos_season_native_language.parquet"))

beep(sound=2)
