
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d02_03_player_languages_anon.R

# From a pre-set country-language table we join onto the players the languages their citizenships indicate they speak. 
# Then we collect their player history, and from the third consecutive half-season the language of the country of the club they play in is considered "acquired". Then we aggregate to the season level, and collect all languages that the player knew in each season in a wide format, and fill out the gaps.

# This version: v2.1: 2024-11-18
#######################################################




#################################
## Country-Language tables
#################################

#Load languages

# countries and languages -- _lim excludes english as second language
countries_languages = read_csv(paste0(data_imported,"/countries_languages_lim.csv"))
# country codes for transactions
countries_codes = read_parquet(file = paste0(data_imported,"/countries.parquet"),
                               as_data_frame = TRUE)

player_acquired_languages = read_parquet(paste0(data_tidy_created,"anon_MV_transf_joint.parquet"))

players = read_parquet(paste0(data_tidy,"anon_players.parquet"))

# Country codes with their languages, recode those that are not in good format.
language_list = countries_languages %>% 
  distinct(Language) %>% pull()

countries_codes = countries_codes %>% 
  mutate(language_name = case_when(name == "Bosnia & Herzegovina" ~ "Bosnia-Herzegovina",
                                   name == "USA" ~ "United States",
                                   name %in% c("England", "Scotland",
                                               "Wales", "Northern Ireland",
                                               "Jersey", "Bermuda", "Gibraltar", 
                                               "Montserrat", "Isle of Man", "Anguilla") ~ "United Kingdom",
                                   name %in% c("Faroe Islands") ~ "Denmark",
                                   name == "Macedonia" ~ "North Macedonia",
                                   name == "Columbia" ~ "Colombia",
                                   TRUE ~ name),
         
  ) %>% 
  left_join(., countries_languages, 
            by = c("language_name" = "Country")
  )

# Country codes
write.csv2(countries_codes, paste0(data_tidy_created, "countries_codes_languages.csv"),
           row.names = FALSE)


#################################
## Native languages
#################################

#First, we need for each player the initial set of languages he speaks.

# Load data ---------------------------------
players_languages = players %>% 
  select(anon_player_id, anon_other_player_id,
         place_of_birth_country
         , `citizenship-0`, `citizenship-1`)

# unify country names + combine citizenships when in same polity
players_languages = players_languages %>% 
  pivot_longer(cols = contains("citizenship"), 
               names_to = "citizenship_rank",
               values_to = "citizenship"
  ) %>% 
  mutate(country_joiner = case_when(`citizenship` %in% c("England", "Scotland", 
                                                         "Wales", "Northern Ireland",
                                                         "Jersey", "Bermuda", "Gibraltar", 
                                                         "Montserrat", "Isle of Man", "Anguilla") ~ "United Kingdom",
                                    `citizenship` %in% c("Curacao", "Aruba") ~ "Netherlands",
                                    `citizenship` %in% c("Korea, South") ~ "South Korea",
                                    `citizenship` %in% c("DR Congo") ~ "Democratic Republic of the Congo",
                                    `citizenship` %in% c("Cote d'Ivoire") ~ "Ivory Coast",
                                    `citizenship` %in% c("Congo") ~ "Republic of the Congo",
                                    `citizenship` %in% c("Martinique", "Guadeloupe", "French Guiana", 
                                                         "Réunion", "Tahiti", "Neukaledonien") ~ "France",
                                    `citizenship` %in% c("The Gambia") ~ "Gambia",
                                    `citizenship` %in% c("Puerto Rico", "Guam") ~ "United States",
                                    `citizenship` %in% c("Faroe Islands") ~ "Denmark",
                                    `citizenship` %in% c("St. Kitts & Nevis") ~ "Saint Kitts and Nevis",
                                    `citizenship` %in% c("St. Vincent & Grenadinen") ~ "Saint Vincent and the Grenadines",
                                    `citizenship` %in% c("Korea, North") ~ "North Korea",
                                    `citizenship` %in% c("Chinese Taipei (Taiwan)") ~ "Taiwan",
                                    `citizenship` %in% c("St. Lucia") ~ "Saint Lucia",
                                    `citizenship` %in% c("Osttimor") ~ "East Timor",
                                    `citizenship` %in% c("Palästina") ~ "Palestine",
                                    `citizenship` %in% c("Brunei Darussalam") ~ "Brunei",
                                    `citizenship` %in% c("Southern Sudan") ~ "South Sudan",
                                    TRUE ~ `citizenship`
  ),
  birth_country = case_when(`place_of_birth_country` %in% c("England", "Scotland", 
                                                            "Wales", "Northern Ireland",
                                                            "Jersey", "Bermuda", "Gibraltar", 
                                                            "Montserrat", "Isle of Man", "Anguilla") ~ "United Kingdom",
                            `place_of_birth_country` %in% c("Curacao", "Aruba") ~ "Netherlands",
                            `place_of_birth_country` %in% c("Korea, South") ~ "South Korea",
                            `place_of_birth_country` %in% c("DR Congo") ~ "Democratic Republic of the Congo",
                            `place_of_birth_country` %in% c("Cote d'Ivoire") ~ "Ivory Coast",
                            `place_of_birth_country` %in% c("Congo") ~ "Republic of the Congo",
                            `place_of_birth_country` %in% c("Martinique", "Guadeloupe", "French Guiana", 
                                                            "Réunion", "Tahiti", "Neukaledonien") ~ "France",
                            `place_of_birth_country` %in% c("The Gambia") ~ "Gambia",
                            `place_of_birth_country` %in% c("Puerto Rico", "Guam") ~ "United States",
                            `place_of_birth_country` %in% c("Faroe Islands") ~ "Denmark",
                            `place_of_birth_country` %in% c("St. Kitts & Nevis") ~ "Saint Kitts and Nevis",
                            `place_of_birth_country` %in% c("St. Vincent & Grenadinen") ~ "Saint Vincent and the Grenadines",
                            `place_of_birth_country` %in% c("Korea, North") ~ "North Korea",
                            `place_of_birth_country` %in% c("Chinese Taipei (Taiwan)") ~ "Taiwan",
                            `place_of_birth_country` %in% c("St. Lucia") ~ "Saint Lucia",
                            `place_of_birth_country` %in% c("Osttimor") ~ "East Timor",
                            `place_of_birth_country` %in% c("Palästina") ~ "Palestine",
                            `place_of_birth_country` %in% c("Brunei Darussalam") ~ "Brunei",
                            `place_of_birth_country` %in% c("Southern Sudan") ~ "South Sudan",
                            TRUE ~ `place_of_birth_country`
  )
  ) %>%
  left_join(., countries_languages,
            by = c("country_joiner" = "Country")
  )


# Create unique language list for all players.
players_languages = players_languages %>% 
  arrange(anon_player_id, Language, desc(Weight)) %>% 
  group_by(anon_player_id, Language) %>% 
  filter(dplyr::row_number() == 1,
         !is.na(Language)) %>% 
  ungroup() %>% 
  select(anon_player_id, anon_other_player_id, Language, Weight)


write.csv2(players_languages, 
           paste0(data_tidy_created, "anon_player_native_languages.csv"),
           row.names = FALSE)


#################################
## Acquired languages
#################################
# this is not used in the Management Science version eventually, was used in an earlier draft


# Create acquired languages, we define if it is at least the third consecutive half-season for a player in a country.
player_acquired_languages = player_acquired_languages %>% 
  select(anon_other_player_id, anon_player_id, season, season_half, anon_teamid, country_id)


# require *4* season-halves at same league
player_acquired_languages = player_acquired_languages %>% 
  arrange(anon_other_player_id, anon_player_id, season, season_half) %>% 
  group_by(anon_other_player_id, anon_player_id) %>% 
  mutate(acquire_language = case_when(country_id == dplyr::lag(country_id, 1) &
                                        country_id == dplyr::lag(country_id, 2) &
                                        country_id == dplyr::lag(country_id, 3) &
                                        country_id == dplyr::lag(country_id, 4)  ~ 1,
                                      TRUE ~ 0
  )
  ) %>% 
  ungroup() %>% 
  group_by(anon_other_player_id, anon_player_id, country_id) %>% 
  mutate(acquire_language = cummax(acquire_language)) %>% # to handle when leaves country and returns to country
  ungroup()

# Join the languages belonging to the acquired language country.

countries_codes_languages = read.csv2(paste0(data_tidy_created, "countries_codes_languages.csv"))


player_acquired_languages = player_acquired_languages %>%
  mutate(country_id = as.character(country_id)) %>% 
  left_join(., countries_codes_languages %>% mutate(acquire_language = 1),
            by = c("country_id" = "country_id",
                   "acquire_language" = "acquire_language"
            )
  ) %>% 
  select(anon_other_player_id, anon_player_id, season, season_half, anon_teamid, country_id, Language, Weight)


#Now for each player and each season, we create a dataset with the native language, then we row-bind them.
native_skeleton = player_acquired_languages %>%
  ungroup() %>% 
  group_by(anon_other_player_id, anon_player_id, season) %>% 
  summarise(n = 1)


player_native_languages = read.csv2(paste0(data_tidy_created, "anon_player_native_languages.csv"))

player_native_languages = native_skeleton %>% 
  left_join(., player_native_languages %>% select(-anon_player_id, ),
            by = c("anon_other_player_id" = "anon_other_player_id")
  ) %>% 
  select(-n)



player_acquired_languages = player_acquired_languages %>%
  ungroup() %>% 
  group_by(anon_other_player_id, anon_player_id, season, Language) %>% 
  summarise(Weight = max(Weight, na.rm = TRUE))



#################################
# combine native and acquired languages.
#################################
# Note lots of missing obs bc using players outside core data. 

player_languages = player_native_languages %>% 
  rbind(., player_acquired_languages) %>% 
  arrange(anon_other_player_id, anon_player_id, season, Language, desc(Weight)) %>%
  ungroup() %>% 
  group_by(anon_other_player_id, anon_player_id, season, Language) %>% 
  filter(dplyr::row_number() == 1) %>% 
  ungroup()

player_languages = player_languages %>% 
  mutate(Weight = case_when(Weight > -Inf ~ Weight)
  )


player_languages = player_languages %>% 
  pivot_wider(., id_cols = c("anon_other_player_id", "anon_player_id", "season"),
              names_from = "Language", values_from = "Weight")

player_languages = player_languages %>% 
  select(-`NA`)

player_languages[-1:-3][is.na(player_languages[-1:-3])] = 0

# Fill out holes in the histories.

player_languages = player_languages %>%
  arrange(anon_other_player_id, anon_player_id, season) %>% 
  ungroup() %>% 
  group_by(anon_other_player_id, anon_player_id) %>% 
  mutate_at(names(player_languages)[-1:-3], cummax) %>% 
  ungroup()

#clean
rm(native_skeleton, player_native_languages, player_acquired_languages)

#################################
# Save data
#################################


write.csv2(player_languages, 
           paste0(data_tidy_created, "anon_player_total_languages.csv"),
           row.names = FALSE)

rm(player_languages)