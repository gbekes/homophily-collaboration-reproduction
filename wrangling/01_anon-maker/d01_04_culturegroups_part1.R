
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#
# d01_04_culturegroups_part1.R

# Create groups of cultures for each player
# Raw data used. 

# part 1

# This version: v2.1: 2024-11-18
#######################################################



# load tables
player_cultures_both = read_parquet(paste0(data_tidy_temp,"players.parquet"))
matches_info = read_csv(paste0(data_tidy,"matches_info.csv"))

## player cultures
player_cultures_both = player_cultures_both %>% 
  select(wh_player_id, tm_player_id
         , citizenship_v0 = `citizenship-0`, 
         citizenship_v1 =`citizenship-1`)  %>% 
  mutate(citizenship_v0 = case_when(`citizenship_v0` %in% c("England", "Scotland", 
                                                            "Wales", "Northern Ireland",
                                                            "Jersey", "Bermuda", "Gibraltar", 
                                                            "Montserrat", "Isle of Man", "Anguilla") ~ "United Kingdom",
                                    `citizenship_v0` %in% c("Curacao", "Aruba") ~ "Netherlands",
                                    `citizenship_v0` %in% c("Korea, South") ~ "South Korea",
                                    `citizenship_v0` %in% c("DR Congo") ~ "Democratic Republic of the Congo",
                                    `citizenship_v0` %in% c("Cote d'Ivoire") ~ "Ivory Coast",
                                    `citizenship_v0` %in% c("Congo") ~ "Republic of the Congo",
                                    `citizenship_v0` %in% c("Martinique", "Guadeloupe", "French Guiana", 
                                                            "Réunion", "Tahiti", "Neukaledonien") ~ "France",
                                    `citizenship_v0` %in% c("The Gambia") ~ "Gambia",
                                    `citizenship_v0` %in% c("Puerto Rico", "Guam") ~ "United States",
                                    `citizenship_v0` %in% c("Faroe Islands") ~ "Denmark",
                                    `citizenship_v0` %in% c("St. Kitts & Nevis") ~ "Saint Kitts and Nevis",
                                    `citizenship_v0` %in% c("St. Vincent & Grenadinen") ~ "Saint Vincent and the Grenadines",
                                    `citizenship_v0` %in% c("Korea, North") ~ "North Korea",
                                    `citizenship_v0` %in% c("Chinese Taipei (Taiwan)") ~ "Taiwan",
                                    `citizenship_v0` %in% c("St. Lucia") ~ "Saint Lucia",
                                    `citizenship_v0` %in% c("Osttimor") ~ "East Timor",
                                    `citizenship_v0` %in% c("Palästina") ~ "Palestine",
                                    `citizenship_v0` %in% c("Brunei Darussalam") ~ "Brunei",
                                    `citizenship_v0` %in% c("Southern Sudan") ~ "South Sudan",
                                    TRUE ~ `citizenship_v0`
  ),
  citizenship_v1 = case_when(`citizenship_v1` %in% c("England", "Scotland", 
                                                     "Wales", "Northern Ireland",
                                                     "Jersey", "Bermuda", "Gibraltar", 
                                                     "Montserrat", "Isle of Man", "Anguilla") ~ "United Kingdom",
                             `citizenship_v1` %in% c("Curacao", "Aruba") ~ "Netherlands",
                             `citizenship_v1` %in% c("Korea, South") ~ "South Korea",
                             `citizenship_v1` %in% c("DR Congo") ~ "Democratic Republic of the Congo",
                             `citizenship_v1` %in% c("Cote d'Ivoire") ~ "Ivory Coast",
                             `citizenship_v1` %in% c("Congo") ~ "Republic of the Congo",
                             `citizenship_v1` %in% c("Martinique", "Guadeloupe", "French Guiana", 
                                                     "Réunion", "Tahiti", "Neukaledonien") ~ "France",
                             `citizenship_v1` %in% c("The Gambia") ~ "Gambia",
                             `citizenship_v1` %in% c("Puerto Rico", "Guam") ~ "United States",
                             `citizenship_v1` %in% c("Faroe Islands") ~ "Denmark",
                             `citizenship_v1` %in% c("St. Kitts & Nevis") ~ "Saint Kitts and Nevis",
                             `citizenship_v1` %in% c("St. Vincent & Grenadinen") ~ "Saint Vincent and the Grenadines",
                             `citizenship_v1` %in% c("Korea, North") ~ "North Korea",
                             `citizenship_v1` %in% c("Chinese Taipei (Taiwan)") ~ "Taiwan",
                             `citizenship_v1` %in% c("St. Lucia") ~ "Saint Lucia",
                             `citizenship_v1` %in% c("Osttimor") ~ "East Timor",
                             `citizenship_v1` %in% c("Palästina") ~ "Palestine",
                             `citizenship_v1` %in% c("Brunei Darussalam") ~ "Brunei",
                             `citizenship_v1` %in% c("Southern Sudan") ~ "South Sudan",
                             TRUE ~ `citizenship_v1`
  )
  )



######### Group these countries
country_culture_all = readxl::read_excel(paste0(data_imported,"/country_culture_all.xlsx")) %>%
  filter(if_else(is.na(not_culture_group), 0, not_culture_group) != 1)


## for primary citizenship
player_cultures_both = player_cultures_both %>% 
  left_join(.,
            country_culture_all,
            by = c("citizenship_v0" = "country")
  )

player_cultures_both = player_cultures_both %>%
  select(wh_player_id, citizenship_v0, citizenship_v1,
         culture_any_v0 = culture_any
  ) %>% 
  group_by(wh_player_id) %>% 
  mutate(culture_counter = row_number()) %>% 
  ungroup()

player_cultures_both = player_cultures_both %>%
  pivot_wider(., id_cols = c("wh_player_id", "citizenship_v0", 
                             "citizenship_v1"),
              names_from = "culture_counter",
              values_from = "culture_any_v0", 
              names_prefix = "culture_any_v0_"
  )


## for secondary citizenship
player_cultures_both = player_cultures_both %>% 
  left_join(.,
            country_culture_all,
            by = c("citizenship_v1" = "country")
  )

player_cultures_both = player_cultures_both %>%
  select(wh_player_id, citizenship_v0, citizenship_v1, culture_any_v0_1,
         culture_any_v0_2,
         culture_any_v1 = culture_any
  ) %>% 
  group_by(wh_player_id) %>% 
  mutate(culture_counter = row_number()) %>% 
  ungroup()

player_cultures_both = player_cultures_both %>%
  pivot_wider(., id_cols = c("wh_player_id", "citizenship_v0", 
                             "citizenship_v1",
                             "culture_any_v0_1",
                             "culture_any_v0_2"
  ),
  names_from = "culture_counter",
  values_from = "culture_any_v1", 
  names_prefix = "culture_any_v1_"
  )


#### create the player-culture table
player_cultures_both = player_cultures_both %>% 
  mutate(culture_counter = 4 - (is.na(culture_any_v0_1) + is.na(culture_any_v0_2) +
                                  is.na(culture_any_v1_1) + is.na(culture_any_v1_2))
  ) %>% 
  rename(culture_all1 = culture_any_v0_1,
         culture_all2 = culture_any_v0_2,
         culture_all3 = culture_any_v1_1,
         culture_all4 = culture_any_v1_2
  ) %>% 
  pivot_longer(cols = starts_with("culture_all"), 
               names_to = "culture_rank",
               values_to = "culture_all",
               names_prefix = "culture_all") %>% 
  filter(!is.na(culture_all)) 



## Clear of duplicate cultures
player_cultures_both = player_cultures_both %>% 
  group_by(wh_player_id, culture_all) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# ## Case 1: not more than 2 culture groups
# fewculturals = player_cultures_both %>% 
#     group_by(wh_player_id) %>%
#     mutate(maxronum = max(row_number())) %>% 
#     filter(maxronum <= 3) %>% 
#     ungroup()
# 
# ## Case 2:at least 2 culture groups
# moreculturals = player_cultures_both %>% 
#     group_by(wh_player_id) %>%
#     mutate(maxronum = max(row_number())) %>% 
#     filter(maxronum > 3) %>% 
#     ungroup()
# 
# set.seed(7891)
# 
# moreculturals = moreculturals %>%
#   group_by(wh_player_id) %>% 
#   sample_n(., 3L, replace = FALSE) %>% 
#   ungroup()
# 
# player_cultures_both = rbind(fewculturals, moreculturals)
# rm(fewculturals, moreculturals)

player_cultures_both = player_cultures_both %>%
  group_by(wh_player_id) %>% 
  mutate(new_culture_rank = row_number()) %>% 
  ungroup() %>% 
  select(-culture_rank) %>% 
  pivot_wider(.,
              id_cols = c("wh_player_id",
                          "citizenship_v0",
                          "citizenship_v1",
                          "culture_counter"
              ),
              names_from = "new_culture_rank",
              values_from = "culture_all",
              names_prefix = "culture_all"
  )

write_parquet(player_cultures_both, paste0(data_tidy_temp, "player_cultures_both.parquet"))


# new important descriptives
tabyl(player_cultures_both$culture_all2)

# more people have double culture because of bi-cultural countries
tabyl(player_cultures_both$citizenship_v1) 

# delete the tables
rm(player_cultures_both, country_culture_all, matches_info)