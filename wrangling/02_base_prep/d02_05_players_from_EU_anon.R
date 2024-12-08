
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d02_05_players_from_EU_anon.R
# Create a variable that indicates if a player is from the EU

# This version: v2.1: 2024-11-18
#######################################################



# load data
team_info = read_parquet(paste0(data_tidy,"anon_team_info.parquet"))

MV_transf_joint = read_parquet(paste0(data_tidy_created,"anon_MV_transf_joint.parquet"))

players = read_parquet(paste0(data_tidy,"anon_players.parquet"))




# # Definitions:
# 
# ## EU standard
# 
# This set of countries will belong to the eu category
# 
#  - EU
#  - UK
#  - EEA countries
#  - Cotonou agreement countries
# 
# ## EU France version
# 
#  - EU standard
#  - Uzbekistan, Kyrgyzstan
#  - all African countries, except Egypt
#  - all Oceania countries except New Zealand
#  - Kosovo, Albania, Macedonia
# 
# ## EU Spain version
# 
#  - EU standard
#  - Russia, Turkey
#  

tabyl(players$`citizenship-0`)


eu_extra_france = c("Uzbekistan", "Kyrgyzstan", Africa, Oceania,
                    "Kosovo", "Albania", "Macedonia"
                    )

eu_extra_spain = c("Russia", "Turkey")
eu_maybe_spain = CS_America_esp

eu_standard = c(core_eu, eea, cotonou)


citizenships = players %>% distinct(`citizenship-1`) %>% pull()

citizenships[!(citizenships %in% c(Africa, CS_America_esp, eu_standard))]


MV_transf_joint = MV_transf_joint %>% 
    select(anon_other_player_id, anon_player_id, season, season_half, anon_teamid) %>% 
    left_join(team_info %>% select(anon_teamid, country),
              by = c("anon_teamid" = "anon_teamid")
              ) %>% 
    left_join(players %>% select(anon_other_player_id, place_of_birth_country,
                                 `citizenship-0`, `citizenship-1`
                                 ),
              by = c("anon_other_player_id" = "anon_other_player_id")
              ) 


MV_transf_joint = MV_transf_joint %>%
    mutate(counts_as_from_EU = case_when(
        ((place_of_birth_country %in% eu_standard) | 
             (`citizenship-0` %in% eu_standard) | 
             (`citizenship-1` %in% eu_standard) 
             ) ~ "yes",
        country == "Spain" & 
            ((place_of_birth_country %in% eu_extra_spain) | 
                 (`citizenship-0` %in% eu_extra_spain) | 
                 (`citizenship-1` %in% eu_extra_spain)
             ) ~ "yes",
        country == "Spain" &
            ((place_of_birth_country %in% eu_maybe_spain) | 
                 (`citizenship-0` %in% eu_maybe_spain) |
                 (`citizenship-1` %in% eu_maybe_spain)
             ) ~ "maybe",
        country == "France" & 
            ((place_of_birth_country %in% eu_extra_france) | 
                 (`citizenship-0` %in% eu_extra_france) | 
                 (`citizenship-1` %in% eu_extra_france)
             ) ~ "yes",
        TRUE ~ "no"
        )
        )




# Other groups ----------------------


# need to check Macededonia name
yugoslavia =c("Croatia","Slovenia", "Kosovo", "Serbia", "North Macedonia", "Montenegro", "Bosnia-Herzegovina")
ussr = c("Belarus", "Russia", "Ukraine",
         "Turkmenistan", "Uzbekistan", "Kazakhstan", "Kyrgyzstan", "Azerbaijan", "Armenia," ,"Georgia",
         "Moldova", "Estonia", "Latvia", "Lithuania" )
czsk = c("Slovakia", "Czech Republic")
irish = c("Ireland", "Northern Ireland")
uk = c("England", "Wales","Scotland" , "Jersey", "Gibraltar" ,  "Isle of Man", "Northern Ireland" )
gb= c("England", "Wales","Scotland", "Isle of Man")

# add additional flags
MV_transf_joint = MV_transf_joint %>%
    mutate(
        counts_as_yugoslavia = case_when(
        ((place_of_birth_country %in% yugoslavia) | 
             (`citizenship-0` %in% yugoslavia) | 
             (`citizenship-1` %in% yugoslavia) ) ~ "yes",
        TRUE ~ "no"),
        
            counts_as_ussr = case_when(
                ((place_of_birth_country %in% ussr) | 
                     (`citizenship-0` %in% ussr) | 
                     (`citizenship-1` %in% ussr) ) ~ "yes",
                TRUE ~ "no")
        ,
        
        counts_as_czsk = case_when(
            ((place_of_birth_country %in% czsk) | 
                 (`citizenship-0` %in% czsk) | 
                 (`citizenship-1` %in% czsk) ) ~ "yes",
            TRUE ~ "no")
        )

tabyl(MV_transf_joint$counts_as_ussr)


MV_transf_joint = MV_transf_joint %>%
    mutate(
        counts_as_irish = case_when(
            ((place_of_birth_country %in% irish) | 
                 (`citizenship-0` %in% irish) | 
                 (`citizenship-1` %in% irish) ) ~ "yes",
            TRUE ~ "no"),
        
        counts_as_gb = case_when(
            ((place_of_birth_country %in% gb) | 
                 (`citizenship-0` %in% gb) | 
                 (`citizenship-1` %in% gb) ) ~ "yes",
            TRUE ~ "no")
        ,
        
        counts_as_uk = case_when(
            ((place_of_birth_country %in% uk) | 
                 (`citizenship-0` %in% uk) | 
                 (`citizenship-1` %in% uk) ) ~ "yes",
            TRUE ~ "no")
    )

tabyl(MV_transf_joint$counts_as_irish)


# Save the dataset ----------------------

write_parquet(MV_transf_joint, 
              paste0(data_tidy_created,"anon_is_player_from_EU.parquet"))

#drop the data

rm(MV_transf_joint, team_info, players, 
   players_languages, countries_codes, countries_languages, countries_codes_languages)
