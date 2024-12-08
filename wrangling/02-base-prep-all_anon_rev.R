
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# 02_base_prep_all_anon_rev.R:

# Creates all player and team level datasets using the raw data that had been only anonymized (player id, team id)
# uses publicly shared data

# runtime warning: takes a few hours

# Scripts by Bence Szabó, Gábor Békés with thanks to Endre Borza (2021-2024) 
# This version: v2.1: 2024-11-18
#######################################################



# country corrections
source(paste0("wrangling/02_base_prep/","d0a_country_group_categories.R"))

##################################
##################################

# part 1
source(paste0("wrangling/02_base_prep/","d02_01_player-maker-anon.R"))             
source(paste0("wrangling/02_base_prep/","d02_02_player_careers_anon.R"))             
source(paste0("wrangling/02_base_prep/","d02_03_player_languages_anon.R"))             
source(paste0("wrangling/02_base_prep/","d02_04_countries_cult_dist_anon.R"))             
source(paste0("wrangling/02_base_prep/","d02_05_players_from_EU_anon.R"))             

source(paste0("wrangling/02_base_prep/","d02_06_matches-maker_anon.R"))           

source(paste0("wrangling/02_base_prep/","d02_07_simpson_anon.R"))

# part 2
source(paste0("wrangling/02_base_prep/","d02_08_shared_past_anon.R"))
source(paste0("wrangling/02_base_prep/","d02_09_seasonhalf_events_anon.R"))
source(paste0("wrangling/02_base_prep/","d02_10_seasonhalf_static_anon.R"))


    

