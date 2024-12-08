
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#
# 01-raw-to-anon-maker_rev.R
# Creates all player and team level datasets using the raw data that had been only anonymized (player id, team id)


# runtime warning: takes a few hours

# Scripts by Bence Szabó, Gábor Békés with thanks to Endre Borza (2021-2024) 
# This version: v1.1: 2024-11-14
#######################################################


# setup
anon_tables      <- paste(data_hub,"raw/anon-tables/", sep = "/")
data_tidy_temp   <- paste(data_hub,"raw/data_tidy_temp/", sep = "/")

# source files location
source(paste0("wrangling/01_anon-maker/","raw-file-locations.R"))


##################################
# events to pass data
##################################

source(paste0("wrangling/01_anon-maker/","d01_01_match_passes.R"))             
source(paste0("wrangling/01_anon-maker/","d01_02_matches-maker.R"))
source(paste0("wrangling/01_anon-maker/","d01_03_pairwise_pass_count_shared_minutes.R"))             
source(paste0("wrangling/01_anon-maker/","d01_04_culturegroups_part1.R"))             
source(paste0("wrangling/01_anon-maker/","d01_05_culturegroups_part2.R"))             

##################################
# anonymze
##################################
# sometimes there is an error in anon_table vs anon-table, check. 

# create _anon versions
source(paste0("wrangling/01_anon-maker/","d01_dx_anonymization-id-generator.R"))
source(paste0("wrangling/01_anon-maker/","d01_dx_anonymization-raw_parquet.R"))

