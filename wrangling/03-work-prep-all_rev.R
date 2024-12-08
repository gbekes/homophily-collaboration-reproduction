
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# 03_work_prep_all_rev.R

# creates the work dataset for the analysis

# Scripts by Bence Szabó, Gábor Békés with thanks to Endre Borza (2021-2024) 
# This version: v2.1: 2024-11-18
#######################################################



# wrangling
source(paste0("wrangling/03_work-prep/","d03_01-analysis_table.R"))
source(paste0("wrangling/03_work-prep/","d03_02-player_lang_combo_rev.R"))
source(paste0("wrangling/03_work-prep/","d03_03-language_join_mansci.R"))

# creates key vars, themes
source(paste0("wrangling/03_work-prep/","d03_04-variables_mansci.R"))

# creates team level aggregates
source(paste0("wrangling/03_work-prep/","d03_05-match_level_mansci.R"))
