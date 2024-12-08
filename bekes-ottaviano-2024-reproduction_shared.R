
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# Runs all wrangling and analysis code
# must run wrangling once

# runtime warning: takes several hours

# Scripts by Bence Szabó, Gábor Békés with thanks to Endre Borza (2021-2024) 
# This version: v2.5: 2024-11-18
#######################################################



################################################x
# Part I Raw data to anonynmized data tables
################################################x

# data here is NOT publicly shared but can be accessed via reproducibility requests. 
# journal data editors were granted access. 

# setup
# source(paste0("wrangling/","d00_setup.R"))             # data_dir must be first defined 
# source(paste0("wrangling/","d01_paths.R"))             # data_dir must be first defined 

# this was run to create anonymized data files
# source(paste0("wrangling/","01-raw-to-anon-maker_rev.R"))

# rm(list=ls()) # may start a new session instead #
# gc()

################################################x
# Part II anonynmized data table wrangling
################################################x

# This data is publicly available 

# setup

source(paste0("wrangling/","d00_setup.R"))             # data_dir must be first defined 
source(paste0("wrangling/","d01_paths.R"))             # data_dir must be first defined 

source(paste0("wrangling/","02-base-prep-all_anon_rev.R"))

###################################################
# Part III creating the work data tables
################################################x

rm(list=ls()) # Ideally start a NEW SESSION instead 
gc()

source(paste0("wrangling/","d00_setup.R"))             # data_dir must be first defined 
source(paste0("wrangling/","d01_paths.R"))             # data_dir must be first defined 

source(paste0("wrangling/","03-work-prep-all_rev.R"))

#######################################
#######################################
# Part IV  analysis to reproduce the paper
#######################################
#######################################

rm(list=ls()) # Ideally start a NEW SESSION instead 
gc()

source(paste0("wrangling/","d00_setup.R"))             # data_dir must be first defined 
source(paste0("wrangling/","d01_paths.R"))             # data_dir must be first defined 

data_tidy = data_work # the main folder for data is now work-data!


# prep graphs, tables
source(paste0("wrangling/","theme_bg.R"))             
source(paste0("wrangling/","stat_binscatter.R"))
source(paste0("analysis/","fixest-settings.R"))             

# reads in work data, aggregated as season-half level, directed pairs
data_work <- readRDS(paste0(data_tidy,"anon_work-regressions.RData"))

# reproduce graphs and tables
source(paste0("analysis/","01_descriptives.R"))
source(paste0("analysis/","02_regressions_mansci.R"))

#drop the data
rm(data_exp1, data_exp2, data_exp3, data_player, data_player_pair, data_player_sh, data_player_pair_sh,   data_work, data_work_gs, dissecting_total, fig1, grouplist)
rm(list=ls(pattern = "^pois"))

data_work <- readRDS(paste0(data_tidy,"anon_work-regressions.RData"))
source(paste0("analysis/","03_online_appendix_mansci.R"))
