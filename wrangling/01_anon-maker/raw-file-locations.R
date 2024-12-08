
#######################
# v3 raw data files and libraries
#######################

data_wh     <- paste(data_raw,"t2_raw/wh", sep = "/")
data_tm     <- paste(data_raw,"t2_raw/tm", sep = "/")
data_corefs <- paste(data_raw,"corefs", sep = "/")
data_t3     <- paste(data_raw,"t3", sep = "/")
data_events <- paste(data_raw,"passes/", sep = "/")

################################
### MAIN RAW FILES as strings!
################################

# corefs
players_coref.parquet       = paste(data_corefs,"players_coref.parquet",sep="/")

# t3/
lineups.parquet       = paste(data_t3,"player_fixtures.parquet",sep="/") #player_fixtures / match_participation.parquet#
team_fixtures.parquet     = paste(data_t3,"team_fixtures.parquet",sep="/")   #team_info#

# t2_raw/wf/
formations.parquet    = paste(data_wh,"formations.parquet",sep="/") #formations#
team_info.parquet_wh     = paste(data_wh,"teams.parquet",sep="/")  #teams#
matches.parquet       = paste(data_wh,"matches.parquet",sep="/") 
seasons.parquet       = paste(data_wh,"seasons.parquet",sep="/")
formation_use.parquet = paste(data_wh,"formation_use.parquet",sep="/")

# t2_raw/tm/
player_transfers.parquet = paste(data_tm,"player_transfers.parquet",sep="/")
player_values.parquet    = paste(data_tm,"player_values.parquet",sep="/") #market_values.parquet#
team_relations.parquet   = paste(data_tm,"team_relations.parquet",sep="/")
player_info.parquet      = paste(data_tm,"player_info.parquet",sep="/")  
team_info.parquet        = paste(data_tm,"team_info.parquet",sep="/")  

