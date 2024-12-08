
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d02_08_shared_past_anon.R

# players' shared past
# run time warning: 10-60 mins

# This version: v2.1: 2024-11-18
#######################################################

# In this script we are going to create the following variables:
# 
#     shared_time_hs-count = count, the number of half-seasons (incl current) the two players played before
#     shared_time_team-count = count, the number of teams (inc current) the two players played together
#     shared_time_youth-binary = indicator, =1 if two players ever played together in any youth team
#     shared_time_ever_youth-binary = indicator, =1 if two players ever played in any youth time, incl when not at the same time. 
#     


## load data
MV_transf_joint = read_parquet(paste0(data_tidy_created,"anon_MV_transf_joint.parquet"))
MV_transf_joint %<>% ungroup()
skim(MV_transf_joint)

team_relations = read_parquet(paste0(data_tidy,"anon_team_relations.parquet"))

team_info = read_parquet(paste0(data_tidy,"anon_team_info.parquet"))

## join info on youth teams
# team info
team_info = team_info %>% 
    select(anon_teamid, country) %>% 
    mutate(anon_teamid = as.character(anon_teamid))

child_groups = team_relations %>% 
    group_by(anon_child_team_id) %>% 
    summarise(group_id = min(anon_parent_team_id))

youth_teams = team_relations %>% 
    left_join(.,
              child_groups,
              by = "anon_child_team_id"
              )

# create player groups
anon_other_player_id_list = MV_transf_joint %>% distinct(anon_other_player_id)

anon_other_player_id_list

splitted = split(anon_other_player_id_list, 1:16)

# empty dataset
varlist = c("anon_other_player1", "anon_other_player2", "season", "season_half", "shared_time_hs_count", "shared_teams_count")
df_shared = data.frame(1:length(varlist)) %>% t() %>% as.data.frame()
names(df_shared) = varlist

df_shared = df_shared %>% 
    filter(anon_other_player1 != 1)

# we go by players
for(i in 1:length(splitted)){
    # dataframe
    df_tmp = splitted[[i]] %>% 
        left_join(.,
                  MV_transf_joint %>% 
                      select(anon_other_player_id, season, season_half, anon_teamid),
                  by = c("anon_other_player_id")
                  ) %>% 
        left_join(.,
                  MV_transf_joint %>% 
                      select(anon_other_player_id, season, season_half, anon_teamid),
                  by = c("anon_teamid", "season", "season_half")
        ) %>% 
    rename(anon_other_player1 = anon_other_player_id.x,
           anon_other_player2 = anon_other_player_id.y)
        
    df_tmp = df_tmp %>% 
        filter(anon_other_player1 != anon_other_player2)
    
    df_tmp = df_tmp %>% 
        mutate(counter = 1) %>%
        arrange(anon_other_player1, anon_other_player2, season, season_half) %>% 
        mutate(team_counter = case_when(anon_teamid != dplyr::lag(anon_teamid) & 
                                            anon_other_player2 == dplyr::lag(anon_other_player2) ~ 1,
                                        TRUE ~ 0)
               ) %>%
        arrange(anon_other_player1, season, season_half, anon_other_player2) %>% 
        group_by(anon_other_player1, anon_other_player2) %>% 
        mutate(shared_time_hs_count = cumsum(counter),
               shared_teams_count = 1 + cumsum(team_counter)) %>% 
        ungroup()
    
    df_tmp = df_tmp %>% 
      filter(season>2010)
    
    
    df_shared = rbind(df_shared,
                      df_tmp %>% select(all_of(varlist))
                      )
    
    print(i)
    nrow(df_shared)
}

# checks
df_tmp1 = df_tmp 
df_tmp1 %<>% 
  filter(anon_other_player1<1000)

tabyl(df_shared$shared_teams_count)

# save data
write_parquet(df_shared, paste0(data_tidy_created,"anon_df_shared_time_teams.parquet"))

rm(MV_transf_joint, team_relations, team_info, child_groups, youth_teams, anon_other_player_id_list, splitted, varlist, df_shared, df_tmp, df_tmp1)

