#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
# 01_descriptives.R
#
# This version: v1.1 2024-04-10
#######################################################

# This scripts provides tables for the online appendix

################################
# Table 1 Most frequent nationalities
################################
data_player = data_work %>%
  group_by(passer) %>%
  dplyr::summarise(citizenship_main1=first(citizenship_v0_p1),
                   team=first(team)) %>%
  ungroup()


c0=freq(data_player$citizenship_main1, 
        order = "freq", totals=TRUE, rows = 1:18,
        cumul=F, report.nas = F)
print(c0)


################################
#Table 2 Variable types - based on level of aggregation
################################
################################
# dataset, passers
################################

data_player_sh = data_work %>%
  dplyr::group_by(passer, season_name_half) %>%
  dplyr::summarise(pc=sum(pass_count)) %>%
  dplyr::ungroup()
summary(data_player_sh$pc)

# On averarage a passer will have 34 passes to a receiver.
data_player_pair = data_work %>%
  dplyr::group_by(passer, receiver) %>%
  dplyr::summarise(pc=sum(pass_count)) %>%
  dplyr::ungroup()
summary(data_player_pair$pc)


# On average, he passes to 18.07 receivers (ranging from 2 to 35 with median equal to 19). The average pass count from passer to receiver is 16.18 (ranging from 0 to 488 with median equal to 8)
data_player_pair_sh = data_work %>%
  dplyr::group_by(passer, season_name_half) %>%
  dplyr::summarise(pc=n_distinct(receiver)) %>%
  dplyr::ungroup()
summary(data_player_pair_sh$pc)


nrow(data_work) # 669,022
nrow(data_player_sh) # 37,026, 
nrow(data_player_pair)# 310,501 
nrow(data_player) # 6,998


################################
# Figure 1 Distribution of passes
################################

count_hist <-ggplot(data = data_work, aes (x = pass_count, y = (..count..)/sum(..count..))) +
  geom_histogram(color = color.outline, fill = theme_colors[1],
                 size = 0.2, alpha = 0.8,  show.legend=F, na.rm=TRUE,
                 binwidth = 1) +
  labs(x = "Pass count (origin-destination-seasonhalf, truncated at 100)", y = "Share of games (percent)") +
  scale_x_continuous(expand = c(0.001,0.001),limits = c(-1, 100), breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.12), breaks = seq(0,0.12, by = 0.02) ) +
  theme_bg() 
count_hist
save_fig("count_hist", output_appendix, "large")

c1=freq(data_work$pass_count, 
        order = "freq", totals=TRUE, rows = 1:100,
        cumul=F, report.nas = F)
print(c1)



################################
# repeat prep
################################


# define control vars and fixed effects

totalpass ="ln_sh_pass_player_id1 + ln_sh_pass_player_id2"
pass_feat = "ln_avg_pass_length + forwardness"
all_pass  = "ln_sh_pass_player_id1+ln_sh_pass_player_id2"

player_diff ="height_difference +value_diff+same_cit_eu"
fe_pos="passer_position2^receiver_position2"
fe_pos3="passer_position_3G^receiver_position_3G"
player_value = "ln_value_p1 + ln_value_p2"
ctrl = "age_p1_y + age_p2_y+w_club_days_id1 +w_club_days_id2 +height_p1+height_p2+is_loan_id1+is_loan_id2"
fe_pos_ctry= "citizenship_v0_p1^pos3G"

fe0 ="team^half_season"
fe1 ="team^half_season + passer_position^league_half_season + receiver_position^league_half_season + passer_nationality^league_half_season+receiver_nationality^league_half_season"
fe2 = "passer^half_season+receiver^half_season "
tabyl(data_work$passer_position_3G)


# culture
cult="same_cit_any + same_language_only + same_colony_only + same_ex_federation"



# drop FEs rows -- to make ols comparable -- =-few observations are dropped form sample
pois1fe=fepois(pass_count ~ same_cult_any     +.[pass_feat]             | .[fe2] + .[fe_pos] 
               ,offset =~ ln_tau_od , data=data_work)

data_work <- data_work %>% rowid_to_column()
rows_to_delete <- as.data.frame(pois1fe$obs_selection)
rows_to_delete$obsRemoved <- rows_to_delete$obsRemoved * -1
colnames(rows_to_delete) <- c("rowid")
data_work <- anti_join(data_work,rows_to_delete,by="rowid")

nrow(data_work) 
# 668,105

########################################################
# Table 3: Robustness I: Alternative specification
########################################################


#base
pois1fe=fepois(pass_count ~ same_cult_any     +.[pass_feat]             | .[fe2] + .[fe_pos] 
               ,offset =~ ln_tau_od , data=data_work)


# base but allowed 1+ time spent
pois1fe_tsp=fepois(pass_count ~ same_cult_any +.[pass_feat]    + ln_passes_in_shared_mins1   | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1
                   , data=data_work)

# Create filtered dataset for robustness
data1=data_work %>%  filter(passes_in_shared_mins>45                        & pass_count>0                        & passer_position2!="Goalkeeper" & receiver_position2!="Goalkeeper")
pois1fe_subset=fepois(pass_count ~ same_cult_any +.[pass_feat]                          | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 
                      , data=data1)

#ols
ols1fe=feols(ln_pass_permin ~ same_cult_any +.[pass_feat] + ln_passes_in_shared_mins1    | .[fe2] + .[fe_pos] 
             , data=data_work)

etable(pois1fe,pois1fe_tsp, pois1fe_subset, ols1fe,
       cluster =~passer^half_season+receiver^half_season)
       
etable(pois1fe,pois1fe_tsp, pois1fe_subset, ols1fe,
       cluster =~passer^half_season+receiver^half_season,
       #       label="reg-pois-cit-robust_metrics",
       caption = "Results on Robustness",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_mixed_robust, 
      file = paste0(output_appendix,"table-robust_metrics.tex"), replace = TRUE)


########################################################
# Table 4 Selection into play: culture detailed
########################################################


pois1fe_ncl=fepois(pass_count ~ .[cult]       +.[pass_feat]          | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 , data=data_work)
pois2fev2_ncl=fepois(passes_in_shared_mins1 ~ .[cult]                           | .[fe2] + .[fe_pos]  , data=data_work)
pois3fe_ncl=fepois(pass_count ~ .[cult] +.[pass_feat]                          | .[fe2] + .[fe_pos] , data=data_work)

etable(pois1fe_ncl,pois2fev2_ncl,pois3fe_ncl,
       cluster= ~ passer^half_season+receiver^half_season)

etable(pois1fe_ncl,pois2fev2_ncl,pois3fe_ncl,
       cluster= ~ passer^half_season+receiver^half_season,
       caption = "Selection into play",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois_min, 
       file = paste0(output_appendix,"table-culture-minutes.tex"), replace = TRUE)



########################################################
# Table 5: Results on Robustness
########################################################


# repeat for 3 vars
pois1fe_r1_ncl=fepois(pass_count ~ .[cult]                    | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1
                      , data=data_work)
pois1fe_r2_ncl=fepois(pass_count ~ .[cult] +.[pass_feat] +experience_shared1     +.[player_diff]| .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 
                      , data=data_work)
pois1fe_r3_ncl=fepois(pass_count ~ .[cult] +.[pass_feat]    + ln_passes_in_shared_mins1   | .[fe2] + .[fe_pos] 
                      , data=data_work)
ols1fe_ncl=feols(ln_pass_permin ~ .[cult] +.[pass_feat] + ln_passes_in_shared_mins1   | .[fe2] + .[fe_pos] 
                 , data=data_work)
pois1fe_r5_ncl=fepois(pass_count ~ .[cult] +.[pass_feat]                          | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 , data=data1)


etable(pois1fe_r1_ncl, pois1fe_r2_ncl, pois1fe_r3_ncl, ols1fe_ncl, pois1fe_r5_ncl,
       cluster= ~ passer^half_season+receiver^half_season)
       
etable(pois1fe_r1_ncl, pois1fe_r2_ncl, pois1fe_r3_ncl, ols1fe_ncl, pois1fe_r5_ncl,
       cluster= ~ passer^half_season+receiver^half_season,
       caption = "Results on Robustness",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois1, 
       file = paste0(output_appendix,"table-metrics-robustness.tex"), replace = TRUE)


########################################################
# Table 6 Heterogeneity by passer: age, experience, group size
########################################################


# does age matter?
table(data_work$age_p1_y_bin2)
pois1fe_p1_age=fepois(pass_count ~ same_cult_any*age_p1_y_bin2 +.[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work,
)



# cutoff at 4
# same culture higher for passers belonging to larger groups?  
pois1fe_cult_c4 = fepois(pass_count ~ same_cult_any*culture_group_large_p1_c4 
                         + .[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od, data=data_work)

# top players
pois1fe_p2_ln_value=fepois(pass_count ~ same_cult_any*ln_value_p2_bin_top2 +
                             .[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work)

etable(pois1fe_p1_age, pois1fe_cult_c4,pois1fe_p2_ln_value,    
       cluster= ~ passer^half_season+receiver^half_season)

# combined table
etable(pois1fe_p1_age, pois1fe_cult_c4,pois1fe_p2_ln_value,    
       cluster= ~ passer^half_season+receiver^half_season,
       label="reg_table_heterogeneity",
       caption = "Heterogeneity by age, group size and receiver",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois_heterogeneity, 
      file = paste0(output_appendix,"reg_table_heterogeneity.tex"), replace = TRUE)


########################################################
# Table 7 All pass sequences
########################################################


pois1fe_seq1=fepois(pass_count_sequences ~ same_cult_any +.[pass_feat]  | .[fe2] + .[fe_pos] ,offset =~  ln_tau_od , 
                    data=data_work)
pois1fe_seq2=fepois(pass_count_complex ~ same_cult_any +.[pass_feat]  | .[fe2] + .[fe_pos] ,offset =~  ln_tau_od , 
                    data=data_work)
pois1fe_seq1_fwd=fepois(pass_count_forward_sequences ~ same_cult_any +.[pass_feat]  | .[fe2] + .[fe_pos] ,offset =~  ln_tau_od , 
                        data=data_work)
pois1fe_seq2_fwd=fepois(pass_count_forward_complex ~ same_cult_any +.[pass_feat]  | .[fe2] + .[fe_pos] ,offset =~  ln_tau_od , 
                        data=data_work)

etable(pois1fe_seq1,pois1fe_seq2,pois1fe_seq1_fwd,pois1fe_seq2_fwd,
       cluster= ~ passer^half_season+receiver^half_season)
       
etable(pois1fe_seq1,pois1fe_seq2,pois1fe_seq1_fwd,pois1fe_seq2_fwd,
       cluster= ~ passer^half_season+receiver^half_season ,
       caption = "Pass sequences and complicated pass sequences",
       label ="tab:reg-pois-seq",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois_seq, 
       file = paste0(output_appendix,"reg-pois-seq.tex"), replace = TRUE)
       



########################################################
# Table 8 Team level performance and passes
########################################################


# Regression on aggregate data: points on passes

# in: 
# anon_teams_aggregated.parquet

# out: 
# output/reg-team-passes.tex
#######################################################

# reads in work data, aggregated as season-half level
#  anon_all_events_season-half_teams_major.parquet

teams_season_half_aggregated= read_parquet(file = paste0(data_tidy, "anon_teams_aggregated.parquet"),
                                           as_data_frame = TRUE)

tabyl(teams_season_half_aggregated$season_name_half)

teams_season_half_aggregated %<>%
  mutate(ln_points_per_game=log(points_per_game),
         ln_points_per_game=ifelse(points_per_game==0,0,ln_points_per_game)
  )

teams_season_half_aggregated1=teams_season_half_aggregated %>%
  filter(season_name_half=="2015-2016_h1")


points1=feols(points_per_game  ~  ln_pass_count_per_game |  league_season  
              ,data=teams_season_half_aggregated1)
points2=feols(points_per_game  ~  ln_pass_count_per_game |  league_season^season_half +anon_teamid
              ,data=teams_season_half_aggregated)
points3=feols(ln_points_per_game  ~  ln_pass_count_per_game |  league_season^season_half +anon_teamid
              ,data=teams_season_half_aggregated)
points4=feols(ln_points_per_game  ~  ln_pass_count_per_game + ln_value_p1|  league_season^season_half +anon_teamid
              ,data=teams_season_half_aggregated)

etable(points1, points2, points3, points4, 
       cluster = ~ league_season^season_half)
etable(points1, points2, points3, points4, 
       cluster = ~ league_season^season_half,
       caption = "Team level performance and passes",
       tablenotes = notes_teams1, 
       label ="tab:reg-team-passes",
      file = paste0(output_appendix,"reg-team-passes-ext.tex"), replace = TRUE)

