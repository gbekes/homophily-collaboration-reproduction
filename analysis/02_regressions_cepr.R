
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
# 02_regressions.R
#
# This version: 2023-07-03
#
#######################################################


#######################################################
# Regression on aggregate data: points on passes

# in: 
# anon_work-regressions.RData

#######################################################


# reads in work data, aggregated as season-half level, directed pairs

# if not loaded already
load(file=paste0(data_tidy,"anon_work-regressions.RData"))
source(paste0("analysis/","fixest-settings.R"))             


###########################################
# define control vars and fixed effects
###########################################

totalpass ="ln_sh_pass_player_id1 + ln_sh_pass_player_id2"
pass_feat = "ln_avg_pass_length+ forwardness"
all_pass  = "ln_sh_pass_player_id1+ln_sh_pass_player_id2"

player_diff ="height_difference +value_diff+same_cit_eu"
fe_pos="passer_position2^receiver_position2"
fe_pos3="passer_position_3G^receiver_position_3G"
player_value = "ln_value_p1 + ln_value_p2"
ctrl = "age_p1_y + age_p2_y+w_club_days_id1 +w_club_days_id2 +height_p1+height_p2+is_loan_id1+is_loan_id2"
fe_pos_ctry= "citizenship_v0_p1^pos3G"

fe0 ="team^half_season"
fe2 = "passer^half_season+receiver^half_season "
tabyl(data_work$passer_position_3G)


# culture
cult="same_cit_any + same_language_only + same_colony_only + same_ex_federation"




###############################################
################################################
# MAIN TABLES
###############################################
################################################


###############################################
# Table 3
# \input{tables/reg-pois-cult-base}
###############################################


# naive with tau
pois1_theory1tau=fepois(pass_count ~ same_cult_any +ln_total_pass_by_passer_sh + ln_total_reception_by_receiver_sh   +.[pass_feat]    | .[fe0] +  .[fe_pos]
                        ,offset =~ ln_tau_od  , data=data_work)
# FE
pois1fe=fepois(pass_count ~ same_cult_any     +.[pass_feat]             | .[fe2] + .[fe_pos] 
               ,offset =~ ln_tau_od , data=data_work)

etable(pois1_theory1tau, pois1fe,
       cluster= ~ passer^half_season+receiver^half_season)
       

etable(pois1_theory1tau, pois1fe,
       cluster= ~ passer^half_season+receiver^half_season ,
       caption = "Baseline results",
       tablenotes = note_pois, 
       label ="tab:reg-pois-cult-base",
       file = "output/reg-pois-cult-base.tex", replace = TRUE)



###############################################
# Table 4
# \input{tables/reg-pois-cit-uncond}
###############################################

# pois1fe same as before
pois0=fepois(pass_count ~ same_cult_any                                          | .[fe0], data=data_work)
pois1fe_time=fepois(pass_count ~ same_cult_any     +.[pass_feat]               | .[fe2] + .[fe_pos] , data=data_work)

etable(pois0, pois1fe_time, pois1fe,
       cluster= ~ passer^half_season+receiver^half_season )
       
etable(pois0, pois1fe_time, pois1fe,
       cluster= ~ passer^half_season+receiver^half_season ,
       caption = "Total and choice homophily",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois0, 
       drop =c("age_p1_y", "age_p2_y", "w_club_days_id1" ,"w_club_days_id2","height_p1", "height_p2", "is_loan_id1", "is_loan_id2",
               "ln_avg_pass_length", "forwardness"),
       file = "output/reg-pois-cit-uncond.tex", replace = TRUE)

############################
# Figure 3
############################
coeff_total = round(pois0$coefficients["same_cult_any"],4)
c_choice = round(pois1fe$coefficients["same_cult_any"],4)
c_coach= round(pois1fe_time$coefficients["same_cult_any"]-pois1fe$coefficients["same_cult_any"],4)
c_induced = round(pois0$coefficients["same_cult_any"]-pois1fe_time$coefficients["same_cult_any"],4)


fig3 <- data.frame(
  var= c("choice homophily", "mix by coach decisions", "induced: sorting, selection"),
  coeff = c(c_choice, c_coach, c_induced),
  type = "A", "A", "A") %>% 
  mutate(total = coeff_total,
         var = factor(var, levels = c("choice homophily", "mix by coach decisions", "induced: sorting, selection"))
  )


dissecting_total = ggplot(fig3, aes(fill=var, y=coeff, x=type )) + 
  geom_col(position = position_stack(reverse = TRUE), alpha=0.95) +
  scale_fill_manual(values = c(color[3], color[1], color[5]), name = NULL) +
  labs(y = "Homophily bias (coefficients)", x = "") +
  theme_bg() +
  scale_y_continuous(expand=c(0,0), limits=c(0, 0.07), breaks = seq(0, 0.07, by = 0.01),
                     minor_breaks = seq(0, 0.07, by=0.005), labels = scales::percent) +
  coord_flip() +
  theme(legend.position = "bottom", axis.ticks.y = element_blank(), axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), legend.text=element_text(size=6)
  ) +
  geom_text(aes(label = paste0(coeff*100, "%")), 
            size = 3, position = position_stack(vjust = 0.5, reverse = TRUE))  +
  geom_text(aes(x = type, y = total, 
                label = paste0(total*100, "%"), fill = NULL),
            size = 3, position = position_stack(vjust = 1.06, reverse = TRUE))

dissecting_total
aspect_ratio <- 2.5
save_fig("dissecting_total", "output/", "large")



###############################################
# Table 5
# \input{tables/table-robust_metrics}
###############################################

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
       cluster =~passer^half_season+receiver^half_season,
       #       label="reg-pois-cit-robust_metrics",
       caption = "Results on Robustness",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois1, 
       file = "output/table-robust_metrics.tex", replace = TRUE)


###############################################
# Figure 4
# Separate file
###############################################


###############################################
# Table 6
# \input{tables/table-robust-confounders}
###############################################

pois1fe_r1=fepois(pass_count ~ same_cult_any                     | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1
                  , data=data_work)
pois1fe_r2=fepois(pass_count ~ same_cult_any +.[pass_feat] + experience_shared1  + experience_other_team    | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 
                  , data=data_work)
pois1fe_r3=fepois(pass_count ~ same_cult_any +.[pass_feat] + experience_shared1  + experience_other_team   +.[player_diff] | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 
                  , data=data_work)
# position interaction * country for national style
pois1fe_r4=fepois(pass_count ~ same_cult_any     +.[pass_feat]             | .[fe2] + .[fe_pos_ctry] ,offset =~ ln_passes_in_shared_mins1 , data=data_work)


etable(pois1fe_r1, pois1fe_r2, pois1fe_r3, pois1fe_r4,
       cluster =~passer^half_season+receiver^half_season,
       label="reg-pois-cit-extend",
       caption = "Results on Robustness",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois1, 
       file = "output/table-robust-confounders.tex", replace = TRUE)


###############################################
# Table 7
# \input{tables/reg-cult_similar.tex} 
###############################################

# nation, colony, federation, language
pois1fe_ncl=fepois(pass_count ~ .[cult]       +.[pass_feat]          
                        | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work)

# Melitz-Tozbal LC similarity
pois1fe_similar1=fepois(pass_count ~ similar_lang_cat+.[pass_feat]             
                        | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work)
pois1fe_similar2=fepois(pass_count ~ similar_lang_cat+similar_location    +.[pass_feat]             
                        | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work)

# VWS
pois1fe_similar3=fepois(pass_count ~ wvs_distance00_cat   +   .[pass_feat]             
                        | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work)
# alternative measure is wvs_distance_min_cat = robust

etable(pois1fe_ncl, pois1fe_similar1, pois1fe_similar2,
       cluster = ~passer^half_season+receiver^half_season)
       

etable(pois1fe_ncl, pois1fe_similar1, pois1fe_similar2,pois1fe_similar3,
       cluster = ~passer^half_season+receiver^half_season,
       label="reg-pois-similar-cult",
       caption = "Dissecting culture",
       order = "Shared cultural background (any) (0/1)", 
       tablenotes = note_pois_cult, 
       file = "output/reg-cult_similar.tex", replace = TRUE)




###############################################
# Table 8
# see separate file `03_aggregate_win_pass.R`
###############################################

fe1 ="team^half_season + passer_position^league_half_season + receiver_position^league_half_season + passer_nationality^league_half_season+receiver_nationality^league_half_season"


###############################################
# Table 9
#\input{tables/reg-pois-cult-player}
###############################################
pois1_players1=fepois(pass_count ~ same_cult_any   + .[ctrl] + .[player_value] +.[pass_feat]    |  .[fe_pos] + .[fe1] 
                      ,data=data_work)
etable(pois1_players1)

# player
 etable(pois1_players1,
        cluster= ~ passer^half_season+receiver^half_season ,
        caption = "Player values",
        tablenotes = note_pois, 
        label ="tab:reg-pois-cult-player",
        file = "output/reg-pois-cult-player.tex", replace = TRUE)
 


###############################################
 # Table 10
 #\input{tables/reg-pois-hetero-length}
###############################################


tabyl(data_work$long_pass)
pois1fe_nolength=fepois(pass_count ~ same_cult_any                | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work)
pois1fe_long_pass1=fepois(pass_count ~ same_cult_any*long_pass + forwardness | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work)
pois1fe_long_pass2=fepois(pass_count ~ same_cult_any*long_pass +.[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work)

# extended pass sequence with long passes
etable(pois1fe_nolength, pois1fe_long_pass1,pois1fe_long_pass2,
       cluster= ~ passer^half_season+receiver^half_season ,
       caption = "Heterogeneity by complexity I: Long passes",
       label ="tab:reg-pois-length",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois, 
       file = "output/reg-pois-hetero-length.tex", replace = TRUE)


###############################################
# Table 11
#\input{tables/reg-pois-seq}
###############################################

pois1fe_seq1=fepois(pass_count_sequences ~ same_cult_any +.[pass_feat]  | .[fe2] + .[fe_pos] ,offset =~  ln_tau_od , 
                    data=data_work)
pois1fe_seq2=fepois(pass_count_complex ~ same_cult_any +.[pass_feat]  | .[fe2] + .[fe_pos] ,offset =~  ln_tau_od , 
                    data=data_work)
pois1fe_seq1_fwd=fepois(pass_count_forward_sequences ~ same_cult_any +.[pass_feat]  | .[fe2] + .[fe_pos] ,offset =~  ln_tau_od , 
                        data=data_work)
pois1fe_seq2_fwd=fepois(pass_count_forward_complex ~ same_cult_any +.[pass_feat]  | .[fe2] + .[fe_pos] ,offset =~  ln_tau_od , 
                        data=data_work)

etable(pois1fe_seq1,pois1fe_seq2,pois1fe_seq1_fwd,pois1fe_seq2_fwd,
       cluster= ~ passer^half_season+receiver^half_season ,
       caption = "Pass sequences and complicated pass sequences",
       label ="tab:reg-pois-seq",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois_seq, 
       file = "output/reg-pois-seq.tex", replace = TRUE)



###############################################
# Table 12
#\input{tables/r7_a2b_reg-pois-neargoal}%\label{tab:reg-pois-neargoal}
###############################################


pois1fe_neargoal=fepois(pass_neargoal ~ same_cult_any     +.[pass_feat]             | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 , data=data_work)
# pois1fe_neargoal_1=fepois(pass_neargoal ~ same_cult_any     +.[pass_feat]             | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 , data=data_near)

etable(pois1fe, pois1fe_neargoal,
       cluster= ~ passer^half_season+receiver^half_season,
       caption = "Heterogeneity by passes: high stake passes",
       #label ="tab:reg-pois-neargoal",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois_seq, 
       file = "output/r7_a2b_reg-pois-neargoal.tex", replace = TRUE)




###############################################
# Table 13
#\input{tables/reg_table_heterogeneity}%\label{tab:reg_table_heterogeneity}
###############################################

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

# combined table
etable(pois1fe_p1_age, pois1fe_cult_c4,pois1fe_p2_ln_value,    
       cluster= ~ passer^half_season+receiver^half_season,
       label="reg_table_heterogeneity",
       caption = "Heterogeneity by age, group size and receiver",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois_heterogeneity, 
       file = "output/reg_table_heterogeneity.tex", replace = TRUE)



###############################################
# Table 14
#\input{tables/reg-mechanism-into-groups}
###############################################

# using cutoff at 3
pois1fe_culturegr3=fepois(pass_count ~ same_cult_any 
                          +culture_inoutgroup_g0g1_cut3+ culture_inoutgroup_g1g0_cut3+culture_inoutgroup_g1g1_cut3
                          | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1, data=data_work)

# table
etable(pois1fe_culturegr3,
       cluster= ~ passer^half_season+receiver^half_season,
       caption = "Mechanism: Pass from and to larger grops",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois_mechanism, 
       file = "output/reg-mechanism-into-groups.tex", replace = TRUE)



###############################################
# Table 15
#\input{tables/reg-pois-shared-exp-together-ext} 
###############################################

# define values
experience_low_cut= 215
experience_high_cut= 364
experience_end_cut =729


data_exp1 = data_work 
data_exp1 %<>% filter(new_median_id1==1)
data_exp2 = data_work 
data_exp2 %<>% filter(new_median_id1==1, w_club_days_id1<experience_end_cut)
data_exp3 = data_work 
data_exp3 %<>% filter(new_median_id2==1)

###################################

#baseline: 3 columns
tabyl(data_exp1$w_days_shared_p1_y_bin1)
pois1fe_p1shared_days1=fepois(pass_count ~ same_cult_any*w_days_shared_p1_y_bin1+ 
                                .[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 , data=data_exp1
                              ,fsplit                          =  ~ experience_other_team   )

etable(pois1fe_p1shared_days1)
#constrain
tabyl(data_exp2$w_days_shared_p1_y_bin1)
pois1fe_p1shared_days2=fepois(pass_count ~ same_cult_any*w_days_shared_p1_y_bin1+ .[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 , data=data_exp2)


#longer period for new
tabyl(data_exp3$w_days_shared_p1_y_bin2)
pois1fe_p1shared_days3=fepois(pass_count ~ same_cult_any*w_days_shared_p1_y_bin2+ .[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 , data=data_exp3)


etable(pois1fe_p1shared_days1 ,pois1fe_p1shared_days2, pois1fe_p1shared_days3,
       cluster= ~ passer^half_season+receiver^half_season ,
       caption = "Homophily over time: shared experience",
       tablenotes = note_pois, 
       label ="tab:reg-pois-shared-exp-together-ext",
       file = "output/reg-pois-shared-exp-together-ext.tex", replace = TRUE)




############################
#  Appendix A
############################


############################
# \input{tables/r5_a2b_reg-pois-minutes2}
############################


#pois1fe_ncl=fepois(pass_count ~ .[cult]       +.[pass_feat]          | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 , data=data_work)
pois2fev2_ncl=fepois(passes_in_shared_mins1 ~ .[cult]                           | .[fe2] + .[fe_pos]  , data=data_work)
pois3fe_ncl=fepois(pass_count ~ .[cult] +.[pass_feat]                          | .[fe2] + .[fe_pos] , data=data_work)



etable(pois1fe_ncl,pois2fev2_ncl,pois3fe_ncl,
       cluster= ~ passer^half_season+receiver^half_season,
       caption = "Selection into play",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois_min, 
       file = "output/r5_a2b_reg-pois-minutes2.tex", replace = TRUE)





############################
# \input{tables/r3_a2b_reg-robust2}
############################


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
       cluster= ~ passer^half_season+receiver^half_season,
       caption = "Results on Robustness",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois1, 
       file = "output/r3_a2b_reg-robust2.tex", replace = TRUE)





