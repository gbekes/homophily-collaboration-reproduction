
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
# 02_regressions.R
#
# This version: 2024-03-26
#
#######################################################


#######################################################
# Regression on aggregate data: points on passes

# in: 
# anon_work-regressions.RData

#######################################################

#data_work=readRDS(paste0(data_work,"anon_work-regressions.RData"))

###########################################
# define control vars and fixed effects
###########################################

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


###############################################
################################################
# MAIN TABLES
###############################################
################################################


# drop FEs rows -- to make ols comparable -- =-917 observations are dropped form sample

pois1fe=fepois(pass_count ~ same_cult_any     +.[pass_feat]             | .[fe2] + .[fe_pos] 
               ,offset =~ ln_tau_od , data=data_work)

data_work <- data_work %>% rowid_to_column()
rows_to_delete <- as.data.frame(pois1fe$obs_selection)
rows_to_delete$obsRemoved <- rows_to_delete$obsRemoved * -1
colnames(rows_to_delete) <- c("rowid")
data_work <- anti_join(data_work,rows_to_delete,by="rowid")


###############################################
# Table 1 Baseline results
# \input{tables/reg-pois-cult-base}
###############################################


# naive 
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
       tablenotes = note_pois_base, 
       label ="tab:reg-pois-cult-base",
       file = paste0(output,"reg-pois-cult-base.tex"), replace = TRUE)


###############################################
# Table 2 From total homophily to choice homophily
# \input{tables/reg-pois-cit-uncond}
###############################################

# pois1fe same as before
pois0=fepois(pass_count ~ same_cult_any                                          | .[fe0], data=data_work)
pois1fe_time=fepois(pass_count ~ same_cult_any     +.[pass_feat]               | .[fe2] + .[fe_pos] , data=data_work)

etable(pois0, pois1fe_time, pois1fe,
       drop =c("age_p1_y", "age_p2_y", "w_club_days_id1" ,"w_club_days_id2","height_p1", "height_p2", "is_loan_id1", "is_loan_id2",
                  "ln_avg_pass_length"),
              cluster= ~ passer^half_season+receiver^half_season )
       
etable(pois0, pois1fe_time, pois1fe,
       cluster= ~ passer^half_season+receiver^half_season ,
       caption = "Total and choice homophily",
       order = "Shared cultural background (any) (0/1)", tablenotes = note_pois_unc, 
       drop =c("age_p1_y", "age_p2_y", "w_club_days_id1" ,"w_club_days_id2","height_p1", "height_p2", "is_loan_id1", "is_loan_id2"
               ),
        file = paste0(output,"reg-pois-cit-uncond.tex"), replace = TRUE)



############################
# Figure 1 Dissecting total homophily bias
############################

coeff_total = round(pois0$coefficients["same_cult_any"],4)
c_choice = round(pois1fe$coefficients["same_cult_any"],4)
c_coach= round(pois1fe_time$coefficients["same_cult_any"]-pois1fe$coefficients["same_cult_any"],4)
c_induced = round(pois0$coefficients["same_cult_any"]-pois1fe_time$coefficients["same_cult_any"],4)


fig1 <- data.frame(
  var= c("choice homophily", "mix by coach decisions", "induced: sorting, selection"),
  coeff = c(c_choice, c_coach, c_induced),
  type = "A", "A", "A") %>% 
  mutate(total = coeff_total,
         var = factor(var, levels = c("choice homophily", "mix by coach decisions", "induced: sorting, selection"))
  )


dissecting_total = ggplot(fig1, aes(fill=var, y=coeff, x=type )) + 
  geom_col(position = position_stack(reverse = TRUE), alpha=0.85) +
  scale_fill_manual(values = c(color[3], color[1], color[5]), name = NULL) +
  labs(y = "Homophily bias (coefficients)", x = "") +
  theme_bg() +
  scale_y_continuous(expand=c(0,0), limits=c(0, 0.07), breaks = seq(0, 0.07, by = 0.01),
                     minor_breaks = seq(0, 0.07, by=0.005), labels = scales::percent) +
  coord_flip() +
  theme(legend.position = "bottom", axis.ticks.y = element_blank(), axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), legend.text=element_text(size=8)
  ) +
  geom_text(aes(label = paste0(coeff*100, "%")), 
            size = 3.5, position = position_stack(vjust = 0.5, reverse = TRUE))  +
  geom_text(aes(x = type, y = total, 
                label = paste0(total*100, "%"), fill = NULL),
            size = 3.5, position = position_stack(vjust = 1.06, reverse = TRUE))

dissecting_total
aspect_ratio <- 2.5
save_fig("dissecting_total", output, "large")





###############################################
# Table 3 Extensions: potential confounders
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

etable(pois1fe, pois1fe_r1, pois1fe_r2, pois1fe_r3, pois1fe_r4,
       cluster =~passer^half_season+receiver^half_season)

tabyl(data_work$experience_other_team)

etable(fepois(pass_count ~ same_cult_any +.[pass_feat] + experience_other_team ,offset =~ ln_passes_in_shared_mins1 
                         , data=data_work))       

etable(pois1fe, pois1fe_r1, pois1fe_r2, pois1fe_r3, pois1fe_r4,
       cluster =~passer^half_season+receiver^half_season,
       label="reg-pois-cit-extend",
       caption = "Results on Robustness",
       order = "Shared cultural background (any) (0/1)", 
       tablenotes = note_pois_confound, 
       file = paste0(output,"table-robust-confounders.tex"), replace = TRUE)


###############################################
# Table 4 Dissecting culture
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

pois1fe_similar3=fepois(pass_count ~ wvs_distance00_cat   +   .[pass_feat]             
                        | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work)


etable(pois1fe_ncl, pois1fe_similar1, pois1fe_similar2, pois1fe_similar3,
       cluster = ~passer^half_season+receiver^half_season)
       
# labels renamed in actual table
etable(pois1fe_ncl, pois1fe_similar1, pois1fe_similar2,pois1fe_similar3,
       cluster = ~passer^half_season+receiver^half_season,
       label="reg-pois-similar-cult",
       caption = "Dissecting culture",
       order = "Shared cultural background (any) (0/1)", 
       tablenotes = note_pois_cult, 
       file = paste0(output,"reg-cult_similar.tex"), replace = TRUE)





###############################################
# Table 5 Benchmarking homophily
###############################################

pois1_players1=fepois(pass_count ~ same_cult_any   + .[ctrl] + .[player_value] +.[pass_feat]    |  .[fe_pos] + .[fe1] 
                      ,data=data_work)

# table
etable(pois1_players1,
       cluster= ~ passer^half_season+receiver^half_season)

etable(pois1_players1,
        cluster= ~ passer^half_season+receiver^half_season ,
        caption = "Player values",
        tablenotes = note_pois_player, 
        label ="tab:reg-pois-cult-player",
        file = paste0(output,"reg-pois-cult-player.tex"), replace = TRUE)
 

 
###############################################
# Table 6 High stakes in Passes 
 #\input{tables/reg-pois-hetero-combined}
###############################################


 # long passes
pois1fe_long_pass1=fepois(pass_count ~ same_cult_any*long_pass + forwardness | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work)
pois1fe_long_pass2=fepois(pass_count ~ same_cult_any*long_pass +.[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od , data=data_work)


# complex passes. 
pois1fe_seq1_fwd=fepois(pass_count_forward_sequences ~ same_cult_any +.[pass_feat]  | .[fe2] + .[fe_pos] ,offset =~  ln_tau_od , 
                        data=data_work)
pois1fe_seq2_fwd=fepois(pass_count_forward_complex ~ same_cult_any +.[pass_feat]  | .[fe2] + .[fe_pos] ,offset =~  ln_tau_od , 
                        data=data_work)

# near goal
pois1fe_neargoal=fepois(pass_neargoal ~ same_cult_any     +.[pass_feat]             | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 , data=data_work)


# table
etable(pois1fe_long_pass1, pois1fe_long_pass2, pois1fe_neargoal,pois1fe_seq1_fwd,pois1fe_seq2_fwd, 
       cluster= ~ passer^half_season+receiver^half_season)

etable(pois1fe_long_pass1, pois1fe_long_pass2, pois1fe_neargoal,pois1fe_seq1_fwd,pois1fe_seq2_fwd,
       cluster= ~ passer^half_season+receiver^half_season ,
       caption = "High stakes in Passes",
       label ="tab:hetero-combined",
       order = "Shared cultural background (any) (0/1)", 
       tablenotes = note_pois_seq, 
       file = paste0(output,"reg-pois-hetero-combined.tex"), replace = TRUE)


###############################################
# Table 7 Pass Group Size and Large Group Dynamics
###############################################

# now drop if any variable in regression above is missing -- super minimal change just to get same N across columns
data_work_gs = data_work 
data_work_gs %<>% drop_na(culture_group_large_p1_c4, 
                       culture_inoutgroup_g0g1_cut3, culture_inoutgroup_g1g0_cut3, culture_inoutgroup_g1g1_cut3)

# same culture higher for passers belonging to larger groups?  
pois1fe_cult_c4 = fepois(pass_count ~ same_cult_any*culture_group_large_p1_c4 
                         + .[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_tau_od, data=data_work_gs)

# Non-homophilious passes across group sizes
pois1fe_culturegr3=fepois(pass_count ~ same_cult_any 
                          +culture_inoutgroup_g0g1_cut3+ culture_inoutgroup_g1g0_cut3+culture_inoutgroup_g1g1_cut3
                          + .[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1, data=data_work_gs)

# table
etable(pois1fe_cult_c4, pois1fe_culturegr3,
       cluster= ~ passer^half_season+receiver^half_season)
       
etable(pois1fe_cult_c4, pois1fe_culturegr3,
       cluster= ~ passer^half_season+receiver^half_season,
       caption = "Pass Group Size and Large Group Dynamics",
       order = "Shared cultural background (any) (0/1)", 
       tablenotes = note_pois_groups, 
       file = paste0(output,"reg-mechanism-into-groups.tex"), replace = TRUE)




###############################################
# Table 8 Homophily over time: shared experience
###############################################

# define values
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


#constrain
tabyl(data_exp2$w_days_shared_p1_y_bin1)
pois1fe_p1shared_days2=fepois(pass_count ~ same_cult_any*w_days_shared_p1_y_bin1+ .[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 , data=data_exp2)


#longer period for new
tabyl(data_exp3$w_days_shared_p1_y_bin2)
pois1fe_p1shared_days3=fepois(pass_count ~ same_cult_any*w_days_shared_p1_y_bin2+ .[pass_feat] | .[fe2] + .[fe_pos] ,offset =~ ln_passes_in_shared_mins1 , data=data_exp3)


# table
etable(pois1fe_p1shared_days1 ,pois1fe_p1shared_days2, pois1fe_p1shared_days3,
       cluster= ~ passer^half_season+receiver^half_season)
       
etable(pois1fe_p1shared_days1 ,pois1fe_p1shared_days2, pois1fe_p1shared_days3,
       cluster= ~ passer^half_season+receiver^half_season ,
       caption = "Homophily over time: shared experience",
       tablenotes = note_pois_sharedexp, 
       label ="tab:reg-pois-shared-exp-together-ext",
       file = paste0(output,"reg-pois-shared-exp-together-ext.tex"), replace = TRUE)

