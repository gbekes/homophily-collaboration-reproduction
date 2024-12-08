
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# Player data maker
# Gábor Békés
# v 1.1 2024-04-15

# This collects all names and comments and settings for my use of fixest in the project


# Setting a dictionary 
setFixest_dict(c(same_value = "Origin, Receiver valued similarly (0/1)", 
                 ln_minutes_shared= "Total minutes  together (ln)", 
                 ln_value_p1= "Passer valuation (ln)", 
                 ln_value_p2= "Receiver valuation (ln)", 
                 ln_total_pass_by_passer_sh ="Total passes by passer (ln)",
                 ln_total_reception_by_receiver_sh ="Total receipts by receiver (ln)",
                 ln_value_both= "Pass pair player valuation (ln)", 
                 same_cult_any= "Same culture (any) (0/1)",
                 same_cit_any= "Same nationality (0/1)", 
                 colony_any= "Same colonial legacy (0/1)",
                 common_language_only= "Same language  (0/1)", 
                 same_lang_any_notcit = "Shared  language (0/1)",
                 same_language_only = "Shared  language (0/1)",
                 same_colony_only2= "Same colonial/federal legacy (0/1)",
                 same_colony_only= "Same colonial legacy (0/1)",
                 same_ex_federation= "Same federal legacy (0/1)",
                 
                 "same_catSameinternationals"= "Shared Lang, National, Internats (0/1)", 
                 "same_catSamelang,diffnat"= "Shared Lang (0/1)", 
                 "same_catSamenat,nointernat"= "Shared Lang, National (0/1)", 
                 ln_avg_pass_length= "Average length of passes (ln)", 
                 forwardness ="Average forwardness Ind (0-1)",
                 #ln_sh_pass_player_id1="Passer total passes (ln)",
                 #ln_sh_pass_player_id2="Receiver total passes (ln)",
                 ln_total_pass_by_passer_sh ="Passer total passes (ln)",
                 ln_total_reception_by_receiver_sh ="Receiver total pass received (ln)",
                 experience1 ="Passer has 1sh+ experience  (0/1)",
                 experience2 =" has 1sh+ experience  (0/1)",
                 experience_shared1="Shared experience, 1sh+ (0/1)",
                 experience_shared2="Shared experience, 2sh+ (0/1)",
                 experience_other_team="Experience in other team (0/1)",
                 "Shared nationality (0/1) x Shared experience, 1sh+ (0/1)"="Shared nationality x Experience (0/1) ",
                 "same_cit_any x experience_shared1"="Shared nationality x Experience (0/1) ",
                 ln_w_club_p1="Passer - Days w club (ln)",
                 ln_w_club_p2="Receiver - Days w club (ln)",
                 same_cit_pass_share="Share of same nat pairs in passes",
                 ln_value ="Team players value (ln)",                           
                 height_difference="Heigh difference in cm" ,
                   value_diff="Players value difference, d(ln)",
                   same_cit_eu="Both treated as EU player (0/1)",
                 ln_pass_count="Total pass count (ln)",                                 
                 div_simpson_starting="Simpson index of starters",
                 "broad_position_p1-broad_position_p2"="position_p1-position_p2",
                 "player_id1-time" = "Passer x half-season",
                 "player_id2-half_season" = "Receiver x half-season",
                 "broad_position_p1-broad_position_p2 fixed effects" = "origin pl. position x receiver pl. pos FE",
                 "p1_position-league_half_season"="origin pl_position-league_half_season",
                 "ln_passes_in_shared_mins1"="Passer total passes when together",
                 "passer_position2-receiver_position2" = "Cross-position Dummies"
                              
))  


order_cit=c("Shared culture (any) (0/1)")

toorder=c("Shared culture (any) (0/1)",
          "Shared nationality (0/1)", 
          "Shared colonial legacy (0/1)",
          "Shared language only (0/1)" 
)

#########################
# POISSON

note_pois_unc = c("Poisson regression, dependent variable is pass count. Standard errors, clustered at passer level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. Column (2) and (3) include ln(average pass distance) and forwardness index.") 
note_pois_base =c("Poisson regression, dependent variable is pass count. Offset variable included. Standard errors, clustered at passer by half-season and receiver by half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. ")
note_pois_confound =c("Poisson regression, dependent variable is pass count. Offset variable included. Standard errors, clustered at passer by half-season and receiver by half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. '\textit{'Both treated as EU player (0/1)' reflects the corresponding national regulations (see Online Appendix for details).")
note_pois_cult =c("Poisson regression, dependent variable is pass count. 
                  Offset variable included. Standard errors, clustered at passer by half-season and receiver by half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. Column (1): Same culture separated into traits, topcoded in this order: nationality, federal legacy, colonial legacy, and language. Column (2) keeps same nationality and adds linguistic similarity categories as per the ’LC’ index of Melitz-Toubal (2014) (base is different country without similar language). Column (3) adds geographic proximity. Column (4) has value similarity based on the 2021 WVS/EWS, cut at median (base is different values). All columns include pass distance and the forwardness index. ")
# to edit \cite{Melitz_Toubal_2014}

note_pois_player = c("Poisson regression model, dependent variable is pass count. Standard errors, clustered at passer by half-season and receiver by half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. 
             Player values (million euros in ln) are measured at the start of the half-season. For both players, the individual controls are: height, age, time since with club (in days), binary if on loan. Includes ln(average pass distance) and forwardness index. ")
note_pois_groups=c("Poisson regression, dependent variable is pass count. Offset variable included. Standard errors, clustered at passer by half-season and receiver by half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. In column (1) 'passer group: large' is 1 for groups of 4 players or more. In column (2) group size is based on same culture groups, large with at least 3 players. The average length of passes and average forward index are included.")
note_pois_sharedexp = c("Poisson regression, dependent variable is pass count. Offset variable included. Standard errors, clustered at passer by half-season and receiver by half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. ‘Shared experience’ is binary: 1 if passer and receiver have spent at least 215 at current team together. ‘Shared experience long’ is 364 days or more. ‘Early shared experience w other club’ is at past clubs including youth teams. In all specifications, the average length of passes and average forward index are included.")
note_mixed_robust = c("Column 1-3 Poisson regressions, the dependent variable is pass count. Offset variable included. Column 4 OLS regression model, the dependent variable is ln(pass per minute). Standard errors, clustered at passer by half-season and receiver by half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. ")




note_pois0   =c("Poisson regression model. Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. Same cultural background is equality of either cultural aspect (language, colonial, federal legacy or nationality). Player values are start of the half-season. In column 1, additional controls are (for both players): height, age, time since with club (in days), binary if on loan. Includes ln(average pass distance) and forwardness index. Total team pass count is captured via team *half-season fixed effects. ")
note_pois1   =c("Column 1-3 Poisson, column 4 OLS regression model. Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$.  Same cultural background is equality of either cultural aspect (language, colonial, federal legacy or nationality). Includes ln(average pass distance) and forwardness index. Total team pass count is captured via team *half-season fixed effects. Both players EU+ reflect national regulations to play, see Appendix \ref{sec:football-nationality}. Similar valuation and height: both below/above median. ")
note_pois_seq=c("Poisson regression model. Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. Same cultural background is equality of either cultural aspect (language, colonial, federal legacy or nationality). Includes ln(average pass distance) and forwardness index. Total team pass count is captured via team *half-season fixed effects. Sequence count is the number of pass sequences, complex seq count is the number of at least 2 pass-long sequences.  Includes ln(average pass distance) and forwardness index. Total team pass count is captured via team *half-season fixed effects. ")
note_pois_min=c("Poisson regression model. Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. In column 2, the dependent variable is total pass count by player 1 in minutes when both are fielded.  Same cultural background is equality of either cultural aspect (language, colonial, federal legacy or nationality). Total team pass count is captured via team *half-season fixed effects.  ")

note_pois    =c("Poisson regression model. Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. Same cultural background is equality of either cultural aspect (language, colonial, federal legacy or nationality). $tau$ is Sum of passes by passer when receiver is also on the pitch divided by total sum of passes by the passer. Total team pass count is captured via team *half-season fixed effects. ")
note_pois_player    =c("Poisson regression model. Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. Same cultural background is equality of either cultural aspect (language, colonial, federal legacy or nationality). $tau$ is Sum of passes by passer when receiver is also on the pitch divided by total sum of passes by the passer. Total team pass count is captured via team *half-season fixed effects. Player values (euro million in ln) are meausred at the start of period. 'Personal controls' are (for both players): height, age, time since with club (in days), binary if on loan.")
note_pois_cult    =c("Poisson regression model. Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. Column 1: Same cultural background is separated into aspects (language, colonial, federal legacy or nationality. Column 2: has any culture indicator and adds linguistic similarity, Column 3 add geographic proximity. Column 4 has value similarity based on the 2021 WVS/EWS. $tau$ is Sum of passes by passer when receiver is also on the pitch divided by total sum of passes by the passer. Total team pass count is captured via team *half-season fixed effects. ")



#Same nationality, same language and different nationality: the base category is different language (and nationality).
#In same national teams, same nationality but in national teams, same language but different nationality: the base category is different language (and nationality).


notes_pois_experience=c("Poisson regression model. Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$.   Same cultural background is equality of either cultural aspect (language, colonial, federal legacy or nationality). Player values are start of the half-season  1sh+ experience is a binary variable denoting if player has been with team at least a half-season. Shared experience is one both have been together in team for at least a half-season. Match pass count is captured via team *time period fixed effects. ")

notes_pois_ext=c("Poisson regression model. Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$.  Same value is binary, 1 if similar valued players.  Same cultural background is equality of either cultural aspect (language, colonial, federal legacy or nationality). Match pass count is captured via team *time period fixed effects. ")

notes_pois_seq=c("Poisson regression model. Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$.   Same cultural background is equality of either cultural aspect (language, colonial, federal legacy or nationality). Sequence count is the number of pass sequences, complex seq count is the number of at least 2 pass-long sequences.   Match pass count is captured via team *time period fixed effects. ")


note_pois=c("Poisson regression model. Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. Top 5 soccer leagues, 8 seasons: 2011-2019. Half-season: 16-20 games before and after 1 January. Same cultural background is equality of either cultural aspect (language, colonial, federal legacy or nationality).  Includes ln(average pass distance) and forwardness index. Total team pass count is captured via team *half-season fixed effects. ")

#########################
# aggregate

notes_teams1=c("OLS regression model. Standard errors, clustered at the team level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$. Team * half-season level aggregated data. Top 5 soccer leagues. Column 1: First half of 2015/16 season, columns 2 and 3: 8 seasons: 2011-2019. Half season is 16-20 games before and after 1 January.  ")

# appendix

note_ols=c("Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$.  
           Same value is binary, 1 if similar valued players.  Same cultural background is equality of either cultural aspect (language, colonial legacy or nationality). Player values are start of the half-season 
           Fixed effects are position of player1 (player2) *time period and citizenship of player1 (player)*half-season Match pass count is captured via team *time period fixed effects. ")

notes_fe=c("Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$.  Same value is binary, 1 if similar valued players.  Same cultural background is equality of either cultural aspect (language, colonial legacy or nationality). Player values are start period. Fixed effects are player1-period and player2*time . Match pass count is captured via team *time period fixed effects. Match pass count is captured via team *time period fixed effects. ")

note_ols_cat=c("Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$.  Same value is binary, 1 if similar valued players. Same cat is is a factor of national similarity types. Base category: different language (and nationality). Fixed effects are position of player1 (player2) *time period and citizenship of player1 (player)*half-season Match pass count is captured via team *time period fixed effects. ")

notes_fe_cat=c("Standard errors, clustered at passer*half-season and receiver*half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$.  Same value is binary, 1 if similar valued players. Same cat is is a factor of national similarity types. Base category: different language (and nationality). Player values are start period. Fixed effects are player1-period and player2*time . Match pass count is captured via team *time period fixed effects. Match pass count is captured via team *time period fixed effects. ")



postprocess = function(x, caption, tablenotes){
  
  # we add the threeparttable
  
  # Beginning + caption
  # notes_value = paste0("\\item[", 1:n_notes, "] ", tablenotes, "\n")
  extra_begin = "\\begin{table}\n\\small\n\\begin{threeparttable}[b]\n"
  if(!missing(caption)){
    extra_begin[2] = paste0("\\caption{", caption, "}\n")
  }
  
  # End + notes
  extra_end = "\\end{threeparttable}\n\\end{table}\n"
  if(!missing(tablenotes)){
    notes_start = "\n\\begin{tablenotes}\n"
    notes_end = "\\end{tablenotes}\n"
    
    n_notes = length(tablenotes)
    notes_value = paste0("\\item ", tablenotes, "\n")
    
    extra_end = c(notes_start, notes_value, notes_end, extra_end)
  }
  
  # Inserting the code
  glue_that = function(x) paste(x, collapse = " ")
  x[x == "%start:tab\n"] = glue_that(extra_begin)
  x[x == "%end:tab\n"] = glue_that(extra_end)
  
  x
}

grouplist=list(  "Pass feature variables"=c("ln_avg_pass_length", "forwardness", "Average forwardness Ind (0-1)", "Average length of passes (ln)"),
                 "Player feature variables"=c("ln_value_p1", "ln_value_p2", "ln_value_both", "same_value",  "P1 and P2 valued similarly (0/1)","Pass pair player valuation (ln)")
)

# c("P1 and P2 valued similarly (0/1)", "Average forwardness Ind", "Average length of passes (ln)", "Pass pair player valuation (ln)" )


#style.tex = style.tex("aer"),
# Registering the routine (avoids using it in etable call)
setFixest_etable(postprocess.tex = postprocess, 
                 fitstat = ~ n + pr2,
                 # style.tex(line.bottom = "\\bottomrule")
                  style.tex = style.tex("aer")
                 )
# sdBelow=T



note_pois_heterogeneity=c("Poisson regression model. Standard errors, clustered at passer by half-season and receiver by half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$.  The average length of passes and average forward index are included.  Total team pass count is captured via team  by half-season fixed effects. Receiver quality is based on transfer values, highest two per team. Group size is based on same culture groups. Experienced player is aged 23.2 on the starts of the half-season.")
note_pois_mechanism =c("Poisson regression model. Standard errors, clustered at passer by half-season and receiver by half-season level, are in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.1$.  Includes ln(average pass distance) and forwardness index. Total team pass count is captured via team  by half-season fixed effects. Cutoff for group size: large group is defined as an average size of at least 3 members. ")