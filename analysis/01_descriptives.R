
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
# 01_descriptives.R
#
# This version: v1.1 2024-04-10
#######################################################

# This scripts provides numbers quoted in the text


################################
# Players
################################

# We have 6,998 players in our sample, for whom we can fully map their entire career,
# European football is truly globalized as there are players from 138
# French, Spanish and Italian players make up the largest citizenship groups, followed by Germans, English, Brazilians and Argentinians.Other countries of citizenship with several players include the Netherlands, Serbia, Senegal, and Uruguay. 

data_player = data_work %>%
  group_by(passer) %>%
  dplyr::summarise(citizenship_main1=first(citizenship_v0_p1),
            team=first(team)) %>%
  ungroup()

Hmisc::describe(data_player$citizenship_main1)



# In our dataset, 37.9\% of the players have the same nationality, 8.4\% have the same colonial legacy, 1.5\% have the same federal legacy and 2.8\% have the same language but different colonial legacy, different federal legacy and different nationality. 
# We consider all these players as having the same culture. According to this definition, 50.6\% of the players in our sample have the same culture, whereas 49.4\% of them do not.

percent.table(data_work$same_cit_any)
percent.table(data_work$same_colony_only)
percent.table(data_work$same_federation_only)
percent.table(data_work$same_language_only)
percent.table(data_work$same_cult_any)


################################
# Passes
################################

# 730 passes per game
# We have 10.73 million passes in total.
nrow(data_work) # 669,022
summary(data_work$pass_count)
15.92*669022 # total passes
15.92*669022/14608 # total pass/game



################################
# dataset, passers
################################

# The resulting estimation dataset is a directional pass dataset that,
# keeping track of who is the passer and who is the receiver, 
# consists of 669,022 observations at the passer, receiver and half-season level. 
# In a half-season, a player makes a total of 288 passes on average (ranging between 0 and 2166, with median equal to 221). 
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






################################
#  Other numbers
################################

# share of passes by positions. 
fe_pos="passer_position2^receiver_position2"
tabyl(data_work$passer_position2)



#  50% of player pairs are involved in at least a complex pass sequence in our sample. 
# Conditional on having joined at least a complex pass sequence in a half-season, on average player pairs are involved in 3.88 complex pass sequences in that half-season.
Hmisc::describe(data_work$pass_count_complex)
Hmisc::describe(data_work$pass_count_complex[data_work$pass_count_complex>0], basic = T)
round(333712/  669022,2)
########################################################################################
########################################################################################


################################
# Appendix 
################################

# 54% of those who share a colonial past also share a citizenship. 
percent.table(data_work$same_cit_any[data_work$same_colony_any==1])

# As for colonial legacy, the majority of the same colonial legacy category comes from a link between a ruler and a former colony. 
# Some links are derived from having the same colonial ruler. 
percent.table(data_work$same_colony[data_work$same_colony_any==1])

# possible for two players to have a ruler-colony legacy and a colonial sibling legacy
percent.table(data_work$same_colony_sibling[data_work$same_colony_any==1], data_work$same_colony[data_work$same_colony_any==1])

percent.table(data_work$same_cit_any, data_work$same_colony_any)


# there are 52,092 player-pairs*time (7.8\%) where only one direction of the pass is recorded.
nrow(data_work[data_work$pass_count==0,])

