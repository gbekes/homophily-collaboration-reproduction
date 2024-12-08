
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams
#

# d02_04_countries_cult_dist_anon.R

# Using Toubal-Melitz dataset
# alters it to match countries in the dataset, ie manages GB, Belgium and Luxemburg
# uses table:ling_to_append

# This version: v2.1: 2024-11-18
#######################################################






# load data
ling_web_total_dta = haven::read_dta(paste0(data_imported,"/culture/ling_web_total.dta"))

# codes
codes_dta = haven::read_dta(paste0(data_imported,"/culture/codes.dta"))

# correction table
to_append=read_csv(paste0(data_imported,"/ling_to_append.csv"))

# to get colony status we use a different table
cepii_gravdata = read_dta(paste0(data_imported,"/cepii-gravdata-2015.dta")) %>%  ## correct Yugoslavia
  mutate(iso3_o = case_when(iso3_o == "YUG" ~ "SRB",
                            iso3_o == "ROM" ~ "ROU",
                            iso3_o == "ZAR" ~ "COD",
                            
                            TRUE ~ iso3_o
                            ),
         iso3_d = case_when(iso3_d == "YUG" ~ "SRB",
                            iso3_d == "ROM" ~ "ROU",
                            iso3_d == "ZAR" ~ "COD",
                            TRUE ~ iso3_d
                            )
         )

cepii_gravdata = cepii_gravdata %>% 
  select(iso_o = iso3_o,
         iso_d = iso3_d,
         colony,
         sibling,
         comlang_off,
         comlang_ethno,
         contig
         )


### REWRITE THIS PART AGAIN (2022/02/12)
cepii_gravdata = cepii_gravdata %>% 
  left_join(.,
            ling_web_total_dta %>% select(-col),
            by = c("iso_o" = "iso_o",
                   "iso_d" = "iso_d"
            )
  )
  
cepii_gravdata = cepii_gravdata %>% 
  mutate(country_o = if_else(str_detect(country_o,  "d'Ivoire"), "Cote d'Ivoire", country_o),
         country_d = if_else(str_detect(country_d,  "d'Ivoire"), "Cote d'Ivoire", country_d)
  ) %>% 
  # take croatia and aruba out
  filter(country_o != "Croatia",
         country_d != "Croatia",
  ) %>% 
  filter(country_o != "Aruba",
         country_d != "Aruba",
  ) %>% 
  select(iso_o, country_o, iso_d, country_d,
         comlang_off, comlang_ethno, csl, cnl, prox1, lp1, prox2, lp2, 
         cl, cle, colony, sibling, contig
         )

cepii_gravdata %<>% mutate(col =comlang_off)

# Augment the ling web_total_dta with rows about the relation to themselves. I only fill in col = 1 and cle = 0.8
countries = cepii_gravdata %>% 
  distinct(country_o, iso_o)

samecountries = cbind(countries, countries)

names(samecountries) = c( "country_o", "iso_o", 
                          "country_d", "iso_d")

samecountries = samecountries %>% 
  mutate(col = 1,
         csl = 1,
         cnl = 1,
         prox1 = NA,
         lp1 = NA,
         prox2 = NA,
         lp2 = NA,
         cl = 1,
         cle = 1,
         colony = 0,
         sibling = 0,
         contig = 0,
         comlang_ethno = 1,
         comlang_off = 1
  )

# samecountries = samecountries[names(cepii_gravdata)]

samecountries <- samecountries %>%
  dplyr::select(all_of(names(cepii_gravdata)))


cepii_gravdata = rbind(cepii_gravdata, 
                           samecountries) %>% 
  arrange(country_o, country_d)

tabyl(cepii_gravdata$country_o)

forappend = samecountries %>% select(country_o, country_d)
names(forappend) = names(to_append)
to_append = rbind(to_append,
                  forappend
)


to_append = to_append %>% 
  left_join(.,
            codes_dta,
            by = c("extra_countries" = "country")
  ) %>% 
  left_join(.,
            codes_dta,
            by = c("mother_countries" = "country")
  ) %>% 
  mutate(iso_extra = coalesce(iso.x, iso.y)) %>% 
  select(-iso.x, -iso.y)


# First join on the o-countries

cepii_gravdata_extras_o = cepii_gravdata %>% 
  left_join(.,
            to_append,
            by = c("country_o" = "mother_countries")
  )

cepii_gravdata_extras_d = cepii_gravdata_extras_o %>% 
  left_join(.,
            to_append,
            by = c("country_d" = "mother_countries")
  )

extras = cepii_gravdata_extras_d %>% 
  filter((!is.na(extra_countries.x)) | (!is.na(extra_countries.y))
  ) %>% 
  mutate(country_o = if_else(!is.na(extra_countries.x),
                             extra_countries.x,
                             country_o
  ),
  country_d = if_else(!is.na(extra_countries.y),
                      extra_countries.y,
                      country_d
  ),
  iso_o = if_else(!is.na(iso_extra.x),
                  iso_extra.x ,
                  iso_o
  ),
  iso_d = if_else(!is.na(iso_extra.y),
                  iso_extra.y,
                  iso_d
  )
  ) %>% 
  select(-starts_with("iso_extra"),
         -starts_with("extra_countries"),
  )

# checks

extras %>% 
   filter(country_o == "England",
          country_d == "Belgium")

extras %>% 
   filter(country_o == "France",
          country_d == "Cote d'Ivoire")

extras %>% 
 filter(country_o == "Croatia",
        country_d == "Serbia")
 
# Check for duplicates.

 find_dupli = extras %>% 
   group_by(country_o, country_d) %>% 
   summarise(N = n()) %>%
     filter(N > 1)
 
 # find_dupli
# check for missings
 extras %>% filter(is.na(col)) 
 
##############################
## MANUAL CHECKS
##############################
 
extras = extras %>% 
   mutate(serb_yugo_o = 1*(iso_o %in% c("SRB", "HRV", "BIH", "MNE")),
          serb_yugo_d = 1*(iso_d %in% c("SRB", "HRV", "BIH", "MNE")),
          full_yugo_o = 1*((iso_o %in% serb_yugo_o) | iso_o %in% c("MKD",
                                                                  "XKX",
                                                                  "SVN"
                                                                  )
                          ),
          full_yugo_d = 1*((iso_d %in% serb_yugo_d) | iso_d %in% c("MKD",
                                                                   "XKX",
                                                                   "SVN"
                                                                   )
                           )
          )


 
extras = extras %>% 
   mutate(col = case_when(serb_yugo_o == 1 & serb_yugo_d == 1 ~ 1, # Yugoslavia
                          serb_yugo_o == 1 & serb_yugo_d == 0 & full_yugo_d == 1 ~ 0,
                          
                          serb_yugo_d == 1 & serb_yugo_o == 1 ~ 1,
                          serb_yugo_d == 1 & serb_yugo_o == 0 & full_yugo_o == 1 ~ 0,
                          
                          # Czechoslovakia
                          iso_o == "CZE" & iso_d == "SVK" ~ 1,
                          iso_d == "CZE" & iso_o == "SVK" ~ 1,
                          
                          
                          # Soviet Union
                          iso_o == "BLR" & iso_d == "RUS" ~ 1,
                          iso_d == "BLR" & iso_o == "RUS" ~ 1,
                          
                          iso_o == "UKR" & iso_d == "RUS" ~ 0,
                          iso_d == "UKR" & iso_o == "RUS" ~ 0,
                          
                          # Belgium and Germans
                          iso_o == "BEL" & iso_d == "DEU" ~ 0,
                          iso_d == "BEL" & iso_o == "DEU" ~ 0,
                          iso_o == "BEL" & iso_d == "AUT" ~ 0,
                          iso_d == "BEL" & iso_o == "AUT" ~ 0,
                          iso_o == "BEL" & iso_d == "LIE" ~ 0,
                          iso_d == "BEL" & iso_o == "LIE" ~ 0,
                          
                          
                          # Swiss and Italy
                          iso_o == "CHE" & iso_d == "ITA" ~ 0,
                          iso_d == "CHE" & iso_o == "ITA" ~ 0,
                          
                          
                          TRUE ~ col
                          ),
          sibling = case_when(serb_yugo_o == 1 & serb_yugo_d == 1 ~ 1, # Yugoslavia
                          serb_yugo_o == 1 & serb_yugo_d == 0 & full_yugo_d == 1 ~ 1,
                          
                          serb_yugo_d == 1 & serb_yugo_o == 1 ~ 1,
                          serb_yugo_d == 1 & serb_yugo_o == 0 & full_yugo_o == 1 ~ 1,
                          
                          # Czechoslovakia
                          iso_o == "CZE" & iso_d == "SVK" ~ 1,
                          iso_d == "CZE" & iso_o == "SVK" ~ 1,
                          
                          
                          # Soviet Union
                          # iso_o == "BLR" & iso_d == "RUS" ~ 1,
                          # iso_d == "BLR" & iso_o == "RUS" ~ 1,
                          # 
                          # iso_o == "UKR" & iso_d == "RUS" ~ 1,
                          # iso_d == "UKR" & iso_o == "RUS" ~ 1,
                          
                          TRUE ~ sibling
                          ),
          colony = case_when(# Soviet Union
            iso_o == "BLR" & iso_d == "RUS" ~ 1,
            iso_d == "BLR" & iso_o == "RUS" ~ 1,
            iso_o == "UKR" & iso_d == "RUS" ~ 1,
            iso_d == "UKR" & iso_o == "RUS" ~ 1,
            
            iso_o == "IRL" & iso_d == "GBNIR" ~ 1,
            iso_d == "IRL" & iso_o == "GBNIR" ~ 1,
            
            iso_o == "GNQ" & iso_d == "PRT" ~ 1,
            iso_d == "GNQ" & iso_o == "PRT" ~ 1,
            
            TRUE ~ colony
            ),
          )
 
# check
extras %>% 
   filter((country_o == "Serbia" & country_d == "Croatia") | 
            (country_d == "Serbia" & country_o == "Croatia") |
            
            (country_o == "Serbia" & country_d == "Bosnia and Herzegovina") | 
            (country_d == "Serbia" & country_o == "Bosnia and Herzegovina") |
            
            (country_o == "Serbia" & country_d == "Montenegro") | 
            (country_d == "Serbia" & country_o == "Montenegro") |
            
            
            (country_o == "Croatia" & country_d == "Bosnia and Herzegovina") | 
            (country_d == "Croatia" & country_o == "Bosnia and Herzegovina") |
            
            (country_o == "Croatia" & country_d == "Montenegro") | 
            (country_d == "Croatia" & country_o == "Montenegro") |
            
            (country_o == "Montenegro" & country_d == "Bosnia and Herzegovina") | 
            (country_d == "Montenegro" & country_o == "Bosnia and Herzegovina") 
          
          ) %>% 
   select(country_o, country_d, col, colony, sibling, cle)

 
 
 
# save ling
write_csv(extras, paste0(data_tidy_created,"ling_web_augmented.csv"))

# drop data
rm(ling_web_total_dta, codes_dta, 
   to_append, cepii_gravdata, samecountries, countries, cepii_gravdata_extras_o, cepii_gravdata_extras_d, 
   extras, find_dupli, forappend)

