
#######################################################
# Békés-Ottaviano: Cultural Homophily and Collaboration in Superstar Teams

# p3a-country_definitions.R

# country group definitions
# v1.1 2024-03-20
#
#######################################################




##############################
# EU by law
##############################

# # Definitions:
# 
# ## EU standard
# 
# This set of countries will belong to the eu category
# 
#  - EU
#  - UK
#  - EEA countries
#  - Cotonou agreement countries
# 
# ## EU France version
# 
#  - EU standard
#  - Uzbekistan, Kyrgyzstan
#  - all African countries, except Egypt
#  - all Oceania countries except New Zealand
#  - Kosovo, Albania, Macedonia
# 
# ## EU Spain version
# 
#  - EU standard
#  - Russia, Turkey
#  
# Maybe: all Spanish speaking south and central American countries


core_eu = c("Austria", "Italy", "Belgium", "Latvia", "Bulgaria", "Lithuania", "Croatia", "Luxembourg",
            "Cyprus", "Malta", "Czechia", "Netherlands",  "Denmark", "Poland", "Estonia", "Portugal", "Finland", 
            "Romania", "France", "Slovakia", "Germany", "Slovenia", "Greece","Spain", "Hungary", "Sweden", "Ireland", "United Kingdom",
            "England", "Czech Republic", "Scotland", "Northern Ireland",  "Andorra", "Curacao", "Aruba", "Wales", "Northern Ireland", "Faroe Islands",  
            "Guadeloupe", "French Guiana", "Réunion"  , "Jersey", "Gibraltar" ,  "Bermuda", "Montserrat", "Neukaledonien" ,  "Monaco" , "Isle of Man" , "Martinique", "Anguilla"
) # I put UK here without a better option

eea = c("Iceland", "Liechtenstein", "Norway")

cotonou = c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Chad", "Comoros", "Congo", "Cote d'Ivoire", "Eritrea", "Ethiopia", "Gabon", "Ghana", "Gambia",  "The Gambia"  ,
            "Djibouti", "Guinea", "Guinea Bissau", "Guinea Equatorial", "Kenya" , "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria", "Central African Republic", "Democratic Republic of Congo", "Republic of South Africa", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia" , "Sudan", "Swaziland", "Eswatini", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe",
            "Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Cuba", "Dominica", "Jamaica", "Grenada", "Guyana", "Haiti", "Dominican Republic", "Saint Kitts and Nevis", "St. Kitts & Nevis",  "St. Vincent & Grenadinen",
            "Saint Vincent and the Grenadines", "Saint Lucia", "St. Lucia" , "Suriname", "Trinidad and Tobago",
            "Fiji", "Cook Islands", "Marshall Islands", "Solomon Islands", "Kiribati", "Nauru", "Niue", "Palau", "Papua New Guinea", "Samoa", "Federated States of Micronesia", "Timor Leste", "Tonga", "Tuvalu", "Vanuatu")

# "Egypt", # not in France
Africa = c(
  "Algeria",  "Angola",  "Benin",  "Botswana",  "Burkina Faso",  "Burundi",  "Cape Verde",  "Cameroon",
  "Central African Republic",  "Chad",  "Comoros",  "Congo", "Democratic Republic of the Congo", "DR Congo" ,
  "Cote d'Ivoire",  "Djibouti",  "Equatorial Guinea",  "Eritrea",  "Swaziland",  "Ethiopia",  "Gabon",
  "Gambia",  "Ghana",  "Guinea",  "Guinea-Bissau",  "Kenya",  "Lesotho",  "Liberia",  "Libya",  "Madagascar",
  "Malawi",  "Mali",  "Mauritania",  "Mauritius",  "Morocco",  "Mozambique",  "Namibia",  "Niger",
  "Nigeria",  "Rwanda",  "Sao Tome and Principe",  "Senegal",  "Seychelles",  "Sierra Leone",  "Somalia",
  "South Africa",  "South Sudan",  "Sudan",  "Tanzania",  "Togo",  "Tunisia",  "Uganda",  "Zambia",  "Zimbabwe")

#without NZ
Oceania = c("Australia", "Micronesia", "Fiji", "Kiribati", "Marshall Islands", "Nauru", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu and Vanuatu")

# "Puerto Rico",
CS_America_esp = 
  c(
    "Cuba",    "Dominican Republic",    "Belize",    "Costa Rica",    "El Salvador",    "Guatemala",    "Honduras",
    "Mexico",    "Nicaragua",    "Panama",    "Argentina",    "Bolivia",    "Chile",    "Colombia",    "Ecuador",
    "Paraguay",    "Peru",    "Uruguay",    "Venezuela"
  )



eu_extra_france = c("Uzbekistan", "Kyrgyzstan", Africa, Oceania,
                    "Kosovo", "Albania", "Macedonia"
)

eu_extra_spain = c("Russia", "Turkey")
eu_maybe_spain = CS_America_esp

eu_standard = c(core_eu, eea, cotonou)
eu_standard_core = c(core_eu, eea)

# Other groups: (ex-)Federaions

yugo7 =c("Croatia","Slovenia", "Kosovo", "Serbia", "North Macedonia", "Montenegro", "Bosnia-Herzegovina")
ussr = c("Belarus", "Russia", "Ukraine",
         "Turkmenistan", "Uzbekistan", "Kazakhstan", "Kyrgyzstan", "Azerbaijan", "Armenia," ,"Georgia",
         "Moldova", "Estonia", "Latvia", "Lithuania" )
czsk = c("Slovakia", "Czech Republic")
uk_irish = c("England", "Wales","Scotland" , "Jersey", "Gibraltar" ,  "Isle of Man", "Northern Ireland", "Ireland" )

