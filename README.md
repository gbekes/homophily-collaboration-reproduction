# Reproduction notes

**Reproduction Guide for "Cultural Homophily and Collaboration in Superstar Teams" By Gábor Békés (CEU) and Gianmarco Ottaviano (Bocconi), Management Science (forthcoming)**

## Overview

This document provides guidance for reproducing the results in "Cultural Homophily and Collaboration in Superstar Teams". The reproduction package contains code for data processing, analysis, and figure/table generation.


## Rights

### The raw data is not shared publicly.

The raw data is not shared publicly. It is instead aimed at allowing reproduction. 

Most of the data used in this research had been webscraped. *Under Austrian law, the reproduced (scraped) data can be used for research but may not be shared publicly. The data may however be made accessible to a clearly defined group of individuals for their joint scientific research or to individual third parties for the purpose of verifying the quality of scientific research, as long as this is justified for non-commercial purposes.*

### Anonymized data is shared

We share anonymized or aggregated data publicly as well as all data wrangling codes. 

# Software, codes

## Software needs

### Base

The codes were prepared and tested with R version 4.3.3 (2024-02-29 ucrt) -- "Angel Food Cake", running on Windows 11, X64 platform, 32GB RAM. 

### R
The R environment is captured with 'renv' and the list of packages are available in the codebase in the '/info' folder. 


## Repository Structure

The codebase is organized into four main components:

1. **Data Preparation (01-raw-to-anon-maker)**
   - Creates anonymized player and team datasets
   - Processes raw data into analysis-ready formats
   - Key file: `01-raw-to-anon-maker_rev.R`

2. **Base Data Processing (02-base-prep-all)**
   - Creates player and team level datasets
   - Handles player careers, languages, and EU status
   - Key file: `02-base-prep-all_anon_rev.R`

3. **Analysis Data Preparation (03-work-prep-all)**
   - Creates the final working dataset
   - Generates key variables and team-level aggregates
   - Key file: `03-work-prep-all_rev.R`

4. **Analysis Scripts**
   - Descriptive statistics: `01_descriptives.R`
   - Main regressions: `02_regressions_mansci.R`
   - Online appendix: `03_online_appendix_mansci.R`


## Reproduction Steps

1. **Environment Setup**
   - Install required R packages listed in `d00_setup.R`
   - Set data directory in `d01_paths.R`

2. **Data Processing**
   ```R
   # Run data preparation scripts in sequence
   source("01-raw-to-anon-maker_rev.R")
   source("02-base-prep-all_anon_rev.R")
   source("03-work-prep-all_rev.R")
   ```

3. **Analysis**
   ```R
   # Run analysis scripts
   source("01_descriptives.R")
   source("02_regressions_mansci.R")
   source("03_online_appendix_mansci.R")
   ```

## Output Files

The code generates several types of output:
- Regression tables (LaTeX format)
- Figures (PDF format)
- Summary statistics tables
- Intermediate data files for analysis

Key output locations:
- Main tables/figures: `/output/`
- Appendix materials: `/output_appendix/`

## Runtime Considerations

- Full reproduction takes several hours
- Data processing scripts are memory-intensive
- Recommended: Run on a machine with at least 16GB RAM

## Package Dependencies

Key R packages required:
- Data handling: tidyverse, arrow, haven
- Analysis: fixest, splines
- Visualization: ggplot2

## Notes for Reviewers

1. The code includes some error checking and robustness tests
2. Anonymization procedures are implemented to protect player identities
3. The analysis can be run in parts using the modular structure
4. Results are designed to be fully reproducible given the same data inputs

## Support

- Corresponding author: Gábor Békés (bekesg@ceu.edu)
- Scripts writen by Bence Szabó, Gábor Békés with help from Endre Borza

# Dataset details

## Overview

The analysis requires several data sources:
- Player-level event data (passes, positions)
- Team composition data
- Player background information (nationality, languages)
- Market value data
- Cultural distance measures

Note: Some raw data files are not publicly shared but can be accessed via reproduction requests.


## Raw data (gated) 

### How to use

* Download and save in a folder. It's about 1GB
* Extract all zip files in folders
* set your data path in `d01_paths.R` such as  

```
data_hub      ="E/data_hub_shared_gated"
```

Run the data wrangling phases 1 (anonymization) 2 and 3 (creating work data) scripts and the analysis reproduction scripts


### Input dataset Listing

|  Path | Filename | Extension | Source | Content Description |
| ---------------- | ------------ | ---------- | ------------------- | ----------------------------------------- |
| /raw/passes | england_premier-league_2017-2018.parquet | parquet | whoscored.com | Detailed pass-by-pass data for specific league season |
|/raw/onfield_data | onfield-england_premier-league_2017-2018.parquet | parquet | whoscored.com |  which players are on the field together during matches. |
| /raw/t2_raw/tm | player_info.parquet | parquet | transfermarkt | Comprehensive player biographical and career information |
| /raw/t2_raw/tm | player_values.parquet | parquet | transfermarkt | Market valuations for players over time |
| /raw/t2_raw/tm | team_relations.parquet | parquet | transfermarkt | Relationships between teams (e.g., main team and youth/reserve teams) |
| /raw/t2_raw/tm | player_transfers.parquet | parquet | transfermarkt | Complete transfer history of players between teams |
| /raw/t2_raw/tm | team_info.parquet | parquet | transfermarkt | Team metadata including location, country, and basic information |
| /raw/t2_raw/wh | formation_use.parquet | parquet | whoscored.com | Team formations, lineups  |
| /raw/t2_raw/wh | formations.parquet | parquet | whoscored.com | Team formations, lineups |
| /raw/t2_raw/wh | matches.parquet | parquet | whoscored.com | Match-level data including results and statistics |
| /raw/t2_raw/wh | seasons.parquet | parquet | whoscored.com | Season-level competition information |
| /raw/t2_raw/wh | teams.parquet | parquet | whoscored.com | Teams core information to match |
| /raw/t3 | /layer_fixtures.parquet | parquet | whoscored.com |  participation data including lineups and player match involvements |
| /raw/t3 |team_fixtures.parquet | parquet | whoscored.com |  participation data including lineups and player match involvements |
| /raw/corefs | players_coref.parquet | parquet | own  | Player name variations and identity mapping |
| /raw/data-imported | cepii-gravdata-2015.dta | dta | CEPII | Gravity model variables for international trade analysis |
| /raw/data-imported | country_culture_all.xls | excel | CEPII | Cultural dimensions and indices by country |
| /raw/data-imported/culture | codes.dta | dta | CEPII | Country and language iso codes lookup |
| /raw/data-imported/culture | ling_web_total.dta | dta | Melitz-Toubal | Linguistic distance measures between languages |
| /raw/culture | wvs-wave7-bekes_euc.txt | txt | WWS | World Value Survey country aggregates |

## Publicly shared data

### Raw Data Files
|  Path | Filename | Extension | Source | Content Description |
|-----------|----------|-----------|--------|-------------------|
| raw/data_imported | countries_languages_lim | csv | external | Country-language pairs with weights |
| raw/data_imported | countries | parquet | external | Country codes and names |
| raw/data_imported | ling_to_append | csv | external | Additional language relationships |
| raw/data_imported | cepii-gravdata-2015 | dta | external | Country pair characteristics (distance, colony) |
| raw/data_imported/culture | ling_web_total | dta | external | Language similarity measures |
| raw/data_imported/culture | codes | dta | external | Country code mappings |
| raw/data_imported/culture | wvs-wave7-bekes_euc | txt | external | World Values Survey cultural distances |
| raw/t2_raw/wh | seasons | parquet | external | Season definitions and metadata |

### Anonymized Source Files
|  Path | Filename | Extension | Source | Content Description |
|-----------|----------|-----------|--------|-------------------|
| anon | anon_players | parquet | wrangling/01 | Core player characteristics |
| anon | anon_player_info | parquet | wrangling/01 | Additional player information |
| anon | anon_player_values | parquet | wrangling/01 | Player market values over time |
| anon | anon_player_transfers | parquet | wrangling/01 | Player transfer history |
| anon | anon_matches | parquet | wrangling/01 | Match details and results |
| anon | anon_team_relations | parquet | wrangling/01 | Team hierarchy relationships |
| anon | anon_team_info | parquet | wrangling/01 | Team characteristics |
| anon | anon_lineups | parquet | wrangling/01 | Match lineup information |
| anon | anon_all_player_minutes | parquet | wrangling/01 | Minutes played by player per match |

### Game/League Data Files
|  Path | Filename | Extension | Source | Content Description |
|-----------|----------|-----------|--------|-------------------|
| anon | anon_all_culture_season_half | parquet | wrangling/01 | Cultural composition by team-season |
| anon | anon_player_passcounts_shared_minutes_major_leagues | parquet | wrangling/01 | Pass counts between players during shared time |
| anon | anon_all_events_agg_major_leagues | parquet | wrangling/01 | Aggregated match events and statistics |
| anon | anon_formation_use | parquet | wrangling/01 | Formation and player position data by match |



## Output data

`anon_work-regressions.RData` 

Observations (N = 669,022): anonymized teams X anonymized passer X anonymized receiver X half-seasons. 

Variables (K = 81): Contains the final analysis table used in the descriptive analysis and the regressions, with variables describing pass counts, intensity, cultural distance, and person-level observable characteristics. 

## Updates

Update: 2024-11-30. See Changelog
