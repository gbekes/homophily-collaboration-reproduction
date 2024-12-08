

###############################################
# set this to your data folder

data_hub      ="C:/Users/bekes/Documents/data_hub_shared_public"

# definitions
source(paste0("wrangling/01_anon-maker/","league-filter.R"))  
source(paste0("wrangling/03_work-prep/","p3a-country_definitions.R"))             # data_dir must be first defined


data_tidy        <- paste(data_hub,"anon/", sep = "/")
data_imported    <- paste(data_hub,"raw/data-imported/", sep = "/")
data_raw         <- paste(data_hub,"raw/", sep = "/")
data_events      <- paste(data_hub,"raw/passes/", sep = "/")


# create work-data folder in data_hub
created = paste0(data_hub, "/work-data/")
if (!dir.exists(created)) {
  dir.create(created)
}
data_work        <- paste(data_hub,"work-data/", sep = "/")


created = paste0(data_raw, "data_tidy_temp/")
if (!dir.exists(created)) {
  dir.create(created)
}
data_tidy_temp      <- paste(data_hub,"raw/data_tidy_temp/", sep = "/")

created = paste0(data_tidy, "anon_created/")
if (!dir.exists(created)) {
  dir.create(created)
}
data_tidy_created        <- paste(data_hub,"anon/anon_created/", sep = "/")


created = paste0(data_raw, "anon_tables/")
if (!dir.exists(created)) {
  dir.create(created)
}
anon_tables      <- paste(data_hub,"raw/anon_tables/", sep = "/")


# the main folder for data is now work-data!
# output
output = paste0(data_work, "output/")
if (!dir.exists(output)) {
  dir.create(output)
}

output_appendix = paste0(data_work, "output_appendix/")
if (!dir.exists(output_appendix)) {
  dir.create(output_appendix)
}




