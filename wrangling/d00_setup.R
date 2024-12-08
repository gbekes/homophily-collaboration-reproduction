# setup
# first time: install needed packages

#library

library(arrow, warn.conflicts = FALSE)
library(haven)
library(skimr)
library(modelsummary)
library(summarytools) 
library(janitor)
library(pander)
library(kableExtra)

library(naniar) 
library(sur) 
library(scales)

library(fixest)
library(splines)
library(binsreg)



library(beepr)
library(crayon)  # for terminal colors

#library(tidylog)

crayon <- function(x) cat(magenta(x), sep = "\n")

#options("tidylog.display" = list(crayon))
#options("tidylog.display" = list())  # turn off

#library(tidytable)
library(plyr)
library(tidyverse)
library(broom)
library(magrittr) 

#install.packages("sqldf")
library(sqldf)


# functions
create_output_if_doesnt_exist<- function(folder){
  if (!dir.exists(folder)){
    dir.create(folder)
    print(paste(folder,"folder created."))}
  else{
    print(paste(folder,"folder already exists"))}
}


