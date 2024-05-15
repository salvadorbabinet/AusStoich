# AusStoich EDA 
# Load useful libraries 
library(tidyverse)

# Import dataset as a tibble and set variable types
raw_data <- read_csv('austraits_leaf_stoichiometry_MASTER_v1.0_10-05-2024.csv') 
raw_data #For reference

tidy_data <- read_csv(
  file = 'austraits_leaf_stoichiometry_MASTER_v1.0_10-05-2024.csv',
  col_types = cols(
    woodiness = col_factor(c('herbaceous', 'woody')),
    reclass_life_history = col_factor(c('short', 'long'))
    )
  )
tidy_data

# Post-import data tidying 
glimpse(raw_data)
raw_data |> count(myc_type)