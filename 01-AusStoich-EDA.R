# AusStoich EDA 
# Load useful libraries 
library(tidyverse)

# Import dataset as a tibble and set variable types
raw_data <- read_csv('austraits_leaf_stoichiometry_MASTER_v1.0_10-05-2024.csv') 
raw_data #For reference

tidy_data <- read_csv(
  file = 'austraits_leaf_stoichiometry_MASTER_v1.0_10-05-2024.csv',
  na = c('', 'NA', 'uncertain'),
  col_types = cols(
    woodiness = col_factor(c('herbaceous', 'woody')),
    reclass_life_history = col_factor(c('short', 'long')),
    putative_BNF = col_factor(c('0', '1')),
    myc_type = col_factor(c('AM', 'EcM', 'EcM-AM', 'ErM', 'NM', 'NM-AM'))
    )
  )
tidy_data <- tidy_data |> select(Unique_ID:CP_ratio)
tidy_data

