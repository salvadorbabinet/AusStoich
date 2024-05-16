# AusStoich EDA 
# Load useful libraries 
library(tidyverse)

# Import data as a tibble and set variable types
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
tidy_data <- tidy_data |> select(Unique_ID:CP_ratio) #Post-import tidying
tidy_data

# Exploratory data analysis 
# Visualize species observation frequencies 
species <- tidy_data |> 
  count(species_binom, sort = T) |> 
  mutate(species_binom = fct(species_binom))

species |> 
  filter(species_binom != 'NA' & n >= 30) |> 
  ggplot(aes(x = species_binom, y = n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(
    title = 'Species observation frequency in AusTraits (n > 30)',
    x = 'Species', y = 'Frequency'
  )


