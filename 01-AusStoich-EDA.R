# AusStoich EDA 
# Load useful libraries 
library(tidyverse)

# Import & tidy data as a tibble 
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

tidy_data <- tidy_data |> 
  select(Unique_ID:CP_ratio) |> 
  relocate(species_binom, .after = genus) 

tidy_data

# Observation frequencies across taxa 
count_table <- function(df, x) { 
  df |> 
    count({{x}}, sort = T) |> 
    filter({{x}} != 'NA') |> 
    mutate(name = fct({{x}})) |> 
    select(n:name) |> 
    relocate(name)
}

family <- count_table(tidy_data, family) 
genus <- count_table(tidy_data, genus) #For reference, look into iterating across these
species <- count_table(tidy_data, species_binom) 

species |> #All species
  ggplot(aes(x = name, y = n)) +
  geom_col() + theme(axis.text.x = element_blank()) + 
  labs(
    title = 'Species observation frequency in AusTraits',
    x = 'Species', y = 'Frequency'
  )

print(species, n = 30) 

species |> #Only species above a given frequency threshold 
  filter(n >= 30) |> 
  ggplot(aes(x = name, y = n)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90)) + 
  labs(
    title = 'Species observation frequency in AusTraits (n > 30)',
    x = 'Species', y = 'Frequency'
    )

# Variation 


# Co-variation 

