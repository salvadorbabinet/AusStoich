# AusStoich EDA 
# Libraries & functions
library(tidyverse)

# Takes tibble tb, variable x; returns number of missing values (for reference)
count_NA <- function(tb, x) {
  tb |> 
    filter(is.na({{x}})) |> 
    count({{x}})
}

# Takes tibble tb, variable x; returns factorized count table for x
count_table <- function(tb, x) { 
  tb |> 
    count({{x}}, sort = T) |>
    filter({{x}} != 'NA') |> 
    mutate(name = fct({{x}})) |> 
    select(n:name) |> relocate(name)
}


# Data import & tidying ---------------------------------------------------
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
  select(Unique_ID:LATLONG) |> 
  filter(!is.na(Unique_ID)) |> 
  relocate(species_binom, .after = genus) 

tidy_data


# Observation frequencies across taxa -------------------------------------
family <- count_table(tidy_data, family) 
genus <- count_table(tidy_data, genus) 
species <- count_table(tidy_data, species_binom) 

print(species, n = 20) 

species |> #All species
  ggplot(aes(x = name, y = n)) +
  geom_col() + theme(axis.text.x = element_blank()) + 
  labs(
    title = 'Species observation frequency in AusTraits',
    x = 'Species', y = 'Frequency'
  )

species |> #Only species above a given frequency threshold 
  filter(n >= 30) |> 
  ggplot(aes(x = name, y = n)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90)) + 
  labs(
    title = 'Species observation frequency in AusTraits (n > 30)',
    x = 'Species', y = 'Frequency'
    )


# Variation ---------------------------------------------------------------
tidy_data |> ggplot(aes(x = leaf_N_per_dry_mass)) +
  geom_histogram(bins = 80) +
  labs(
    title = 'Foliar N concentration across all samples',
    x = 'Foliar N per dry mass', y = 'Frequency'
  )

tidy_data |> ggplot(aes(x = leaf_P_per_dry_mass)) +
  geom_histogram(bins = 80) +
  labs(
    title = 'Foliar P concentration across all samples',
    x = 'Foliar P per dry mass', y = 'Frequency'
  )

tidy_data |> ggplot(aes(x = leaf_C_per_dry_mass)) +
  geom_histogram(bins = 80) +
  labs(
    title = 'Foliar C concentration across all samples',
    x = 'Foliar C per dry mass', y = 'Frequency'
  )

outliers_n <- tidy_data |> 
  filter(leaf_N_per_dry_mass > 50) |> 
  arrange(desc(leaf_N_per_dry_mass)) 
View(outliers_n) 


# Co-variation ------------------------------------------------------------
# Summarize ratios 
tidy_data |> #TD - Convert to general summary function & iterate 
  summarize(
    np = mean(NP_ratio, na.rm = T), #TD - Use geometric means? Isles 2020
    cn = mean(CN_ratio, na.rm = T),
    cp = mean(CP_ratio, na.rm = T),
    .by = woodiness
  )

# Example density curve 
tidy_data |> ggplot(aes(x = CN_ratio, y = after_stat(density))) +
  geom_freqpoly(aes(linetype = woodiness)) +
  labs(
    title = 'C:N density curve across woodiness levels',
    x = 'C:N Ratio', y = 'Density'
  )

# Example scatter plot 
tidy_data |> ggplot(aes(x = CN_ratio, y = CP_ratio)) +
  geom_point(alpha = 0.4) +
  labs(
    title = 'Relationship between C:N & C:P across all samples',
    x = 'C:N Ratio', y = 'C:P Ratio'
  )

