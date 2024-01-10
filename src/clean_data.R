# script to create a clean df with selected columns for dashboard
# check "../docs/eda_dataset.ipynb" for details of selection and filtering

library(tidyverse)

df_ori <- read_delim("data/street-trees.csv",
                     delim = ";")

df <- df_ori |>
  select(GENUS_NAME, NEIGHBOURHOOD_NAME, HEIGHT_RANGE, DIAMETER) |>
  filter(DIAMETER > 0) |>
  janitor::clean_names() |>
  mutate(neighbourhood_name = str_to_title(neighbourhood_name)) |>
  mutate(genus_name = str_to_title(genus_name)) |>
  mutate(neighbourhood_name = str_replace(neighbourhood_name, "Arbutus Ridge", "Arbutus-Ridge"))

write_csv(df, "data/street_trees_cleaned.csv")