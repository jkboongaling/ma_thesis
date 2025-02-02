# Extract GBD data for level 2 causes of death
# Available from https://vizhub.healthdata.org/gbd-results/
# Accessed: 02 Feb 2025

library(tidyverse)
library(ungroup)

options(scipen = 100000, digits = 4)

# Identify working directory
getwd()
parent <- normalizePath(file.path(getwd(), ".."), "/")

# Set path and filenames
path <- paste0(parent, "/in/gbd")
csv <- paste0(path, "/IHME-GBD_2021_DATA-c7e29050-1.csv")
merged_csv <- paste0(parent, "/out/data/gbd_cod_l2.csv")

# Extract data
df0 <- read_csv(csv) %>%
  filter(cause_id != 294) %>%  # Remove "all causes"
  select(location_id, sex_id, year, age_name, cause_id, cause_name, val) %>%
  rename(loc = location_id,
         sex = sex_id,
         age = age_name,
         n = val) %>%
  mutate(
    age = case_match(
      age,
      "<1 year"      ~ 0,
      "12-23 months" ~ 1,
      "2-4 years"    ~ 2,
      "5-9 years"    ~ 5,
      "10-14 years"  ~ 10,
      "15-19 years"  ~ 15,
      "20-24 years"  ~ 20,
      "25-29 years"  ~ 25,
      "30-34 years"  ~ 30,
      "35-39 years"  ~ 35,
      "40-44 years"  ~ 40,
      "45-49 years"  ~ 45,
      "50-54 years"  ~ 50,
      "55-59 years"  ~ 55,
      "60-64 years"  ~ 60,
      "65-69 years"  ~ 65,
      "70-74 years"  ~ 70,
      "75-79 years"  ~ 75,
      "80-84 years"  ~ 80,
      "85-89 years"  ~ 85,
      "90-94 years"  ~ 90,
      "95+ years"    ~ 95
    ),
    cod5 = case_match(
      cause_id,
      344 ~ 3,
      526 ~ 5,
      542 ~ 5,
      717 ~ 4,
      386 ~ 5,
      410 ~ 2,
      558 ~ 5,
      626 ~ 5,
      640 ~ 5,
      653 ~ 5,
      955 ~ 3,
      956 ~ 3,
      957 ~ 3,
      961 ~ 3,
      962 ~ 5,
      973 ~ 5,
      974 ~ 5,
      491 ~ 1,
      508 ~ 5,
      688 ~ 4,
      696 ~ 4,
    )
  ) %>%
  arrange(loc, year, sex, cod5, age) %>%
  group_by(loc, year, sex, cod5, age) %>%
  summarise(n =  sum(n), .groups = "drop")

# "Ungroup" number of deaths
df1 <-
  df0 %>%
  group_by(loc, year, sex, cod5) %>%
  summarise(n = list(pclm(age[1:22], n[1:22], 6)$fitted), .groups = "drop") %>%
  unnest(cols = c(n)) %>%
  mutate(age = rep(0:100, length.out = n()), .before = n) %>%
  distinct()

# Compute proportions
df2 <-
  df1 %>%
  group_by(loc, year, sex, age) %>%
  mutate(total = sum(n, na.rm = TRUE), px = n / total) %>% 
  arrange(loc, year, sex, age, cod5)

# Write output
write_csv(df2, "../out/data/gbd_cod5.csv")