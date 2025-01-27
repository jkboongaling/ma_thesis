library(tidyverse)
library(readxl)
library(haven)
library(ungroup)
source("fn_life_table.R")

# Identify working directory
getwd()
parent <- normalizePath(file.path(getwd(), ".."), "/")

# Set path and filenames
path <- paste0(parent, "/in/gbd")
csv <- paste0(path, "/IHME-GBD_2021_DATA-d0c38bb7-1.csv")
merged_csv <- paste0(parent, "/out/data/gbd_qx_5x1.csv")

# If output already exists, delete first
if (file.exists(merged_csv)) file.remove(merged_csv)

# Extract data
df0 <- read_csv(csv) %>%
  select(location_id, sex_id, year, age_name, val) %>%
  rename(loc = location_id,
         sex = sex_id,
         age = age_name,
         qx = val) %>%
  mutate(
    age = case_match(
      age,
      "<1 year"     ~ 0,
      "2-4 years"   ~ 2,
      "5-9 years"   ~ 5,
      "10-14 years" ~ 10,
      "15-19 years" ~ 15,
      "20-24 years" ~ 20,
      "25-29 years" ~ 25,
      "30-34 years" ~ 30,
      "35-39 years" ~ 35,
      "40-44 years" ~ 40,
      "45-49 years" ~ 45,
      "50-54 years" ~ 50,
      "55-59 years" ~ 55,
      "60-64 years" ~ 60,
      "65-69 years" ~ 65,
      "70-74 years" ~ 70,
      "75-79 years" ~ 75,
      "80-84 years" ~ 80,
      "85-89 years" ~ 85,
      "90-94 years" ~ 90,
      "95+ years"   ~ 95
    )) %>%
  arrange(loc, sex, year, age)

# "Ungroup" qx
df1 <-
  df0 %>%
  group_by(loc, sex, year) %>%
  summarise(qx = list(pclm(age[1:21], qx[1:21], 6)$fitted), .groups = "drop") %>% 
  unnest(cols = c(qx)) %>% 
  mutate(age = rep(0:100, length.out = n()), .before = qx)

# Prepare unique combinations of year, loc, and sex
combinations <- df1 %>%
  distinct(loc, year, sex)

# Create LifeTableQx function
