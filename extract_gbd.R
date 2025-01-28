library(tidyverse)
library(readxl)
library(haven)
library(ungroup)
library(DemoTools)
source("fn_life_table.R")

options(scipen = 100000, digits = 4)

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

# From DemoTools
# Apply lt_single_qx
life_table_results <- pmap(combinations, function(loc, year, sex) {
  print(paste("Computing LTs for:", loc, year, sex, "...", collapse = " "))
  qx_values <- df1 %>%
    filter(loc == !!loc, year == !!year, sex == !!sex) %>%
    pull(qx)
  print(length(qx_values))
  # Check if there are valid qx values (non-empty)
  if (length(qx_values) > 0) {
    result <- lt_single_qx(nqx = qx_values, Age = 0:101, sex = if_else(sex == 1, "m", "f"))
    result$loc <- loc
    result$year <- year
    result$sex <- sex
    return(result)
  } else {
    return(NULL)
  }
})

# Combine the results into a single dataframe, excluding NULL results
df <- bind_rows(life_table_results) %>% 
  as_tibble() %>% 
  select(loc, year, sex, everything()) %>% 
  rename(age = Age, mx = nMx, qx = nqx, dx = ndx, Lx = nLx, ax = nAx) %>% 
  mutate(px = 1- qx, .before = lx) %>% 
  select(-ax, everything(), ax, -AgeInt)
  
# Write output
write_csv(df, "../out/data/gbd_lt_1x1_demotools.csv")
