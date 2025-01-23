library(tidyverse)

# Extract population data
rawp <- read_csv("../in/un_wpp_popn_ph.csv")

pdf <-
  rawp %>%
  select(Location, Iso3, Time, SexId, AgeStart, Value) %>%
  rename(
    country_area = Location,
    iso3_code = Iso3,
    year4 = Time,
    sex_code = SexId,
    age = AgeStart,
    popn = Value
  ) %>%
  mutate(data_type = "population",
         icd_code = "",
         .before = sex_code) %>%
  pivot_wider(
    names_from = age,
    names_prefix = "age_",
    names_sort = TRUE,
    values_from = popn,
    values_fill = list(n = 0)
  ) %>%
  mutate(total_num = rowSums(across(starts_with("age_"))), .before = age_0) %>%
  mutate(age_95 = age_95 + age_100, age_unknown = 0) %>%
  select(-age_100)
