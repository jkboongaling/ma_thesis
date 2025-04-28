library(tidyverse)

# Extract population data
rawp <- read_csv("../in/WPP2024_PopulationBySingleAgeSex_Medium_1950-2023.csv")

# Create age groups
brk <- c(0, 1, seq(5, 95, by = 5), Inf)
lbl <- sprintf("%02g", brk[1:21])
# lbl[21] <- "95+"
rawp$agegrp <- cut(
  rawp$AgeGrpStart,
  breaks = brk,
  labels = lbl,
  right = FALSE,
  include.lowest = TRUE
)

pdf <-
  rawp %>%
  filter(ISO3_code == "PHL", Time >= 2006) %>%
  select(Location,
         ISO3_code,
         Time,
         AgeGrpStart,
         agegrp,
         starts_with("Pop")) %>%
  rename(country_area = Location,
         iso3_code = ISO3_code,
         year4 = Time,
  ) %>%
  pivot_longer(
    cols = starts_with("Pop"),
    names_to = c("sex_code"),
    names_prefix = "Pop",
    values_to = "Value"
  ) %>%
  filter(sex_code != "Total") %>%
  mutate(sex_code = recode(sex_code, "Male" = 1, "Female" = 2)) %>%
  group_by(country_area, iso3_code, year4, sex_code, agegrp) %>%
  summarize(Value = round(1000 * sum(Value)), .groups = "drop") %>%
  pivot_wider(
    names_from = agegrp,
    names_prefix = "age_",
    names_sort = TRUE,
    values_from = Value,
    values_fill = list(Value = 0)
  ) %>%
  mutate(data_type = "population",
         icd_code = "",
         .before = sex_code) %>%
  mutate(total_num = rowSums(across(starts_with("age_"))), .before = age_00) %>%
  mutate(age_unknown = 0)

# Extract deaths data
rawd <- read_csv("../out/data/crvs.csv")

# Create age groups
brk <- c(0, 1, seq(5, 95, by = 5), Inf)
lbl <- sprintf("%02g", brk[1:21])
# lbl[21] <- "95+"
rawd$agegrp <- cut(
  rawd$age,
  breaks = brk,
  labels = lbl,
  right = FALSE,
  include.lowest = TRUE
)

ddf <-
  rawd %>%
  # filter(!is.na(age)) %>%
  group_by(year, sex, cod, agegrp) %>%
  summarize(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = agegrp,
    names_prefix = "age_",
    names_sort = TRUE,
    values_from = n,
    values_fill = list(n = 0)
  ) %>%
  mutate(
    country_area = "Philippines",
    iso3_code = "PHL",
    data_type = "mortality",
    total_num = rowSums(across(starts_with("age_"))),
    .before = year
  ) %>%
  rename(
    year4 = year,
    icd_code = cod,
    sex_code = sex,
    age_unknown = age_NA
  ) %>%
  select(country_area,
         iso3_code,
         year4,
         data_type,
         icd_code,
         sex_code,
         everything())

# Combine data
ana <- rbind(pdf, ddf)
# write_csv(ana, "../out/data/temp_anacod3.csv")

ana1 <-
  ana %>%
  filter(year4 >= 2006 & year4 <= 2010)
write_csv(ana1, "../out/data/temp_anacod3_2006.csv")

ana2 <-
  ana %>%
  filter(year4 >= 2011 & year4 <= 2015)
write_csv(ana2, "../out/data/temp_anacod3_2011.csv")

ana3 <-
  ana %>%
  filter(year4 >= 2016 & year4 <= 2020)
write_csv(ana3, "../out/data/temp_anacod3_2016.csv")

ana4 <-
  ana %>%
  filter(year4 >= 2021 & year4 <= 2023)
write_csv(ana4, "../out/data/temp_anacod3_2021.csv")
