library(tidyverse)
library(haven)

# Extract data
raw <- read_csv("../out/data/crvs.csv")

# Create age groups
brk <- c(0, 1, seq(5, 95, by = 5), Inf)
lbl <- sprintf("%02g", brk[1:21])
# lbl[21] <- "95+"
raw$agegrp <- cut(raw$age, breaks = brk, labels = lbl,
                  right = FALSE, include.lowest = TRUE)

write_csv(raw, "temp_anacod3.csv")

df <-
  raw %>%
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
  mutate(country_area = "Philippines",
         iso3_code = "PHL",
         data_type = "Mortality",
         total_num = rowSums(across(starts_with("age_"))),
         .before = year) %>% 
  rename(year4 = year,
         icd_code = cod,
         sex_code = sex,
         age_unknown = age_NA) %>% 
  select(country_area, iso3_code, year4, data_type, icd_code, sex_code, everything())


write_csv(df, "temp_anacod3.csv")
