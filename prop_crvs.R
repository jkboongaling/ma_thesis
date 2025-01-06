library(tidyverse)
library(haven)
library(ungroup)

options(scipen = 100000, digits = 4)

# Extract data
raw <- read_csv("../out/data/crvs.csv")

# Compute proportions
df <-
  raw %>%
  filter(!is.na(age)) %>%
  group_by(year, sex, age, cod) %>%
  summarize(n = n()) %>%
  mutate(total = sum(n), px = n / total)

# Check
summary(df)

# Write output
write_csv(df, "../out/data/px_crvs.csv")
write_dta(df, "../out/data/px_crvs.dta")
