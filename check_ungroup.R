library(tidyverse)
library(haven)

options(scipen = 100000, digits = 4)

# Death count totals by year-sex-cause
# This is Deaths1 in the original file

raw <- read_csv("../out/data/who_mdb.csv")

og <-
  raw %>%
  filter(Country == 3300 & Deaths1 != 0 & Year < 2006) %>%
  select(Year, Sex, Cause, Deaths1) %>%
  arrange(Year, Sex, Cause)

# Max: 233,739
summary(og$Deaths1)

# Total: 19,395,355
x1 <- sum(og$Deaths1)
print(x1)

# Long data
ph <- read_csv("../out/data/who_mdb_ph.csv")

df <-
  ph %>%
  filter(agegrp != 1) %>%
  group_by(Year, Sex, Cause) %>%
  summarise(n = sum(ndeaths, na.rm = TRUE)) %>%
  ungroup()

# Same results
summary(df$n)
x2 <- sum(df$n, na.rm = TRUE)
print(x2)

# Ungrouped data
ph2 <- read_csv("../out/data/who_mdb_ph2.csv")

# Problem!!!
summary(ph2$n)
x3 <- sum(ph2$n, na.rm = TRUE)
print(x3)

flag <-
  ph2 %>%
  filter(n > max(df$n)) %>%
  select(-c("total", "px"))

View(flag)

x4 <- sum(flag$n, na.rm = TRUE)
print(x3 - x4)
