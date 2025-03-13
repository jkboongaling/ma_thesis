library(haven)
source("fn_ungroup.R")

# Extract data
raw <- read_dta("../out/data/mdb_104_cod.dta")

# y: Death counts in the age group
ph <-
  raw %>%
  group_by(Year, Sex, cod5, agegrp) %>%
  summarize(ndeaths = sum(ndeaths)) %>% 
  ungroup() %>%
  arrange(Year, Sex, cod5, agegrp)

# "Ungroup" death counts
ph1 <-
  ph %>%
  group_by(Year, Sex, cod5) %>%
  summarise(n = list(fn1(pick(ndeaths), 1, xlist)), .groups = "drop")

write_csv(check_df, "../out/data/check_df.csv")

ph2 <-
  ph1 %>%
  unnest(n) %>%
  mutate(age = rep(0:100, length.out = n())) %>%
  rename(year = Year, sex = Sex) %>%
  select(year, sex, age, cod5, n) %>%
  arrange(year, sex, age, cod5) %>%
  group_by(year, sex, age) %>%
  mutate(total = sum(n, na.rm = TRUE), px = n / total)

write_dta(ph2, "../out/data/px_mdb_104_cod5.dta")
