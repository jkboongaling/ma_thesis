library(haven)
library(readxl)
source("fn_ungroup.R")

# Extract data
df1 <- read_csv("../out/data/who_mdb_09B.csv")

# Lookup file
lkup <- read_excel("../docs/Data Description Table.xlsx",
                   sheet = "09B") 

colnames(lkup)[1] <- "icd9"

lkup <-
  lkup %>% 
  select(icd9, cod5)

# Lookup COD analysis group
df2 <-
  left_join(df1, lkup, by = join_by(Cause == icd9)) %>% 
  filter(cod5 != 0) %>% 
  group_by(Year, Sex, cod5, agegrp) %>%
  summarize(ndeaths = sum(ndeaths)) %>% 
  ungroup() %>%
  arrange(Year, Sex, cod5, agegrp)

# "Ungroup" death counts
ph1 <-
  df2 %>%
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

write_dta(ph2, "../out/data/px_mdb_09B_cod5.dta")
