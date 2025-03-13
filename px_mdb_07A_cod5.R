library(haven)
library(readxl)
source("fn_ungroup.R")

# Extract data
df1 <- read_csv("../out/data/who_mdb_07A.csv")

# Lookup file
lkup <- read_excel("../docs/Data Description Table.xlsx",
                   sheet = "07A") 

colnames(lkup)[1] <- "icd7"

lkup <-
  lkup %>% 
  select(icd7, cod5)

# Lookup COD analysis group
df3 <-
  left_join(df1, lkup, by = join_by(Cause == icd7)) %>% 
  filter(cod5 != 0) %>% 
  group_by(Year, Sex, cod5, Frmat, agegrp) %>%
  summarize(ndeaths = sum(ndeaths)) %>% 
  ungroup() %>%
  arrange(Year, Sex, cod5, Frmat, agegrp)

# "Ungroup" death counts
ph1 <-
  df3 %>%
  group_by(Year, Sex, cod5, Frmat) %>%
  summarise(n = list(fn1(pick(ndeaths), unique(Frmat), xlist)), .groups = "drop")

write_csv(check_df, "../out/data/check_df.csv")

ph2 <-
  ph1 %>%
  unnest(n) %>%
  mutate(age = rep(0:100, length.out = n())) %>%
  rename(year = Year, sex = Sex) %>%
  select(-c(Frmat)) %>% 
  group_by(year, sex, age, cod5) %>% 
  mutate(n = sum(n)) %>% 
  distinct() %>% 
  ungroup() %>% 
  select(year, sex, age, cod5, n) %>%
  arrange(year, sex, age, cod5) %>%
  group_by(year, sex, age) %>%
  mutate(total = sum(n, na.rm = TRUE), px = n / total)

write_csv(ph2, "../out/data/px_mdb_07A_cod5.csv")
write_dta(ph2, "../out/data/px_mdb_07A_cod5.dta")
