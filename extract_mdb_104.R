library(tidyverse)
library(haven)

# Extract data
raw <- read_csv("../out/data/who_mdb.csv")

# Transform
df <-
  raw %>%
  filter(List == "104") %>%
  mutate(Cause = substr(Cause, 1, 3)) %>% 
  group_by(Year, Sex, agegrp, Cause) %>%
  summarize(ndeaths = sum(ndeaths))

# Check
summary(df)

# Write output
write_csv(df, "../out/data/who_mdb_104.csv")
write_dta(df, "../out/data/who_mdb_104.dta")
