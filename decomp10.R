library(tidyverse)
library(haven)

# Extract crvs data
df1 <- read_dta("../out/data/cod.dta")

# For df2 write px_mdb_104_cod10.R using mdb_104_cod.dta as input
# For df3 write px_mdb_09B_cod10.R using who_mdb_09B.csv as input

# Retain labels as factors
df1 <-
  df1 %>%
  select(year, sex, age, cod10, n, total, px) %>% 
  mutate(sex = as_factor(sex),
         cod10 = as_factor(cod10))

df2 <-
  df2 %>%
  mutate(sex = as_factor(sex),
         cod10 = as_factor(cod10))

levels(df2$sex) <- c("Male", "Female")

df3 <-
  df3 %>%
  mutate(sex = as_factor(sex),
         cod10 = as_factor(cod10))

levels(df3$sex) <- c("Male", "Female")
levels(df3$cod5) <- c("Infectious diseases",
                      "Cancer",
                      "Diabetes",
                      "Ischemic heart disease",
                      "Stroke",
                      "Other cardiovascular diseases",
                      "Respiratory diseases",
                      "Suicide",
                      "Injuries",
                      "Others")

# Check
levels(df1$sex)
levels(df1$cod10)
levels(df2$sex)
levels(df2$cod10)
levels(df3$sex)
levels(df3$cod10)

# Combine all data
cod <- rbind(df1, df2, df3)

# Just to ensure everything is summed up by COD analysis groups
cod10 <-
  cod %>%
  group_by(year, sex, age, cod10) %>%
  summarise(px10 = sum(px))

# Save COD groups to csv
write_csv(cod10, "../out/data/cod10.csv")

# Write decomposition