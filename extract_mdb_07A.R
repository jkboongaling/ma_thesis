library(tidyverse)
library(haven)

# Extract data
raw <- read_csv("../out/data/who_mdb.csv")

# Transform
df <-
  raw %>%
  filter(List == "07A") 

# Check
summary(df)

# Identify unique codes in the data
cod_list <- levels(as_factor(df$Cause))
write_csv(as.data.frame(cod_list), "../out/data/07A_list.csv")

# Write output
write_csv(df, "../out/data/who_mdb_07A.csv")
write_dta(df, "../out/data/who_mdb_07A.dta")
