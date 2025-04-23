library(tidyverse)
library(haven)
library(kableExtra)
library(data.table)

options(scipen = 100000, digits = 4)

################################################################################
# Data preparation

# Extract crvs data
df1 <- read_dta("../out/data/cod.dta")
df2 <- read_dta("../out/data/px_mdb_104_cod10.dta")
df3 <- read_dta("../out/data/px_mdb_09B_cod10.dta")

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
levels(df3$cod10) <- c("Infectious diseases",
                       "Neoplasms",
                       "Diabetes",
                       "Ischemic heart disease",
                       "Stroke",
                       "Other cardiovascular diseases",
                       "Respiratory diseases",
                       "Suicide",
                       "Other external causes",
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

################################################################################
# Clear environment
rm(list=ls())

# LE Decomposition (Cause)
source("fn_decomp.R")

# Inputs
yr1 <- 1993
yr2 <- 2019
s1  <- "Female"
s2  <- "f"

lt <- read_csv("../out/data/wpp_lt_1x1_rfunc.csv")

lt1 <-
  lt %>%
  filter(loc == 608, year == yr1, sex == s1) 

lt2 <-
  lt %>%
  filter(loc == 608, year == yr2, sex == s1) 

ex_diff <- lt2$ex[1] - lt1$ex[1]
ex_decomp1 <- arriaga(lt1$mx, lt2$mx, sex = s2, breakdown = F)

cod10 <- read_csv("../out/data/cod10.csv")

cod <- 
  cod10 %>% 
  pivot_wider(names_from = cod10, values_from = px10)

cause_prop1 <-
  cod %>% 
  filter(year == yr1, sex == s1) %>% 
  select(c(4:13))

cause_prop2 <-
  cod %>% 
  filter(year == yr2, sex == s1) %>% 
  select(c(4:13))

# Replace NAs (if any) with zero
na_mask1 <- is.na(cause_prop1)
na_mask2 <- is.na(cause_prop2)

cause_prop1[na_mask1] <- 0
cause_prop2[na_mask2] <- 0

cat("Number of NAs replaced in cause_prop1:", sum(na_mask1), "\n")
cat("Number of NAs replaced in cause_prop2:", sum(na_mask2), "\n")

# Fix this!
# Temporary only, this simply copies values at age 98 to 99 & 100
if (yr1 >= 2006) {
  cause_prop1 <- bind_rows(cause_prop1, cause_prop1[99, ], cause_prop1[99, ])
}
if (yr2 >= 2006) {
  cause_prop2 <- bind_rows(cause_prop2, cause_prop2[99, ], cause_prop2[99, ])
}

## Now we construct the cause-specific factor.
cause_fac1 <-
  (cause_prop1 * lt1$mx) /
  ifelse((lt2$mx - lt1$mx) == 0, 1, lt2$mx - lt1$mx) * ex_decomp1

cause_fac2 <-
  (cause_prop2 * lt2$mx) /
  ifelse((lt2$mx - lt1$mx) == 0, 1, lt2$mx - lt1$mx) * ex_decomp1

# Same results as the Arriaga method by age
sum(cause_fac2 - cause_fac1)

cause_mat <- cause_fac2 - cause_fac1

table <- matrix(round(
  c(lt2$ex[1],
    lt1$ex[1],
    sum(ex_decomp1),
    sum(cause_mat[,1]),
    sum(cause_mat[,2]),
    sum(cause_mat[,3]),
    sum(cause_mat[,4]),
    sum(cause_mat[,5]),    
    sum(cause_mat[,6]),
    sum(cause_mat[,7]),
    sum(cause_mat[,8]),
    sum(cause_mat[,9]),
    sum(cause_mat[,10]),
    sum(cause_mat)), 2))

row.names(table) <- c(
  paste0("Life expectancy at birth for PH ", str_to_lower(s1), "s in ", yr2),
  paste0("Life expectancy at birth for PH ", str_to_lower(s1), "s in ", yr1),
  "Life expectancy difference",
  "Infectious diseases",
  "Neoplasms",
  "Diabetes",
  "Ischemic heart disease",
  "Stroke",
  "Other cardiovascular diseases",
  "Respiratory diseases",
  "Suicide",
  "Other external causes",
  "Other causes",
  "Estimated total difference from decomposition")
colnames(table) <- paste0("PH ", s1, "s")
kable(table, caption = "Arriaga Decomposition by Cause")

total_dat <- data.frame(Age = 0:100, Value = ex_decomp1)

cause_dat <- data.frame(
  Age = rep(0:100, 10),
  Cause = rep(
    c("Infectious diseases",
      "Neoplasms",
      "Diabetes",
      "Ischemic heart disease",
      "Stroke",
      "Other cardiovascular diseases",
      "Respiratory diseases",
      "Suicide",
      "Other external causes",
      "Other causes"),
    each = 101
  ),
  Value = c(cause_mat[, 1], cause_mat[, 2], cause_mat[, 3], cause_mat[, 4], cause_mat[, 5],
            cause_mat[, 6], cause_mat[, 7], cause_mat[, 8], cause_mat[, 9], cause_mat[, 10])
)
