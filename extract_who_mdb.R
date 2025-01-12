library(tidyverse)
library(haven)

# Identify working directory
getwd()
parent <- normalizePath(file.path(getwd(), ".."), "/")

# Set path and filenames
path <- paste0(parent, "/in/who_mdb/data")
merged_csv <- paste0(parent, "/out/data/who_mdb.csv")
merged_dta <- paste0(parent, "/out/data/who_mdb.dta")

# If output already exists, delete first
if (file.exists(merged_csv)) file.remove(merged_csv)
if (file.exists(merged_dta)) file.remove(merged_dta)

# List files
filenames_list <- list.files(path = path, full.names = TRUE)

# Extract data from all files
all <- lapply(filenames_list, function(filename) {
  print(paste("Merging", filename, sep = " "))
  read_csv(filename)
})

df <- do.call(rbind.data.frame, all)

# PH only, include causes with nonzero deaths, 2006+ data will be from CRVS
ph <-
  df %>%
  filter(Country == 3300 & Deaths1 != 0 & Year < 2006) %>%
  group_by(Year, Sex, Cause) %>%
  pivot_longer(
    cols = starts_with("Deaths"),
    names_to = c("agegrp"),
    names_prefix = "Deaths",
    values_to = "ndeaths"
  ) %>%
  ungroup() %>%
  select(-c(starts_with("IM_"), Country, Admin1, SubDiv)) %>%
  mutate(agegrp = as.numeric(agegrp), Frmat = as.numeric(Frmat)) %>%
  arrange(Year, Sex, Cause, Frmat, agegrp)

# Write output
write_csv(ph, merged_csv)
write_dta(ph, merged_dta)
