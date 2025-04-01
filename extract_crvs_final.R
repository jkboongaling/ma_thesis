library(tidyverse)
library(readxl)
library(haven)

# Identify working directory
getwd()
parent <- normalizePath(file.path(getwd(), ".."), "/")

# Set path and filenames
path <- paste0(parent, "/in/crvs/data")
merged_csv <- paste0(parent, "/out/data/crvs.csv")
merged_dta <- paste0(parent, "/out/data/crvs.dta")

# If output already exists, delete first
if (file.exists(merged_csv)) file.remove(merged_csv)
if (file.exists(merged_dta)) file.remove(merged_dta)

# List xlsx and csv files
filenames_list <- list.files(path = path,
                             pattern = "\\.xlsx$|\\.csv$",
                             full.names = TRUE)

# List columns needed
col_list <- c(
  "date_of_death_year",
  "sex",
  "age",
  "cause_of_death_3digit",
  "icd_code",
  "icd_code_3d"
)

# List how to rename columns
rn_map <- c(
  year = "date_of_death_year",
  cod = "cause_of_death_3digit",
  cod = "icd_code",
  cod = "icd_code_3d"
)

# Extract data from all files
all <-
  lapply(filenames_list, function(filename) {
    print(paste("Merging", filename, sep = " "))
    if (grepl("\\.xlsx$", filename)) {
      df <- read_excel(filename)
    } else {
      df <- read_csv(filename)
    }
    ex <- intersect(col_list, names(df))
    df <- df %>% select(all_of(ex)) %>% rename(any_of(rn_map))
  })

df <- do.call(rbind.data.frame, all)

df <-
  df %>%
  mutate_at(c("sex", "age"), as.numeric) %>%
  mutate(
    age = case_when(age <= 200 ~ 0, age >= 201 & age <= 298 ~ age - 200, TRUE ~ NA),
    cod = substr(cod, 1, 4)
  ) %>%
  arrange(year, sex, age, cod)

# Write output
write_csv(df, merged_csv)
write_dta(df, merged_dta)
