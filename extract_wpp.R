library(tidyverse)
library(readxl)
library(haven)

# Identify working directory
getwd()
parent <- normalizePath(file.path(getwd(), ".."), "/")

# Set path and filenames
path <- paste0(parent, "/in/wpp/lt")
xlsx_m <- paste0(path,
                 "/WPP2024_MORT_F06_2_SINGLE_AGE_LIFE_TABLE_ESTIMATES_MALE.xlsx")
xlsx_f <- paste0(path,
                 "/WPP2024_MORT_F06_3_SINGLE_AGE_LIFE_TABLE_ESTIMATES_FEMALE.xlsx")
merged_csv <- paste0(parent, "/out/data/wpp_lt_1x1.csv")

# If output already exists, delete first
if (file.exists(merged_csv)) file.remove(merged_csv)

# Extract data for males
mlt <-
  excel_sheets(xlsx_m) %>%
  head(-1) %>%
  set_names() %>%
  map(~ {
    # Column E
    df_e <-read_xlsx(
      xlsx_m,
      sheet = .x,
      range = cell_limits(c(18, 5), c(NA, 5)),
      col_names = c("loc"))
    # Column K to W  
    df_kw <- read_xlsx(
    xlsx_m,
    sheet = .x,
    range = cell_limits(c(18, 11), c(NA, NA)),
    col_names = c("year", "x", "n", "mx", "qx", "px", "lx", 
                  "dx", "Lx", "Sx", "Tx", "ex", "ax"),
    na = c("", "..."))
    # Combine
    df <- bind_cols(df_e, df_kw)
    return(df)
    }) %>%
  bind_rows() %>% 
  mutate(sex = 1, .before = x)

# Extract data for females
flt <-
  excel_sheets(xlsx_f) %>%
  head(-1) %>%
  set_names() %>%
  map(~ {
    # Column E
    df_e <-read_xlsx(
      xlsx_f,
      sheet = .x,
      range = cell_limits(c(18, 5), c(NA, 5)),
      col_names = c("loc"))
    # Column K to W  
    df_kw <- read_xlsx(
      xlsx_f,
      sheet = .x,
      range = cell_limits(c(18, 11), c(NA, NA)),
      col_names = c("year", "x", "n", "mx", "qx", "px", "lx", 
                    "dx", "Lx", "Sx", "Tx", "ex", "ax"),
      na = c("", "..."))
    # Combine
    df <- bind_cols(df_e, df_kw)
    return(df)
  }) %>%
  bind_rows() %>% 
  mutate(sex = 2,  .before = x)

# Combine sexes and filter PH and SEA
all <- rbind(mlt, flt) %>%
  filter(loc %in% c(608, 920))

# Write output
write_csv(all, merged_csv)
