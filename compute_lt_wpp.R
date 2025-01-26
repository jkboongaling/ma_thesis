library(tidyverse)
library(purrr)
source("fn_life_table.R")

# Extract data
df0 <- read_csv("../out/data/wpp_lt_1x1.csv")

df1 <-
  df0 %>%
  mutate(sex = as_factor(recode(sex, `1` = "Male", `2` = "Female")))

# Prepare unique combinations of year, loc, and sex
combinations <- df1 %>%
  distinct(loc, year, sex)

# Apply LifeTableMx for each combination
life_table_results <- pmap(combinations, function(loc, year, sex) {
  print(paste("Computing LTs for:", loc, year, sex, "...", collapse = " "))
  mx_values <- df1 %>%
    filter(loc == !!loc, year == !!year, sex == !!sex) %>%
    pull(mx)
  # Check if there are valid mx values (non-empty)
  if (length(mx_values) > 0) {
    result <- LifeTableMx(mx_values, sex)
    result$loc <- loc
    result$year <- year
    result$sex <- sex
    return(result)
  } else {
    return(NULL)
  }
})

# Combine the results into a single dataframe, excluding NULL results
df <- bind_rows(life_table_results) %>% 
  select(loc, year, sex, everything())
