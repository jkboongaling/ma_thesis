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

# Write output
write_csv(df, merged_csv)
write_dta(df, merged_dta)
