library(tidyverse)
library(haven)
library(ungroup)

options(scipen = 100000, digits = 4)
start <- proc.time()

raw <- read_csv("../out/data/who_mdb.csv")

# Input data
# x: Age group formats
x0 <- c(0:4, seq(5, 95, by = 5))
x1 <- c(0:4, seq(5, 85, by = 5))
x2 <- c(0:1, seq(5, 85, by = 5))
x3 <- NA
x4 <- c(0:1, seq(5, 75, by = 5))
x5 <- c(0:1, seq(5, 70, by = 5))

xlist <- list(x0, x1, x2, x3, x4, x5)

# y: Death counts in the age group
ph <-
  raw %>%
  # PH only, include causes with nonzero deaths, 2006+ data will be from CRVS
  # Move this to extract_who_mdb.R
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

# For testing subsets
# ph <-
#   ph %>%
#   filter(Year==1992)

write_csv(ph, "../out/data/who_mdb_ph.csv")

# Initialize an empty data frame
check_df <-
  data.frame(
    Year = integer(),
    Sex = character(),
    Cause = character(),
    List = character(),
    Total = numeric(),
    Fitted = numeric(),
    Diff = numeric(),
    Pct = numeric()
  )

# Define a function for "ungrouping"
fn1 <- function(df, list) {
  i <- unique(df$Frmat) + 1
  x <- list[[i]]
  y <- df$ndeaths[2:25]
  # Remove NA values
  # Replace zero values with small num to avoid errors in the model
  y <- y[!is.na(y)] + 1e-10
  # The pclm function performs the "ungrouping"
  M1 <- pclm(x, y[1:length(x)], 101 - max(x))
  z <- M1$fitted
  # Check
  a <- sum(df$ndeaths[2:25], na.rm = TRUE)
  b <- round(sum(z), 1)
  c <- round(b - a, 1)
  d <- round(100 * c / a, 1)
  # If c and d is large, redo pclm with adjusted y
  if (!is.nan(c) & (c > 10 & d > 5)) {
    # Transform y (multiply by 100) to deal with small counts
    y <- y * 100
    M1 <- pclm(x, y[1:length(x)], 101 - max(x))
    z <- M1$fitted / 100
    b <- round(sum(z), 1)
    c <- round(b - a, 1)
    d <- round(100 * c / a, 1)
    # If c and d is large, redo pclm without adjustments
    if (!is.nan(c) & (c > 10 & d > 5)) {
      y <- df$ndeaths[2:25]
      y <- y[!is.na(y)]
      M1 <- pclm(x, y[1:length(x)], 101 - max(x))
      z <- M1$fitted
      b <- round(sum(z), 1)
      c <- round(b - a, 1)
      d <- round(100 * c / a, 1)
    }
  }
  # Output
  group <- paste(cur_group(), collapse = " ")
  print(paste(group, unique(df$List), a, b, c, d, collapse = " "))
  # Create a data frame for the current iteration
  current_df <-
    data.frame(
      Year = cur_group()$Year,
      Sex = cur_group()$Sex,
      Cause = cur_group()$Cause,
      List = unique(df$List),
      Total = a,
      Fitted = b,
      Diff = c,
      Pct = d
    )
  # Append the current data frame to the global check_df
  check_df <<- rbind(check_df, current_df)
  # Return fitted deaths
  return(z)
}

# "Ungroup" death counts
ph1 <-
  ph %>%
  group_by(Year, Sex, Cause) %>%
  summarise(n = list(fn1(pick(ndeaths, List, Frmat), xlist)), .groups = "drop")

write_csv(check_df, "../out/data/check_df.csv")

ph2 <-
  ph1 %>%
  unnest(n) %>%
  mutate(age = rep(0:100, length.out = n())) %>%
  rename(year = Year, sex = Sex, cod = Cause) %>%
  select(year, sex, age, cod, n) %>%
  arrange(year, sex, age, cod) %>%
  group_by(year, sex, age) %>%
  mutate(total = sum(n, na.rm = TRUE), px = n / total)

write_csv(ph2, "../out/data/who_mdb_ph2.csv")

end <- proc.time()
elapsed <- end - start
print(elapsed)

###############################################################################

# For testing in a single group
ph_test <-
  raw %>%
  filter(Country == 3300 & Year == 1963 & Sex == 1 & Cause == "A128") %>%
  pivot_longer(
    cols = starts_with("Deaths"),
    names_to = c("agegrp"),
    names_prefix = "Deaths",
    values_to = "ndeaths"
  ) %>%
  ungroup() %>%
  select(-c(starts_with("IM_"), Country, Admin1, SubDiv)) %>%
  mutate(agegrp = as.numeric(agegrp), Frmat = as.numeric(Frmat))

i <- ph_test$Frmat[1] + 1
x <- xlist[[i]]
y <- ph_test$ndeaths[2:25]
y <- y[!is.na(y)]
# y[y == 0] <- 1e-6
# y <- y * 100
M1 <- pclm(x, y[1:length(x)], 101 - max(x))

summary(M1)
plot(M1)

z <- cbind(M1$fitted)
# z <- z/100
View(z)
print(y[1:length(x)])
sum(z)
plot(x, y[1:length(x)])
lines(z)
