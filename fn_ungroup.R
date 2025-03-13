library(tidyverse)
library(ungroup)

# Input data
# x: Age group formats
x0 <- c(0:4, seq(5, 95, by = 5))
x1 <- c(0:4, seq(5, 85, by = 5))
x2 <- c(0:1, seq(5, 85, by = 5))
x3 <- NA
x4 <- c(0:1, seq(5, 75, by = 5))
x5 <- c(0:1, seq(5, 70, by = 5))

xlist <- list(x0, x1, x2, x3, x4, x5)

# Initialize an empty data frame
check_df <-
  data.frame(
    Year = integer(),
    Sex = character(),
    Cause = character(),
    Total = numeric(),
    Fitted = numeric(),
    Diff = numeric(),
    Pct = numeric()
  )

# Define a function for "ungrouping"
fn1 <- function(df, fmt, list) {
  i <- fmt + 1
  x <- list[[i]]
  y <- df$ndeaths[2:25]
  # Remove NA values
  # Add 1e-10 to avoid errors due to zero values
  y <- y[!is.na(y)] + 1e-10
  # The pclm function performs the "ungrouping"
  M1 <- pclm(x, y[1:length(x)], 101 - max(x))
  z <- M1$fitted
  # Check
  a <- sum(df$ndeaths[2:25], na.rm = TRUE)
  b <- round(sum(z), 1)
  c <- round(b - a, 1)
  d <- round(100 * c / a, 1)
  # # If c and d is large, redo pclm with adjusted y
  # if (!is.nan(c) & (c > 10 & d > 5)) {
  #   # Transform y (multiply by 100) to deal with small counts
  #   y <- y * 100
  #   M1 <- pclm(x, y[1:length(x)], 101 - max(x))
  #   z <- M1$fitted / 100
  #   b <- round(sum(z), 1)
  #   c <- round(b - a, 1)
  #   d <- round(100 * c / a, 1)
  #   # If c and d is large, redo pclm without adjustments
  #   if (!is.nan(c) & (c > 10 & d > 5)) {
  #     y <- df$ndeaths[2:25]
  #     y <- y[!is.na(y)]
  #     M1 <- pclm(x, y[1:length(x)], 101 - max(x))
  #     z <- M1$fitted
  #     b <- round(sum(z), 1)
  #     c <- round(b - a, 1)
  #     d <- round(100 * c / a, 1)
  #   }
  # }
  # Output
  group <- paste(cur_group(), collapse = " ")
  print(paste(group, a, b, c, d, collapse = " "))
  # Create a data frame for the current iteration
  current_df <-
    data.frame(
      Year = cur_group()$Year,
      Sex = cur_group()$Sex,
      Cause = cur_group()$cod5,
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
