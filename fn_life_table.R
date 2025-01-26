library(tidyverse)

options(scipen = 100000, digits = 4)

# Modified from KOSTAT Workshop 3
# Add Sx? Check
LifeTableMx <- function(mx, sex) {
  N <- length(mx)
  ax <- rep(0.5, N)
  if (sex == "Male") {
    ax[1] <- ifelse(mx[1] < 0.107, 0.045 + mx[1] * 2.684, 0.330)
  }
  if (sex == "Female") {
    ax[1] <- ifelse(mx[1] < 0.107, 0.053 + 2.800 * mx[1], 0.350)
  }
  qx <- mx / (1 + (1 - ax) * mx)
  qx[N] <- 1
  px <- 1 - qx
  lx <- 100000
  for (y in 1:(N - 1)) {
    lx[y + 1] <- lx[y] * px[y]
  }
  dx <- lx * qx
  Lx <- lx[-1] + ax[-N] * dx[-N]
  Lx[N] <- ifelse(mx[N] > 0, lx[N] / mx[N], 0)
  Tx <- c()
  for (y in 1:N) {
    Tx[y] <- sum(Lx[y:N])
  }
  Sx <- c()
  Sx[1] <- Lx[1] / lx[1]
  Sx[N] <- Tx[N] / Tx[N-1]
  for (y in 2:(N-1)) {
    Sx[y] <- Lx[y] / Lx[y - 1]
  }
  ex <- Tx / lx
  age <- 0:(N - 1)
  all <- data.frame(age, mx, qx, px, lx, dx, Lx, Sx, Tx, ex, ax)
  return(all)
}
