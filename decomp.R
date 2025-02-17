library(tidyverse)
library(kableExtra)
library(data.table)

# library(colorRamps)
# library(RColorBrewer)

# library(reshape2)

source("fn_life_table.R")
source("fn_decomp.R")

options(scipen = 100000, digits = 4)

################################################################################
# LE Decomposition (Arriaga)

lt <- read_csv("../out/data/wpp_lt_1x1_rfunc.csv")

m1 <-
  lt %>%
  filter(loc == 608, year == 1992, sex == "Male") %>%
  select(mx, ex)

m2 <-
  lt %>%
  filter(loc == 608, year == 2022, sex == "Male") %>%
  select(mx, ex)

ex_diff <- m2$ex[1] - m1$ex[1]
ex_decomp1 <- arriaga(m1$mx, m2$mx, sex = "m", breakdown = F)

table <-
  matrix(round(c(m2$ex[1], m1$ex[1], ex_diff, sum(ex_decomp)), 1), ncol = 1)

row.names(table) <-
  c("Life expectancy at birth for PH males in 2022",
    "Life expectancy at birth for PH males in 1992",
    "Life expectancy difference",
    "Estimated difference from decomposition")
colnames(table) <- "Arriaga"
kable(table,caption = "Male life expectancy gap, Philippines, 1992-2022")

ggplot() +
  geom_col(aes(x = 0:100, y = ex_decomp), fill = "blue") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  theme_minimal() +
  labs(title = "Age-decomposition of the difference in PH male life expectancy, 1992-2022", 
       x = "Age", y = "Contributions")

ex_decomp2 <- arriaga(m1$mx, m2$mx, sex = "m", breakdown = T)

table <-
  matrix(round(c(
    m2$ex[1],
    m1$ex[1],
    ex_diff,
    sum(ex_decomp2$direct),
    sum(ex_decomp2$indirect),
    sum(ex_decomp2$direct) + sum(ex_decomp2$indirect)), 1), ncol = 1)

row.names(table) <-
  c("Life expectancy at birth for PH males in 2022",
    "Life expectancy at birth for PH males in 1992",
    "Life expectancy difference",
    "Direct component",
    "Indirect and interaction component",
    "Estimated difference from decomposition")
colnames(table) <- "Arriaga"
kable(table,caption = "Male life expectancy gap, Philippines, 1992-2022")  

# in order to visualize the data with both direct and indirect
# components, we are going to turn this data.frame from wide to
# long format. We are using the functions from the package
# data.table.
ex_decomp2 <-
  melt.data.table(
    setDT(ex_decomp2),
    id.vars = "age",
    measure.vars = c("direct", "indirect")
  )

ggplot(ex_decomp2, aes(x = age, y = value, fill = variable)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  theme_minimal() +
  labs(title = "Age-decomposition of the difference in PH male life expectancy, 1992-2022", 
       x = "Age", y = "Contributions")