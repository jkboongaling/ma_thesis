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
  filter(loc == 608, year == 1992, sex == "Female") %>%
  select(mx, ex)

m2 <-
  lt %>%
  filter(loc == 608, year == 2022, sex == "Female") %>%
  select(mx, ex)

ex_diff <- m2$ex[1] - m1$ex[1]
ex_decomp1 <- arriaga(m1$mx, m2$mx, sex = "f", breakdown = F)

table <-
  matrix(round(c(m2$ex[1], m1$ex[1], ex_diff, sum(ex_decomp1)), 2), ncol = 1)

row.names(table) <-
  c("Life expectancy at birth for PH females in 2022",
    "Life expectancy at birth for PH females in 1992",
    "Life expectancy difference",
    "Estimated difference from decomposition")
colnames(table) <- "Arriaga"
kable(table,caption = "Female life expectancy gap, Philippines, 1992-2022")

ggplot() +
  geom_col(aes(x = 0:100, y = ex_decomp1), fill = "blue") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  theme_minimal() +
  labs(title = "Age-decomposition of the difference in PH male life expectancy, 1992-2022", 
       x = "Age", y = "Contributions")

ex_decomp2 <- arriaga(m1$mx, m2$mx, sex = "f", breakdown = T)

table <-
  matrix(round(c(
    m2$ex[1],
    m1$ex[1],
    ex_diff,
    sum(ex_decomp2$direct),
    sum(ex_decomp2$indirect),
    sum(ex_decomp2$direct) + sum(ex_decomp2$indirect)), 2), ncol = 1)

row.names(table) <-
  c("Life expectancy at birth for PH females in 2022",
    "Life expectancy at birth for PH females in 1992",
    "Life expectancy difference",
    "Direct component",
    "Indirect and interaction component",
    "Estimated difference from decomposition")
colnames(table) <- "Arriaga"
kable(table,caption = "Female life expectancy gap, Philippines, 1992-2022")  

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
  theme_bw() +
  labs(title = "Age-decomposition of the difference in PH female life expectancy, 1992-2022", 
       x = "Age", y = "Contributions")

ggsave("../out/fig/ph_decomp_female_1992_2022.png", width = 1080, height = 600, 
       units = "px", dpi = 96)

# LE Decomposition (Cause)
cod5 <- read_csv("../out/data/cod5.csv")

cod <- 
  cod5 %>% 
  pivot_wider(names_from = cod5, values_from = px5)

cause_prop1 <-
  cod %>% 
  filter(year == 1992, sex == "Female") %>% 
  select(c(4:8))

cause_prop2 <-
  cod %>% 
  filter(year == 2022, sex == "Female") %>% 
  select(c(4:8))

# Fix this! 
# Temporary only, this simply copies values at age 98 to 99 & 100
# cause_prop1 <- bind_rows(cause_prop1, cause_prop1[99,], cause_prop1[99,])
# cause_prop2 <- bind_rows(cause_prop2, cause_prop2[99,], cause_prop2[99,])


## Now we construct the cause-specific factor.
cause_fac1 <-
  (cause_prop1 * m1$mx) /
  ifelse((m2$mx - m1$mx) == 0, 1, m2$mx - m1$mx) * ex_decomp1

cause_fac2 <-
  (cause_prop2 * m2$mx) /
  ifelse((m2$mx - m1$mx) == 0, 1, m2$mx - m1$mx) * ex_decomp1

sum(cause_fac2-cause_fac1)
# This get you the same results as the Arriaga method by age
cause_mat <- cause_fac2 - cause_fac1

table <- matrix(round(
  c(m2$ex[1],
    m1$ex[1],
    sum(ex_decomp1),
    sum(cause_mat[,1]),
    sum(cause_mat[,2]),
    sum(cause_mat[,3]),
    sum(cause_mat[,4]),
    sum(cause_mat[,5]),
    sum(cause_mat)), 2))
  
# The column number is coded by the number of causes it
# represents in the short list provided in the link above.
# You can crate your own list by changing the column number
# included and create your own list of cause contributions.
row.names(table) <- c(
  "Life expectancy at birth for PH females in 2022",
  "Life expectancy at birth for PH females in 1992",
  "Life expectancy difference",
  "Circulatory diseases component",
  "Neoplasms component",
  "Infectious diseases component",
  "External causes component",
  "Other causes component",
  "Estimated total difference from decomposition")
colnames(table) <- "PH females"
kable(table, caption = "Arriaga Decomposition by Age and Cause")
