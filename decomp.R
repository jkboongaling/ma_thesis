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

# Inputs
yr1 <- 2021
yr2 <- 2023
s1  <- "Female"
s2  <- "f"

lt <- read_csv("../out/data/wpp_lt_1x1_rfunc.csv")

lt1 <-
  lt %>%
  filter(loc == 608, year == yr1, sex == s1) 

lt2 <-
  lt %>%
  filter(loc == 608, year == yr2, sex == s1) 

ex_diff <- lt2$ex[1] - lt1$ex[1]
ex_decomp1 <- arriaga(lt1$mx, lt2$mx, sex = s2, breakdown = F)

table <-
  matrix(round(c(lt2$ex[1], lt1$ex[1], ex_diff, sum(ex_decomp1)), 2), ncol = 1)

row.names(table) <-
  c(paste0("Life expectancy at birth for PH ", str_to_lower(s1), "s in ", yr2),
    paste0("Life expectancy at birth for PH ", str_to_lower(s1), "s in ", yr1),
    "Life expectancy difference",
    "Estimated difference from decomposition")
colnames(table) <- "Arriaga"
kable(table,
      caption = paste0(s1, " life expectancy gap, Philippines, ", yr1, "-", yr2))

# ggplot() +
#   geom_col(aes(x = 0:100, y = ex_decomp1), fill = "blue") +
#   scale_x_continuous(breaks = seq(0, 100, 10)) +
#   scale_y_continuous(breaks = seq(0, 1.5, 0.1)) +
#   theme_bw() +
#   labs(
#     title = paste0(
#       "Age-decomposition of the difference in PH ",
#       str_to_lower(s1),
#       " life expectancy, ",
#       yr1,
#       "-",
#       yr2
#     ),
#     x = "Age",
#     y = "Contributions"
#   )

ex_decomp2 <- arriaga(lt1$mx, lt2$mx, sex = s2, breakdown = T)

table <-
  matrix(round(c(
    lt2$ex[1],
    lt1$ex[1],
    ex_diff,
    sum(ex_decomp2$direct),
    sum(ex_decomp2$indirect),
    sum(ex_decomp2$direct) + sum(ex_decomp2$indirect)), 2), ncol = 1)

row.names(table) <-
  c(paste0("Life expectancy at birth for PH ", str_to_lower(s1), "s in ", yr2),
    paste0("Life expectancy at birth for PH ", str_to_lower(s1), "s in ", yr1),
    "Life expectancy difference",
    "Direct component",
    "Indirect and interaction component",
    "Estimated difference from decomposition")
colnames(table) <- "Arriaga"
kable(table,
      caption = paste0(s1, " life expectancy gap, Philippines, ", yr1, "-", yr2)) 

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

# ggplot(ex_decomp2, aes(x = age, y = value, fill = variable)) +
#   geom_col() +
#   scale_x_continuous(breaks = seq(0, 100, 10)) +
#   scale_y_continuous(breaks = seq(0, 1.5, 0.1)) +
#   theme_bw() +
#   labs(
#     title = paste0(
#       "Age-decomposition of the difference in PH ",
#       str_to_lower(s1),
#       " life expectancy, ",
#       yr1,
#       "-",
#       yr2
#     ),
#     x = "Age",
#     y = "Contributions"
#   )

# LE Decomposition (Cause)
cod5 <- read_csv("../out/data/cod5.csv")

cod <- 
  cod5 %>% 
  pivot_wider(names_from = cod5, values_from = px5)

cause_prop1 <-
  cod %>% 
  filter(year == yr1, sex == s1) %>% 
  select(c(4:8))

cause_prop2 <-
  cod %>% 
  filter(year == yr2, sex == s1) %>% 
  select(c(4:8))

# Fix this!
# Temporary only, this simply copies values at age 98 to 99 & 100
if (yr1 >= 2006) {
  cause_prop1 <- bind_rows(cause_prop1, cause_prop1[99, ], cause_prop1[99, ])
}
if (yr2 >= 2006) {
  cause_prop2 <- bind_rows(cause_prop2, cause_prop2[99, ], cause_prop2[99, ])
}

## Now we construct the cause-specific factor.
cause_fac1 <-
  (cause_prop1 * lt1$mx) /
  ifelse((lt2$mx - lt1$mx) == 0, 1, lt2$mx - lt1$mx) * ex_decomp1

cause_fac2 <-
  (cause_prop2 * lt2$mx) /
  ifelse((lt2$mx - lt1$mx) == 0, 1, lt2$mx - lt1$mx) * ex_decomp1

# Same results as the Arriaga method by age
sum(cause_fac2 - cause_fac1)

cause_mat <- cause_fac2 - cause_fac1

table <- matrix(round(
  c(lt2$ex[1],
    lt1$ex[1],
    sum(ex_decomp1),
    sum(cause_mat[,1]),
    sum(cause_mat[,2]),
    sum(cause_mat[,3]),
    sum(cause_mat[,4]),
    sum(cause_mat[,5]),
    sum(cause_mat)), 2))
  
row.names(table) <- c(
  paste0("Life expectancy at birth for PH ", str_to_lower(s1), "s in ", yr2),
  paste0("Life expectancy at birth for PH ", str_to_lower(s1), "s in ", yr1),
  "Life expectancy difference",
  "Circulatory diseases component",
  "Neoplasms component",
  "Infectious diseases component",
  "External causes component",
  "Other causes component",
  "Estimated total difference from decomposition")
colnames(table) <- paste0("PH ", s1, "s")
kable(table, caption = "Arriaga Decomposition by Cause")

total_dat <- data.frame(Age = 0:100, Value = ex_decomp1)

cause_dat <- data.frame(
  Age = rep(0:100, 5),
  Cause = rep(
    c("Circulatory", "Neoplasm", "Infection", "External", "Others"),
    each = 101
  ),
  Value = c(cause_mat[, 1], cause_mat[, 2], cause_mat[, 3], cause_mat[, 4], cause_mat[, 5])
)

ggplot() +
  geom_col(data = cause_dat,
           mapping = aes(x = Age, y = Value, fill = Cause)) +
  geom_line(data = total_dat, mapping = aes(x = 0:100, y = Value)) +
  # geom_hline(yintercept = 0, lty = "dotted", color = "red") +
  scale_fill_brewer(type = "qual", palette = 7) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(-0.1, 1.1, 0.1), limits = c(-0.1, 1.1)) +
  # scale_y_continuous(breaks = seq(-0.1, 0.7, 0.1), limits = c(-0.1, 0.7)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(5)
  ) +
  labs(
    title = paste0(
      "Age- and cause-decomposition of the difference in PH ",
      str_to_lower(s1),
      " life expectancy, ",
      yr1,
      "-",
      yr2
    ),
    x = "Age",
    y = "Contributions",
    fill = "Causes"
  )

ggsave(
  paste0("../out/fig/PH_", s1, "_", yr1, "-", yr2, "_Decomposition.png"),
  width = 1080,
  height = 600,
  units = "px",
  dpi = 96
)

################################################################################
# Continuous Approach

e0_diff1 <- (log(lt2$ex[1] / lt1$ex[1]) / 5) * ((lt2$ex[1] * lt1$ex[1])^0.5)
e0_diff2 <- (lt2$ex[1] - lt1$ex[1]) / 5

# Not much of a difference between the two
# e0_diff1 - e0_diff2

ex_diff <-
  (-log(lt2$mx / lt1$mx) / 5) *
  ((lt1$dx / 100000 * lt2$dx / 100000) ^ 0.5) *
  (lt1$ex + lt1$ex) / 2

# ggplot() +
#   geom_col(aes(x = 0:100, y = ex_diff), fill = "blue") +
#   scale_x_continuous(breaks = seq(0, 100, 10)) +
#   theme_bw() +
#   labs(
#     title = paste0(
#       "Age-decomposition of the difference in PH ",
#       str_to_lower(s1),
#       " life expectancy, ",
#       yr1,
#       "-",
#       yr2
#     ),
#     x = "Age",
#     y = "Contributions"
#   )

# e-dagger
tp  <- yr2 - yr1
gap <- lt2$ex[1] - lt1$ex[1]
gap <- gap / (tp)
ex <- (lt1$ex+lt2$ex)/2

# Function to calculate e-dagger
# From Alyson van Raalte 
ineq_edag <- function(age, dx, lx, ex, ax) {
  age_length_equal <- all.equal(length(age), length(dx), length(lx), length(ex), length(ax))
  stopifnot(age_length_equal)
  # length of the age interval
  n <- c(diff(age), 1)
  explusone <- c(ex[-1], ex[length(age)])
  # the average remaining life expectancy in each age interval
  # (as opposed to the beginning of the interval)
  # ends up being roughly half of the ex between ages
  ex_average <- ex + ax / n * (explusone - ex)
  rev(cumsum(rev(dx * ex_average))) / lx
}

edag <- (ineq_edag(0:100, lt1$dx, lt1$lx, lt1$ex, lt1$ax) *
         ineq_edag(0:100, lt2$dx, lt2$lx, lt2$ex, lt2$ax))^0.5
rho  <- -log(lt2$mx / lt1$mx) / tp
fx   <- (lt1$dx / 100000 * lt2$dx / 100000)^0.5
main <- sum(rho * fx) * edag[1]

# Direct component, as equation shows is the product
# between average improvements in mortality and
# life disparity or e-dagger at birth.
covar <- sum((rho - sum(rho * fx)) * (ex - edag[1]) * fx)
gap2  <- main + covar
table <-
  matrix(round(c(
    lt2$ex[1], lt1$ex[1], gap, sum(rho * fx), edag[1], main, covar, gap2), 4), ncol = 1)

row.names(table) <- c(paste0("Life expectancy at ", yr2),
                      paste0("Life expectancy at ", yr1),
                      paste0("Annualized change between ", yr1, "-", yr2),
                      "Average improvements in mortality",
                      "Life disparity at birth",
                      "Direct component",
                      "Covariance component",
                      "Estimated total difference from decomposition")
colnames(table) <- paste0("PH ", s1, "s")
kable(table, caption = "VCR Decomposition")

ggplot() +
  geom_line(mapping = aes(x = 0:100, y = rho, color = "Mortality Improvements")) +
  # geom_smooth(
  #   mapping = aes(x = 0:100, y = rho, color = "Mortality Improvements"),
  #   se = F, linetype = 2, show.legend = F) +
  geom_line(mapping = aes(x = 0:100, y = (ex - 50) / 125, color = "Life expectancy")) +
  scale_y_continuous(sec.axis = sec_axis(transform = ~ (125 * . + 50), name = "Life expectancy"),
                     breaks = seq(-0.4, 0.4, 0.1),
                     limits = c(-0.4, 0.4)) +
  scale_colour_manual(values = c("blue", "red")) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks = element_blank(),
    plot.margin = margin(5),
    legend.position = "bottom") +
  labs(
    title = paste0(
      "Life expectancy and mortality improvements by age, PH ",
      s1,
      "s, ",
      yr1,
      "-",
      yr2
    ),
    x = "Age",
    y = "Mortality Improvements",
    colour = ""
  )

ggsave(
  paste0("../out/fig/PH_", s1, "_", yr1, "-", yr2, "_Rho.png"),
  width = 1080,
  height = 600,
  units = "px",
  dpi = 96
)