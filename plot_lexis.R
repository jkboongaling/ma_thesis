library(tidyverse)
library(haven)
library(colorspace)
library(patchwork)

options(scipen = 100000, digits = 4)

# Extract data
cod <- read_dta("../out/data/cod.dta")

# Retain labels as factors
cod <-
  cod %>%
  mutate(sex = as_factor(sex),
         cod5 = as_factor(cod5),
         cod10 = as_factor(cod10))

# Check
levels(cod$sex)
levels(cod$cod5)
levels(cod$cod10)

# Qualitative Sequential Scheme Lexis Surface
# Based on R codes by Schöley and Willekens
# https://github.com/jschoeley/viscomplexis

# COD groups
# The groups defined by cod5 and cod10 were coded in Stata
cod5 <-
  cod %>%
  group_by(year, sex, age, cod5) %>%
  summarise(px5 = sum(px))

cod10 <-
  cod %>%
  group_by(year, sex, age, cod10) %>%
  summarise(px10 = sum(px))

# Save COD groups to csv
write_csv(cod5, "../out/data/cod5.csv")
write_csv(cod10, "../out/data/cod10.csv")

# Modal CODs
cod5_mode <-
  cod5 %>%
  group_by(year, sex, age) %>%
  filter(px5 == max(px5)) %>%
  ungroup()

cod10_mode <-
  cod10 %>%
  group_by(year, sex, age) %>%
  filter(px10 == max(px10)) %>%
  ungroup()

summary(cod5_mode)
summary(cod10_mode)

# Define base colors for plot

# cpal5 <- qualitative_hcl(5, palette = "Dark 3")

cpal5 <- c(
  "Circulatory" = "#1B5E20",
  "Neoplasm"    = "#8E24AA",
  "Infection"   = "#8C564B",
  "External"    = "#B71C1C",
  "Others"      = "#0D47A1"
)

cpal10 <- c(
  "Infectious diseases"           = "#1B5E20",
  "Cancer"                        = "#0D47A1",
  "Diabetes"                      = "#B71C1C",
  "Ischemic heart disease"        = "#8E24AA",
  "Stroke"                        = "#4527A0",
  "Other cardiovascular diseases" = "#00796B",
  "Respiratory diseases"          = "#F57F17",
  "Suicide"                       = "#8C564B",
  "Injuries"                      = "#212121",
  "Others"                        = "#FBC02D"
)

breaks <- c(0.2, 0.4, 0.6, 0.8, 1)
alphas <- seq(0.2, 1, length.out = length(breaks) - 1)

# Function
#' Mix a Color With White
#'
#' @details This is alpha blending with a white background. The alpha blending
#'   takes place in the LAB color-space, ensuring perceptually balanced results.
#'
#' @param .rgb   vector of RGB hex values
#' @param .alpha alpha value within [0,1]
#'
#' @return A vector of alpha blended RGB hex values.
#'
MixWithWhite <- function(.rgb, .alpha) {
  # mix .rgb base colours with white according to .alpha
  result <- mixcolor(.alpha, sRGB(1, 1, 1), hex2RGB(.rgb), where = "LAB")
  return(hex(result)) # convert result to rgb hex string
}

# Get the mixed colors
cod5_mode_mix <-
  cod5_mode %>%
  mutate(
    base_col = cpal5[cod5],
    px_disc = cut(px5, breaks, labels = FALSE, include.lowest = TRUE),
    mix_col = MixWithWhite(.rgb = base_col, .alpha = alphas[px_disc])
  )

# Males
pm <-
  cod5_mode_mix %>%
  filter(sex == "Male") %>%
  # Align tiles with grid
  mutate(year = year + 0.5, age = age + 0.5) %>%
  ggplot() +
  # Colored Lexis surface
  geom_tile(aes(
    x = year,
    width = 1,
    y = age,
    height = 1,
    fill = mix_col
  )) +
  # Lexis grid and theme
  geom_hline(yintercept = seq(0, 100, 10),
             alpha = 0.2,
             lty = "dotted") +
  geom_vline(xintercept = seq(1960, 2020, 10),
             alpha = 0.2,
             lty = "dotted") +
  geom_abline(
    intercept = seq(-100, 100, 10) - 1960,
    alpha = 0.2,
    lty = "dotted"
  ) +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1960, 2020, 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
  coord_equal() +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(5)
  )

# Females
pf <-
  cod5_mode_mix %>%
  filter(sex == "Female") %>%
  # Align tiles with grid
  mutate(year = year + 0.5, age = age + 0.5) %>%
  ggplot() +
  # Colored Lexis surface
  geom_tile(aes(
    x = year,
    width = 1,
    y = age,
    height = 1,
    fill = mix_col
  )) +
  # Lexis grid and theme
  geom_hline(yintercept = seq(0, 100, 10),
             alpha = 0.2,
             lty = "dotted") +
  geom_vline(xintercept = seq(1960, 2020, 10),
             alpha = 0.2,
             lty = "dotted") +
  geom_abline(
    intercept = seq(-100, 100, 10) - 1960,
    alpha = 0.2,
    lty = "dotted"
  ) +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1960, 2020, 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
  coord_equal() +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(5)
  )

# Legends
lgnd_mixed <- t(sapply(cpal5, function(x)
  MixWithWhite(x, alphas)))
lgnd_data <- expand.grid(x = 1:nrow(lgnd_mixed),
                         y = 1:ncol(lgnd_mixed))
lgnd_data$col <- as.vector(lgnd_mixed)

pl <-
  ggplot(lgnd_data) +
  geom_tile(aes(x = x, y = y, fill = col),
            colour = "white",
            lwd = 1) +
  scale_fill_identity() +
  scale_x_continuous(
    breaks = 1:length(cpal5),
    labels = names(cpal5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = 1:(length(alphas) + 1) - 0.5,
    labels = breaks * 100,
    expand = c(0, 0)
  ) +
  coord_fixed(1.5) +
  theme_void() +
  theme(
    axis.title.y = element_text(size = 9, angle = 90, face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(
      size = 8,
      vjust = 0.5,
      hjust = 1,
      angle = 90
    ),
    plot.margin = margin(5)
  )

# Layout
p1 <- pm + labs(title = "Males\n")
p2 <- pf + labs(title = "Females\n")
p3 <- pl + ylab("Proportion on all deaths\n")

p1 + inset_element(p3, 0.8, 0.35, 1.8, 0.6)
p2 + inset_element(p3, 0.8, 0.35, 1.8, 0.6)
p4 <- p1 | (p2 + inset_element(p3, 0.8, 0.35, 1.8, 0.6))
p4
