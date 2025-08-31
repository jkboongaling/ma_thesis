library(tidyverse)
library(haven)
library(colorspace)
library(patchwork)

# Extract data
df1 <- read_dta("../out/data/cod.dta")
df2 <- read_dta("../out/data/px_mdb_104_cod5.dta")
df3 <- read_dta("../out/data/px_mdb_09B_cod5.dta")
df4 <- read_dta("../out/data/px_mdb_08A_cod5.dta")
df5 <- read_dta("../out/data/px_mdb_07A_cod5.dta")

# Retain labels as factors
df1 <-
  df1 %>%
  select(year, sex, age, cod5, n, total, px) %>% 
  mutate(sex = as_factor(sex),
         cod5 = as_factor(cod5))

df2 <-
  df2 %>%
  mutate(sex = as_factor(sex),
         cod5 = as_factor(cod5))

levels(df2$sex) <- c("Male", "Female")

df3 <-
  df3 %>%
  mutate(sex = as_factor(sex),
         cod5 = as_factor(cod5))

levels(df3$sex) <- c("Male", "Female")
levels(df3$cod5) <- c("Circulatory", "Neoplasm", "Infection", "External", "Others")

df4 <-
  df4 %>%
  mutate(sex = as_factor(sex),
         cod5 = as_factor(cod5))

levels(df4$sex) <- c("Male", "Female")
levels(df4$cod5) <- c("Circulatory", "Neoplasm", "Infection", "External", "Others")

df5 <-
  df5 %>%
  mutate(sex = as_factor(sex),
         cod5 = as_factor(cod5))

levels(df5$sex) <- c("Male", "Female")
levels(df5$cod5) <- c("Circulatory", "Neoplasm", "Infection", "External", "Others")

# Check
levels(df1$sex)
levels(df1$cod5)
levels(df2$sex)
levels(df2$cod5)
levels(df3$sex)
levels(df3$cod5)
levels(df4$sex)
levels(df4$cod5)
levels(df5$sex)
levels(df5$cod5)

# Combine all data
cod <- rbind(df1, df2, df3, df4, df5)

# Qualitative Sequential Scheme Lexis Surface
# Based on R codes by Schöley and Willekens
# https://github.com/jschoeley/viscomplexis

# Just to ensure everything is summed up by COD analysis groups
cod5 <-
  cod %>%
  group_by(year, sex, age, cod5) %>%
  summarise(px5 = sum(px))

# Save COD groups to csv
write_csv(cod5, "../out/data/cod5.csv")

# Modal CODs
cod5_mode <-
  cod5 %>%
  group_by(year, sex, age) %>%
  filter(px5 == max(px5)) %>%
  ungroup()

summary(cod5_mode)

# Define base colors for plot

# cpal5 <- qualitative_hcl(5, palette = "Dark 3")

cpal5 <- c(
  "Circulatory" = "#b2182b",
  "Neoplasm"    = "#6a3d9a",
  "Infection"   = "#33a02c",
  "External"    = "#7b3f00",
  "Others"      = "#008080"
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
    # mix_col = MixWithWhite(.rgb = base_col, .alpha = 0.7)
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
             alpha = 0.8,
             lty = "dotted") +
  geom_vline(xintercept = seq(1960, 2020, 10),
             alpha = 0.8,
             lty = "dotted") +
  geom_abline(
    intercept = seq(-100, 100, 10) - 1960,
    alpha = 0.8,
    lty = "dotted") +
  geom_vline(xintercept = c(2006),
             lty = "dashed",
             color = "black",
             size = 0.8) +
  annotate("text", x = (2006+2023)/2, y = 50, label = "CRVS", color = "black", fontface = "bold", size = 4, vjust = 0) +
  annotate("text", x = (1963+2006)/2, y = 50, label = "WHOMD", color = "black", fontface = "bold", size = 4, vjust = 0) +
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
             alpha = 0.5,
             lty = "dotted") +
  geom_vline(xintercept = seq(1960, 2020, 10),
             alpha = 0.5,
             lty = "dotted") +
  geom_abline(
    intercept = seq(-100, 100, 10) - 1960,
    alpha = 0.5,
    lty = "dotted") +
  geom_vline(xintercept = c(2006),
             lty = "dashed",
             color = "black",
             size = 0.8) +
  annotate("text", x = (2006+2023)/2, y = 50, label = "CRVS", color = "black", fontface = "bold", size = 4, vjust = 0) +
  annotate("text", x = (1963+2006)/2, y = 50, label = "WHOMD", color = "black", fontface = "bold", size = 4, vjust = 0) +
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
    trans = "reverse",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = 1:(length(alphas) + 1) - 0.5,
    labels = breaks * 100,
    expand = c(0, 0)
  ) +
  coord_flip() +
  theme_void() +
  theme(
    # axis.title.x = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(size = 10, hjust = 0, margin = margin(r = 10)),
    axis.text.x = element_text(size = 10),
    plot.margin = margin(10)
  ) +
  labs(title = "Proportion on all deaths\n")

# Layout
p1 <- pm + labs(title = "Males\n")
p2 <- pf + labs(title = "Females\n")
p3 <- pl + ylab("Proportion on all deaths\n")
p3

p1 + inset_element(p3, 0.8, 0.35, 1.8, 0.6)
p2 + inset_element(p3, 0.8, 0.35, 1.8, 0.6)
p4 <- p1 | (p2 + inset_element(p3, 1.2, 0.35, 1.8, 0.65)) 
p4 

p5 <- p2 + inset_element(p3, 1.2, 0.35, 1.8, 0.65)
p5

combined <- p1 + plot_spacer() + p2 + plot_spacer() + 
  plot_layout(ncol = 4, widths = c(1, 0.01, 1, 0.7))
combined <- combined + inset_element(p3, 0.1, 0.3, 0.8, 0.7)
combined

ggsave(
  paste0("../out/fig/final/", "PH_Lexis_COD_CRVS.png"),
  plot = combined, 
  width = 12, height = 6, dpi = 300, bg = "white"
)

