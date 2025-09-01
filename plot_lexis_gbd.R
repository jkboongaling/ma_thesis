library(tidyverse)
library(haven)
library(colorspace)
library(patchwork)

# Qualitative Sequential Scheme Lexis Surface
# Based on R codes by Schöley and Willekens
# https://github.com/jschoeley/viscomplexis

################################################################################
# Helper functions

# Color mixing function (LAB space alpha blending)
MixWithWhite <- function(.rgb, .alpha) {
  result <- mixcolor(.alpha, sRGB(1, 1, 1), hex2RGB(.rgb), where = "LAB")
  return(hex(result))
}

# Lexis surface plot for one sex
create_lexis_plot <- function(data, mf, code) {
  data %>%
    filter(sex == mf, loc == code) %>%
    mutate(year = year + 0.5, age = age + 0.5) %>%
    ggplot() +
    geom_tile(aes(x = year, y = age, fill = mix_col, width = 1, height = 1)) +
    geom_hline(yintercept = seq(0, 100, 10), alpha = 0.5, lty = "dotted") +
    geom_vline(xintercept = seq(1960, 2020, 10), alpha = 0.5, lty = "dotted") +
    geom_abline(intercept = seq(-100, 100, 10) - 1960, alpha = 0.5, lty = "dotted") +
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
    ) +
    labs(title = paste0(mf, "s", "\n"))
}

# Legend plot
create_legend_plot <- function(cpal5, alphas, breaks) {
  lgnd_mixed <- t(sapply(cpal5, function(x) MixWithWhite(x, alphas)))
  lgnd_data <- expand.grid(x = 1:nrow(lgnd_mixed), y = 1:ncol(lgnd_mixed))
  lgnd_data$col <- as.vector(lgnd_mixed)
  
  ggplot(lgnd_data) +
    geom_tile(aes(x = x, y = y, fill = col), colour = "white", lwd = 1) +
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
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text = element_text(colour = "black", size = 10),
      axis.text.y = element_text(hjust = 0, margin = margin(r = 10)),
      plot.margin = margin(10)
    ) +
    labs(title = "Proportion on all deaths\n")
}

################################################################################
# Extract and prepare data
df <- read_csv("../out/data/gbd_cod5.csv")

cod <- df %>% 
  select(loc, year, sex, age, cod5, n, total, px) %>%
  mutate(
    loc = factor(loc, labels = c("SEA", "PH")),
    sex = factor(sex, labels = c("Male", "Female")),
    cod5 = factor(
      cod5,
      labels = c("Circulatory", "Neoplasm", "Infection", "External", "Others")
    )
  )

# Collapse to COD5 groups
cod5 <- cod %>%
  group_by(loc, year, sex, age, cod5) %>%
  summarise(px5 = sum(px), .groups = "drop")

write_csv(cod5, "../out/data/cod5_gbd.csv")

# Identify modal CODs
cod5_mode <- cod5 %>%
  group_by(loc, year, sex, age) %>%
  filter(px5 == max(px5)) %>%
  ungroup()

# Color settings
cpal5 <- c(
  "Circulatory" = "#b2182b",
  "Neoplasm"    = "#6a3d9a",
  "Infection"   = "#33a02c",
  "External"    = "#7b3f00",
  "Others"      = "#008080"
)

breaks <- c(0.2, 0.4, 0.6, 0.8, 1)
alphas <- seq(0.2, 1, length.out = length(breaks) - 1)

# Apply color mixing
cod5_mode_mix <- cod5_mode %>%
  mutate(
    base_col = cpal5[cod5],
    px_disc = cut(px5, breaks, labels = FALSE, include.lowest = TRUE),
    mix_col = MixWithWhite(base_col, alphas[px_disc])
  )

# Plots
plotm <- create_lexis_plot(cod5_mode_mix, "Male", "PH") +
  theme(
    axis.title.y = element_text(size = 12, angle = 90, vjust = 0.5, margin = margin(r = 10)),
  ) +
  labs(y = "Age")
plotf <- create_lexis_plot(cod5_mode_mix, "Female", "PH")
plotl <- create_legend_plot(cpal5, alphas, breaks)

# Combined layout
combined <- (plotm | plot_spacer() | plotf | plot_spacer()) +
  plot_layout(ncol = 4, widths = c(1, 0.01, 1, 0.7)) +
  inset_element(plotl, 0.1, 0.3, 0.8, 0.7)
combined

# Save output
ggsave(
  "../out/fig/final/PH_Lexis_COD_GBD.png",
  plot = combined,
  width = 12, height = 6, dpi = 300, bg = "white"
)

################################################################################
# Annotated plots

poly_df <- 
  data.frame(
    year = c(1990, 2000, 2010, 2000, 
             2000, 2010, 2020, 2010,
             2010, 2020, 2021.8, 2021.8),
    age = c(20, 20, 30, 30,
            30, 30, 40, 40,
            40, 40, 41.8, 51.8),
    group = rep(1:3, each = 4)
  )

plotm1 <- plotm +
  geom_polygon(
    data = poly_df,
    aes(x = year, y = age, group = group),
    color = "red", fill = NA, linewidth = 0.5
  ) +
  annotate("text", x = c(1999, 2009, 2019), y = c(25, 35, 44), 
           label = c("a", "b", "c"), 
           color = "red", size = 4, fontface = "bold", vjust = 0.5)

plotf1 <- plotf +
  geom_polygon(
    data = poly_df,
    aes(x = year, y = age, group = group),
    color = "red", fill = NA, linewidth = 0.5
  ) +
  annotate("text", x = c(1999, 2009, 2019), y = c(25, 35, 44), 
           label = c("d", "e", "f"), 
           color = "red", size = 4, fontface = "bold", vjust = 0.5)

# Combined layout
combined <- (plotm1 | plot_spacer() | plotf1 | plot_spacer()) +
  plot_layout(ncol = 4, widths = c(1, 0.01, 1, 0.7)) +
  inset_element(plotl, 0.1, 0.3, 0.8, 0.7)
combined

# Save output
ggsave(
  "../out/fig/final/PH_Lexis_COD_GBD_x.png",
  plot = combined,
  width = 12, height = 6, dpi = 300, bg = "white"
)

