library(tidyverse)
library(haven)
library(colorspace)
library(patchwork)

# Qualitative Sequential Scheme Lexis Surface
# Based on R codes by Schöley and Willekens
# https://github.com/jschoeley/viscomplexis

################################################################################
# Helper functions

# Prep function
prep_df <- function(df, label_cod = FALSE) {
  df <- df %>%
    mutate(sex = as_factor(sex), cod5 = as_factor(cod5))
  levels(df$sex) <- c("Male", "Female")
  if (label_cod) {
    levels(df$cod5) <- c("Circulatory",
                         "Neoplasm",
                         "Infection",
                         "External",
                         "Others")
  }
  return(df)
}

# Color mixing function (LAB space alpha blending)
MixWithWhite <- function(.rgb, .alpha) {
  result <- mixcolor(.alpha, sRGB(1, 1, 1), hex2RGB(.rgb), where = "LAB")
  return(hex(result))
}

# Lexis surface plot for one sex
create_lexis_plot <- function(data, mf) {
  data %>%
    filter(sex == mf) %>%
    mutate(year = year + 0.5, age = age + 0.5) %>%
    ggplot() +
    geom_tile(aes(x = year, y = age, fill = mix_col, width = 1, height = 1)) +
    geom_hline(yintercept = seq(0, 100, 10), alpha = 0.5, lty = "dotted") +
    geom_vline(xintercept = seq(1960, 2020, 10), alpha = 0.5, lty = "dotted") +
    geom_abline(intercept = seq(-100, 100, 10) - 1960, alpha = 0.5, lty = "dotted") +
    geom_vline(xintercept = 2006, lty = "dashed", color = "black", size = 0.5) +
    annotate("text", x = (2006 + 2023) / 2, y = 50, label = "CRVS", color = "black", fontface = "bold", size = 4, vjust = 0) +
    annotate("text", x = (1963 + 2006) / 2, y = 50, label = "WHOMD", color = "black", fontface = "bold", size = 4, vjust = 0) +
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
df1 <- read_dta("../out/data/cod.dta") %>%
  select(year, sex, age, cod5, n, total, px) %>%
  prep_df()

df2 <- read_dta("../out/data/px_mdb_104_cod5.dta") %>% prep_df()
df3 <- read_dta("../out/data/px_mdb_09B_cod5.dta") %>% prep_df(label_cod = TRUE)
df4 <- read_dta("../out/data/px_mdb_08A_cod5.dta") %>% prep_df(label_cod = TRUE)
df5 <- read_dta("../out/data/px_mdb_07A_cod5.dta") %>% prep_df(label_cod = TRUE)

cod <- bind_rows(df1, df2, df3, df4, df5)

# Collapse to COD5 groups
cod5 <- cod %>%
  group_by(year, sex, age, cod5) %>%
  summarise(px5 = sum(px), .groups = "drop")

write_csv(cod5, "../out/data/cod5.csv")

# Identify modal CODs
cod5_mode <- cod5 %>%
  group_by(year, sex, age) %>%
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
plotm <- create_lexis_plot(cod5_mode_mix, "Male") +
  theme(
    axis.title.y = element_text(size = 12, angle = 90, vjust = 0.5, margin = margin(r = 10)),
  ) +
  labs(y = "Age")
plotf <- create_lexis_plot(cod5_mode_mix, "Female")
plotl <- create_legend_plot(cpal5, alphas, breaks)

# Combined layout
combined <- (plotm | plot_spacer() | plotf | plot_spacer()) +
  plot_layout(ncol = 4, widths = c(1, 0.01, 1, 0.7)) +
  inset_element(plotl, 0.1, 0.3, 0.8, 0.7)
combined

# Save output
ggsave(
  "../out/fig/final/PH_Lexis_COD_CRVS.png",
  plot = combined,
  width = 12, height = 6, dpi = 300, bg = "white"
)

