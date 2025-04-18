library(tidyverse)
library(haven)
library(scales)

# Extract data
lt <- read_csv("../out/data/wpp_lt_1x1_rfunc.csv")

lt1 <-
  lt %>%
  filter(loc == 608) %>% 
  mutate(sex = as_factor(sex))

# Mortality rates

brks <- seq(from = 0, to = 1, by = 0.1)

plot1 <-
  lt1 %>%
  ggplot() +
  # Heatmap
  geom_raster(aes(
    x = year + 0.5,
    y = age + 0.5,
    fill = mx
  )) +
  # Lexis grid
  geom_hline(yintercept = seq(0, 100, 10),
             alpha = 0.2,
             lty = "dotted") +
  geom_vline(xintercept = seq(1950, 2020, 10),
             alpha = 0.2,
             lty = "dotted") +
  geom_abline(
    intercept = seq(-100, 100, 10) - 1950,
    alpha = 0.2,
    lty = "dotted"
  ) +
  scale_fill_steps(breaks = brks, low = "white", high = "#1f5778",
                    guide = "colourbar", aesthetics = "fill", 
                    limits = c(0, 1), oob = squish) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1950, 2020, 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
  guides(fill = guide_legend(title = "mx", title.position = "left",
                             nrow = 1, label.position = "bottom", keywidth = unit(0.5, "cm"))) +
  coord_equal() +
  facet_grid(col = vars(sex)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(5),
    legend.margin = margin(t = 10),
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines")
  )+
  labs(
    x = "Year",
    y = "Age",
    title = "Death rates by sex, Philippines 1950-2023",
    caption = "Data source: UN"
  )

plot1

ggsave(
  paste0("../out/fig/PH_Lexis_mx_by_sex.png"),
  width = 1080,
  height = 600,
  units = "px",
  dpi = 96
)

# Mortality rate sex ratio


# Breaks for discrete scale
breaks <- c(0, 1/2, 100/175, 100/150, 100/125, 100/101,
            101/100, 125/100, 150/100, 175/100, 2/1, Inf)

labels <- c(">100% excess female mortality",
            "75-100%",
            "50-75%",
            "25-50%",
            "1-25%",
            "Equal mortality",
            "1-25% ",
            "25-50% ",
            "50-75% " ,
            "75-100% ",
            ">100% excess male mortality")

# Compute mx sex ratio
lt2 <-
  lt1 %>%
  select(year, sex, age, mx) %>% 
  pivot_wider(names_from = sex, values_from = mx) %>% 
  mutate(mx_ratio = na_if(Male/Female, Inf)) %>% 
  mutate(mx_ratio_disc = cut(mx_ratio, breaks, labels, include.lowest = TRUE))

plot2 <-
  lt2 %>%
  ggplot() +
  # Heatmap
  geom_raster(aes(x = year + 0.5, y = age + 0.5, fill = mx_ratio_disc)) +
  # Lexis grid
  geom_hline(yintercept = seq(0, 100, 10),
             alpha = 0.2,
             lty = "dotted") +
  geom_vline(xintercept = seq(1950, 2020, 10),
             alpha = 0.2,
             lty = "dotted") +
  geom_abline(
    intercept = seq(-100, 100, 10) - 1950,
    alpha = 0.2,
    lty = "dotted"
  ) +
  scale_fill_brewer(name = NULL, type = "div", palette = 5, drop = FALSE) +
  scale_x_continuous("Year", expand = c(0, 0),
                     breaks = seq(1950, 2020, 10)) +
  scale_y_continuous("Age", expand = c(0, 0),
                     breaks = seq(0, 100, 10)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_equal() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(5),
    legend.margin = margin(l = 10)
  )+
  labs(
    title = "Death rates sex ratio, Philippines 1950-2023",
    caption = "Data source: UN"
  )

plot2

ggsave(
  paste0("../out/fig/PH_Lexis_mx_sex_ratio.png"),
  width = 1080,
  height = 600,
  units = "px",
  dpi = 96
)