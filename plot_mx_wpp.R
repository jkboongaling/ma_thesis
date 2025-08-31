library(tidyverse)
library(gganimate)
library(gifski)
library(scales)

# Extract data
df0 <- read_csv("../out/data/wpp_lt_1x1.csv")

df1 <-
  df0 %>%
  filter(loc == 608) %>%
  mutate(sex = as_factor(recode(sex, `1` = "Male", `2` = "Female")))

# Static plot
sex_colors <-
  c("Male" = "#08306B", "Female" = "#990000")
  
plot1 <-
  df1 %>%
  ggplot(aes(x = age, y = mx, color = sex)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = sex_colors) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 20)) +
  scale_y_continuous(expand = c(0, 0),
                     n.breaks = 10,
                     trans = "log10",
                     labels = label_number(accuracy = 0.001)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(5)
  ) +
  labs(
    x = "Age",
    y = "log(mx)",
    color = "Sex",
    title = "Age-specific death rates by sex, Philippines",
    caption = "Data source: UN World Population Prospects 2024"
  )

# Animate
w <- 1080
h <- 600

plot1x <-
  plot1 +
  labs(title = "ASDRs by sex: Philippines, {closest_state}") +
  transition_states(year, transition_length = 1, state_length = 1) +
  enter_fade() +
  exit_fade() +
  ease_aes("cubic-in-out")

animate(
  plot1x,
  fps = 36,
  duration = 10,
  width = w,
  height = h,
  res = 96,
  renderer = gifski_renderer("../out/fig/final/PH_WPP_mx.gif")
)

# For manuscript
# Static plot
plot2 <-
  df1 %>%
  filter(year %in% c(1993, 2003, 2013, 2019, 2021, 2023)) %>%
  ggplot(aes(x = age, y = mx, color = sex)) +
  geom_line(show.legend = c(color = TRUE), size = 0.8) +
  scale_color_manual(values = sex_colors) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 20)) +
  scale_y_continuous(expand = c(0, 0),
                     n.breaks = 5,
                     trans = "log10",
                     labels = label_number(accuracy = 0.001)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(5),
    panel.spacing.x = unit(1, "cm")
  ) +
  labs(
    x = "Age",
    y = "log(mx)",
    color = "",
    title = "",
    caption = ""
  ) +
  facet_wrap(~ year, nrow = 3, ncol = 2)

plot2

plot2_2y <-
  df1 %>%
  filter(year %in% c(1993, 2023)) %>%
  ggplot(aes(x = age, y = mx, color = sex, linetype = as.factor(year))) +
  geom_line(show.legend = c(color = TRUE), size = 0.8) +
  scale_color_manual(values = sex_colors) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 20)) +
  scale_y_continuous(expand = c(0, 0),
                     n.breaks = 10,
                     trans = "log10",
                     labels = label_number(accuracy = 0.001)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(size = 10, margin = margin(r = 10)),
    axis.text.x = element_text(size = 10, margin = margin(t = 10)),
    plot.margin = margin(5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(1.5, "cm"), 
    legend.key.size = unit(1.2, "lines"),
    legend.box.spacing = unit(1, "cm")
  ) +
  labs(
    x = "Age",
    y = "log(mx)",
    title = "",
    caption = "",
    color = "Sex",
    linetype = "Year"
  )

plot2_2y

ggsave(
  paste0("../out/fig/final/", " PH_WPP_mx.png"),
  plot = plot2_2y, 
  width = 12, height = 6, dpi = 300, bg = "white"
)

