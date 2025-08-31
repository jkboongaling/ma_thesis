library(tidyverse)
library(gganimate)
library(gifski)

# Extract data
df1 <- read_csv("../out/data/wpp_lt_1x1.csv")
df1 <-
  df1 %>% 
  mutate(type = "UN")

df2 <- read_csv("../out/data/wpp_lt_1x1_rfunc.csv")
df2 <-
  df2 %>% 
  mutate(type = "Computed")

df <- rbind(df1, df2) %>% 
  filter(loc == 608) %>%
  mutate(sex = as_factor(recode(sex, `1` = "Male", `2` = "Female")),
         type = as_factor(type))

# Static plot
sex_colors <-
  c("Male" = "#08306B", "Female" = "#990000")

plot1 <-
  df %>%
  ggplot(aes(x = age, y = ex, color = sex, linetype = type)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = sex_colors) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 20)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 80, 10), limits = c(0, 80)) +
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
    y = "Life expectancy",
    title = "Life expectancy by sex, Philippines",
    caption = "Data source: UN World Population Prospects 2024"
  )

# Animate
w <- 1080
h <- 600

plot1x <-
  plot1 +
  labs(title = "Life expectancy by sex: Philippines, {closest_state}") +
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
  renderer = gifski_renderer("../out/fig/final/PH_WPP_ex.gif")
)

# For manuscript
# Static plot
plot2_2y <-
  df %>%
  filter(type == "Computed", year %in% c(1993, 2023)) %>%
  ggplot(aes(x = age, y = ex, color = sex, linetype = as.factor(year))) +
  geom_line(show.legend = c(color = TRUE), size = 0.8) +
  scale_color_manual(values = sex_colors) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 20)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 80, 10), limits = c(0, 80)) +
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
    y = "Life expectancy",
    title = "",
    caption = "",
    color = "Sex",
    linetype = "Year"
  )

plot2_2y

ggsave(
  paste0("../out/fig/final/", " PH_WPP_ex.png"),
  plot = plot2_2y, 
  width = 12, height = 6, dpi = 300, bg = "white"
)

