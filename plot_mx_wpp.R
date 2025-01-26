library(tidyverse)
library(gganimate)
library(gifski)

# Extract data
df0 <- read_csv("../out/data/wpp_lt_1x1.csv")

df1 <-
  df0 %>%
  filter(loc == 608) %>%
  mutate(sex = as_factor(recode(sex, `1` = "Male", `2` = "Female")))

# Static plot
plot1 <-
  df1 %>%
  ggplot(aes(x = age, y = mx, color = sex)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
  scale_y_continuous(expand = c(0, 0),
                     n.breaks = 10,
                     trans = "log10") +
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
    subtitle = ""
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
  renderer = gifski_renderer("../out/fig/ASDRs by sex PH.gif")
)
