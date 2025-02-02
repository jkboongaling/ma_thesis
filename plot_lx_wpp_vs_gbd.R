library(tidyverse)
library(gganimate)
library(gifski)

# Extract data
df1 <- read_csv("../out/data/wpp_lt_1x1.csv")
df1 <-
  df1 %>% 
  mutate(type = "UN") %>% 
  filter(loc == 608)

df2 <- read_csv("../out/data/wpp_lt_1x1_rfunc.csv")
df2 <-
  df2 %>% 
  mutate(type = "UN Computed") %>% 
  filter(loc == 608)

df3 <- read_csv("../out/data/gbd_lt_1x1_demotools.csv")
df3 <-
  df3 %>% 
  mutate(type = "IHME Computed") %>% 
  filter(loc == 16) # 9 = SEA; 16 = PH

df <- rbind(df2, df3) %>% 
  mutate(sex = as_factor(recode(sex, `1` = "Male", `2` = "Female")),
         type = as_factor(type))

# Static plot
plot1 <-
  df %>%
  ggplot(aes(x = age, y = lx, color = sex, linetype = type)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     n.breaks = 10) +
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
    y = "Survivors",
    title = "Life table survival function, Philippines",
    caption = "Data source: UN & IHME"
  )

# Animate
w <- 1080
h <- 600

plot1x <-
  plot1 +
  labs(title = "LT survival function by sex: Philippines, {closest_state}") +
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
  renderer = gifski_renderer("../out/fig/LT survival function by sex PH.gif")
)


# Static plot
plot2 <-
  df %>%
  ggplot(aes(x = age, y = mx, color = sex, linetype = type)) +
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
    caption = "Data source: UN"
  )

# Animate
w <- 1080
h <- 600

plot2x <-
  plot2 +
  labs(title = "ASDRs by sex: Philippines, {closest_state}") +
  transition_states(year, transition_length = 1, state_length = 1) +
  enter_fade() +
  exit_fade() +
  ease_aes("cubic-in-out")

animate(
  plot2x,
  fps = 36,
  duration = 10,
  width = w,
  height = h,
  res = 96,
  renderer = gifski_renderer("../out/fig/ASDRs by sex PH.gif")
)