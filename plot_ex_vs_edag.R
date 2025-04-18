library(tidyverse)
library(haven)
library(gganimate)
library(gifski)

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

# Extract data
lt <- read_csv("../out/data/wpp_lt_1x1_rfunc.csv")

lt1 <-
  lt %>%
  filter(loc == 608) %>% 
  group_by(sex, year) %>% 
  mutate(edag = ineq_edag(age, dx, lx, ex, ax)) %>% 
  ungroup() %>% 
  mutate(sex = as_factor(sex))

# Static plot
plot1 <-
  lt1 %>%
  filter(age == 0) %>%
  ggplot(aes(x = ex, y = edag, color = sex)) +
  geom_point() +
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
    x = "Life expectancy at birth",
    y = "Life disparity at birth",
    title = "Life expectancy and life disparity at birth by sex, Philippines 1950-2023",
    caption = "Data source: UN"
  )

plot1

ggsave("../out/fig/PH_e0_vs_edag0.png", width = 1080, height = 600, 
       units = "px", dpi = 96)

# # Animate
# w <- 1080
# h <- 600
# 
# plot1x <-
#   plot1 +
#   labs(title = "Life expectancy and life disparity by sex, {closest_state}") +
#   transition_states(year, transition_length = 1, state_length = 1) +
#   enter_fade() +
#   exit_fade() +
#   ease_aes("cubic-in-out")
# 
# animate(
#   plot1x,
#   fps = 36,
#   duration = 10,
#   width = w,
#   height = h,
#   res = 96,
#   renderer = gifski_renderer("../out/fig/PH_ex_vs_edag.gif")
# )
