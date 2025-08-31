library(tidyverse)
library(readxl)

# Extract data
df <- read_xlsx(
  "../docs/AnaCOD3 Results.xlsx",
  range = "A3:J20",
  col_names = c(
    "year",
    "com_Male",
    "com_Female",
    "com_Total",
    "idc_Male",
    "idc_Female",
    "idc_Total",
    "usb_Male",
    "usb_Female",
    "usb_Total"
  )
)

# Reshape
df <-
  df %>%
  pivot_longer(
    cols = c(
      starts_with("com_"),
      starts_with("idc_"),
      starts_with("usb_")
    ),
    names_to = c("var", "sex"),
    names_sep = "_",
    values_to = "pct"
  ) %>% 
  mutate(sex = as_factor(sex))

levels(df$sex)

# Custom facet labels
facet_labels <- c("com" = "Completeness",
                  "idc" = "Ill-defined Causes",
                  "usb" = "Usability Index")

# Static plot
plot1 <-
  df %>%
  ggplot(aes(x = year, y = pct, color = sex)) +
  geom_line(size = 0.8) +
  facet_wrap(~ var, labeller = labeller(var = facet_labels)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(2006, 2023, 4)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)),
                     breaks = seq(0, 100, 10), 
                     limits = c(0,100)) +
  scale_color_manual(values = c("#08306B", "#990000", "#00441B")) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(size = 10, margin = margin(r = 10)),
    axis.text.x = element_text(size = 10, margin = margin(t = 10)),
    panel.spacing = unit(1, "cm"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(1.5, "cm"), 
    legend.key.size = unit(1.2, "lines"),
    legend.box.spacing = unit(1, "cm")
  ) +
  labs(x = "Year", y = "Percent", color = "Sex",
       caption = "Note: Values were computed from CRVS data using ANACoD3 tool.")

plot1

ggsave(
  paste0("../out/fig/final/", "PH_ANACoD3_Results.png"),
  plot = plot1, 
  width = 12, height = 6, dpi = 300, bg = "white"
)