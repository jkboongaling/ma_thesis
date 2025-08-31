library(tidyverse)
library(haven)
library(kableExtra)
library(scales)
library(patchwork)

options(scipen = 100000, digits = 4)

################################################################################
# Prepare cause-specific mortality data

# Extract data
df1 <- read_dta("../out/data/cod.dta")
df2 <- read_dta("../out/data/px_mdb_104_cod10.dta")
df3 <- read_dta("../out/data/px_mdb_09B_cod10.dta")

df1 <-
  df1 %>%
  select(year, sex, age, cod10, n, total, px) 

# Check
table(df1$sex)
table(df1$cod10)
table(df2$sex)
table(df2$cod10)
table(df3$sex)
table(df3$cod10)

# Combine all data
cod <- rbind(df1, df2, df3)
cod$sex <- factor(cod$sex, levels = c(1, 2), labels = c("Male", "Female"))
cod$cod10 <- factor(cod$cod10,
                    levels = 1:12,
                    labels = c("Infectious diseases",
                               "Neoplasms",
                               "Diabetes",
                               "Ischemic heart disease",
                               "Stroke",
                               "Other cardiovascular diseases",
                               "Respiratory diseases",
                               "Suicide",
                               "Transport accidents",
                               "Other external causes",
                               "COVID-19",
                               "Others"))

levels(cod$sex)
levels(cod$cod10)

# Just to ensure everything is summed up by COD analysis groups
cod10 <-
  cod %>%
  group_by(year, sex, age, cod10) %>%
  summarise(px10 = sum(px))

levels(cod10$sex)
levels(cod10$cod10)

# Save COD groups to csv
write_csv(cod10, "../out/data/cod10.csv")

################################################################################

# LE Decomposition

source("fn_decomp.R")

lt    <- read_csv("../out/data/wpp_lt_1x1_rfunc.csv")
cod10 <- read_csv("../out/data/cod10.csv")

cod <- 
  cod10 %>% 
  pivot_wider(names_from = cod10, values_from = px10) %>% 
  select(-any_of(levels(cod$cod10)), all_of(levels(cod$cod10)))

# Inputs
# All decomposition configurations  are listed in input_df
input_df <- 
  tribble(~yr1, ~yr2, ~s1, ~brk,
          1993, 2023, "Male", 1,
          1993, 2003, "Male", 2,
          2003, 2013, "Male", 2,
          2013, 2023, "Male", 2,
          2013, 2019, "Male", 2,
          2019, 2021, "Male", 2,
          2021, 2023, "Male", 2,
          1993, 2023, "Female", 1,
          1993, 2003, "Female", 2,
          2003, 2013, "Female", 2,
          2013, 2023, "Female", 2,
          2013, 2019, "Female", 2,
          2019, 2021, "Female", 2,
          2021, 2023, "Female", 2
  )

input_df <-
  input_df %>%
  mutate(id = row_number())

# Decomposition including result tables and plots
plot <-
  pmap(input_df, function(yr1, yr2, s1, brk, id) {
    message("Running for ", s1, ": ", yr1, "-", yr2)
    s2 <- tolower(substr(s1, 1, 1))
    
    lt1 <-
      lt %>%
      filter(loc == 608, year == yr1, sex == s1) 
    
    lt2 <-
      lt %>%
      filter(loc == 608, year == yr2, sex == s1) 
    
    ex_diff <- lt2$ex[1] - lt1$ex[1]
    ex_decomp1 <- arriaga(lt1$mx, lt2$mx, sex = s2, breakdown = F)
    ex_decomp2 <- arriaga(lt1$mx, lt2$mx, sex = s2, breakdown = T)
    
    cause_prop1 <-
      cod %>% 
      filter(year == yr1, sex == s1) %>% 
      select(c(4:15))
    
    cause_prop2 <-
      cod %>% 
      filter(year == yr2, sex == s1) %>% 
      select(c(4:15))
    
    # Replace NAs (if any) with zero
    na_mask1 <- is.na(cause_prop1)
    na_mask2 <- is.na(cause_prop2)
    
    cause_prop1[na_mask1] <- 0
    cause_prop2[na_mask2] <- 0
    
    cat("Number of NAs replaced in cause_prop1:", sum(na_mask1), "\n")
    cat("Number of NAs replaced in cause_prop2:", sum(na_mask2), "\n")
    
    # This simply copies values at age 98 to 99 & 100 for years using CRVS
    # At very old ages, it is very likely that there are multiple causes of death
    # so this is not very useful to interpret anyway.
    if (yr1 >= 2006) {
      cause_prop1 <- bind_rows(cause_prop1, cause_prop1[99, ], cause_prop1[99, ])
    }
    if (yr2 >= 2006) {
      cause_prop2 <- bind_rows(cause_prop2, cause_prop2[99, ], cause_prop2[99, ])
    }
    
    # Now we construct the cause-specific factor.
    cause_fac1 <-
      (cause_prop1 * lt1$mx) /
      ifelse((lt2$mx - lt1$mx) == 0, 1, lt2$mx - lt1$mx) * ex_decomp1
    
    cause_fac2 <-
      (cause_prop2 * lt2$mx) /
      ifelse((lt2$mx - lt1$mx) == 0, 1, lt2$mx - lt1$mx) * ex_decomp1
    
    # Same results as the Arriaga method by age
    # sum(cause_fac2 - cause_fac1)
    
    cause_mat <- cause_fac2 - cause_fac1
    
    # Create results table and save in a csv
    # Convert data to data frames
    lt1_mx_df <- data.frame(lt1_mx = lt1$mx)
    lt2_mx_df <- data.frame(lt2_mx = lt2$mx)
    cause_prop1_df <- setNames(as.data.frame(cause_prop1), paste0("cause1_", 1:12))
    cause_prop2_df <- setNames(as.data.frame(cause_prop2), paste0("cause2_", 1:12))
    ex_decomp_df   <- data.frame(ex_decomp = ex_decomp1)
    cause_mat_df   <- setNames(as.data.frame(cause_mat), paste0("causemat_", 1:12))
    
    # Combine all into one data frame
    combined_df <- data.frame(age = 0:100) %>%
      bind_cols(lt1_mx_df, cause_prop1_df, lt2_mx_df, cause_prop2_df, ex_decomp_df, cause_mat_df) %>%
      mutate(across(-age, ~ format(round(as.numeric(.x), 5), scientific = FALSE, trim = TRUE)))
    
    # Save to CSV
    file_name <- paste0("../out/data/decomp/age/PH_", s1, "_", yr1, "_", yr2, "_decomp.csv")
    write_csv(combined_df, file_name)
    
    # Create a summary table in the console
    table <- matrix(round(
      c(lt2$ex[1],
        lt1$ex[1],
        sum(ex_decomp1),
        sum(ex_decomp2$direct),
        sum(ex_decomp2$indirect),
        sum(cause_mat[,1]),
        sum(cause_mat[,2]),
        sum(cause_mat[,3]),
        sum(cause_mat[,4]),
        sum(cause_mat[,5]),    
        sum(cause_mat[,6]),
        sum(cause_mat[,7]),
        sum(cause_mat[,8]),
        sum(cause_mat[,9]),
        sum(cause_mat[,10]),
        sum(cause_mat[,11]),
        sum(cause_mat[,12]),
        sum(cause_mat)), 2))
    
    row.names(table) <- c(
      paste0("Life expectancy at birth for PH ", str_to_lower(s1), "s in ", yr2),
      paste0("Life expectancy at birth for PH ", str_to_lower(s1), "s in ", yr1),
      "Life expectancy difference",
      "Direct component",
      "Indirect and interaction component",
      "Infectious diseases",
      "Neoplasms",
      "Diabetes",
      "Ischemic heart disease",
      "Stroke",
      "Other cardiovascular diseases",
      "Respiratory diseases",
      "Suicide",
      "Transport accidents",
      "Other external causes",
      "COVID-19",
      "Other causes",
      "Estimated total difference from decomposition")
    colnames(table) <- paste0("PH ", s1, "s")
    print(kable(table, caption = "Arriaga Decomposition by Cause"))
    
    # Define age groups
    age_breaks <- c(0, 1, seq(5, 95, by = 5), Inf)
    age_labels <- c("0", "1-4", paste(seq(5, 90, by = 5), seq(9, 94, by = 5), sep = "-"), "95+")
    
    # Assign age group to each age
    age_group <- cut(0:100, breaks = age_breaks, right = FALSE, labels = age_labels)
    
    total_dat <- data.frame(
      Age = 0:100,
      AgeGrp = age_group,
      Value = ex_decomp1)
    
    cause_dat <- data.frame(
      Age = rep(0:100, 12),
      AgeGrp = rep(age_group, 12),
      Cause = rep(
        c(  "Infectious diseases",
            "Neoplasms",
            "Diabetes",
            "Ischemic heart disease",
            "Stroke",
            "Other cardiovascular diseases",
            "Respiratory diseases",
            "Suicide",
            "Transport accidents",
            "Other external causes",
            "COVID-19",
            "Other causes"),
        each = 101
      ),
      Value = c(cause_mat[, 1], cause_mat[, 2], cause_mat[, 3], cause_mat[, 4], 
                cause_mat[, 5], cause_mat[, 6], cause_mat[, 7], cause_mat[, 8], 
                cause_mat[, 9], cause_mat[, 10], cause_mat[, 11], cause_mat[, 12])
    )
    
    total_dat <-
      total_dat %>% 
      group_by(AgeGrp) %>% 
      summarise(Value = sum(Value), .groups = "drop")
    
    cause_dat <-
      cause_dat %>% 
      mutate(Cause=fct_inorder(Cause)) %>% 
      group_by(Cause, AgeGrp) %>% 
      summarise(Value = sum(Value), .groups = "drop")
    
    # Pivot cause_dat to wide format
    cause_dat_wide <- cause_dat %>%
      pivot_wider(names_from = Cause, values_from = Value)
    
    # Combine with total_dat
    combined_contrib <- total_dat %>%
      rename(Total = Value) %>%
      left_join(cause_dat_wide, by = "AgeGrp")
    
    # Write to CSV
    write_csv(
      combined_contrib,
      paste0("../out/data/decomp/agegrp/PH_", s1, "_", yr1, "_", yr2, "_decomp_grp.csv")
    )
    
    
    
    cause_colors <- c(
      "Infectious diseases"              = "#33a02c", 
      "Neoplasms"                        = "#6a3d9a",  
      "Diabetes"                         = "#ffbf00",  
      "Ischemic heart disease"           = "#b2182b",  
      "Stroke"                           = "#ef3b2c",  
      "Other cardiovascular diseases"    = "#fb6a4a",  
      "Respiratory diseases"             = "#1f78b4", 
      "Suicide"                          = "#7b3f00",   
      "Transport accidents"              = "#b86b3e",   
      "Other external causes"            = "#d8b365",   
      "COVID-19"                         = "#008080",   
      "Other causes"                     = "#4d4d4d"
    )
    
    label_text <- paste0(
      yr2, " e0 = ", format(round(lt2$ex[1], 1), nsmall = 1), "\n",
      yr1, " e0 = ", format(round(lt1$ex[1], 1), nsmall = 1), "\n",
      "\u0394 = ", format(round(sum(ex_decomp1), 1), nsmall = 1)
    )
    
    # Breaks and limits
    if (brk == 1) {
      y_breaks <- seq(-0.3, 1.1, 0.1)
      y_limits <- c(-0.3, 1.1)
    } else {
      y_breaks <- seq(-0.6, 0.7, 0.1)
      y_limits <- c(-0.6, 0.7
      )
    }
    
    p <-
      ggplot() +
      geom_col(data = cause_dat,
               mapping = aes(x = AgeGrp, y = Value, fill = Cause)) +
      geom_line(data = total_dat, mapping = aes(x = AgeGrp, y = Value, group = 1)) +
      geom_hline(yintercept = 0, lty = "dashed", color = "black") +
      scale_fill_manual(values = cause_colors) +
      scale_y_continuous(breaks = y_breaks, 
                         limits = y_limits, 
                         labels = label_number(accuracy = 0.1)) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.text = element_text(colour = "black"),
        axis.text.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(t = 10), angle = 45, hjust = 1),
        plot.margin = margin(5)
      ) +
      annotate(
        "text",
        x = 17,
        y = Inf,
        label = label_text,
        hjust = 0,
        vjust = 1.5,
        size = 3,
        fontface = "italic"
      ) +
      labs(
        title = paste0(
          "Age- and cause-decomposition of the change in ",
          str_to_lower(s1),
          " life expectancy, Philippines ",
          yr1,
          "-",
          yr2
        ),
        x = "Age Group",
        y = "Contribution (in years)",
        fill = "Cause"
      )
    
    ggsave(
      paste0("../out/fig/new2/", str_pad(id, 2, pad = "0"), 
             " PH_", s1, "_", yr1, "-", yr2, "_Decomposition.png"),
      plot = p,
      width = 12, height = 6, dpi = 300, bg = "white"
    )
    
    return(p)
  }
  )

################################################################################
# Plots for manuscript

# Common themes
m_theme <-
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

f_theme <-
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

r_theme <-
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10)),
        legend.position = "right")

b_theme <-
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10)),
        legend.position = "bottom")

no_x_theme <-
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank())

# Age- and cause-decomposition of the change in life expectancy
# Philippines 1993–2019
pm1 <- plot[[1]] + ggtitle("Males") + m_theme
pf1 <- plot[[8]] + ggtitle("Females") + f_theme

combined1 <- pm1 + plot_spacer() + pf1 +
  plot_layout(ncol = 3, widths = c(1, 0.01, 1), guides = "collect") &
  r_theme

combined1

# Age- and cause-decomposition of the change in life expectancy
# Philippines 1993–2023
pm2 <- plot[[2]] + ggtitle("Males") + m_theme
pf2 <- plot[[9]] + ggtitle("Females") + f_theme

combined2 <- pm2 + plot_spacer() + pf2 +
  plot_layout(ncol = 3, widths = c(1, 0.01, 1), guides = "collect") &
  r_theme

combined2

# Loop shortcut
for (i in 1:7) {
  pm <- plot[[i]] + ggtitle("Males") + m_theme
  pf <- plot[[i + 7]] + ggtitle("Females") + f_theme
  
  combined <- pm + plot_spacer() + pf +
    plot_layout(ncol = 3, widths = c(1, 0.01, 1), guides = "collect") &
    r_theme
  
  ggsave(
    paste0(
      "../out/fig/new2/combined/",
      str_pad(i, 2, pad = "0"),
      " PH_Decomposition_Combined.png"
    ),
    plot = combined,
    width = 12,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  
  combined <- pm + plot_spacer() + pf +
    plot_layout(ncol = 3, widths = c(1, 0.01, 1), guides = "collect") &
    b_theme
  
  ggsave(
    paste0(
      "../out/fig/new2/combined/manu/",
      str_pad(i, 2, pad = "0"),
      " PH_Decomposition_Combined.png"
    ),
    plot = combined,
    width = 12,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}

################################################################################
# Can remove beyond this!
# Age- and cause-decomposition of the change in life expectancy
# Philippines 1993–2023
# Selected pre-pandemic intervals
pm3 <- plot[[3]] + ggtitle("Males") + m_theme + no_x_theme
pf3 <- plot[[3 + 7]] + ggtitle("Females") + f_theme + no_x_theme
pm4 <- plot[[4]] + ggtitle("") + m_theme + no_x_theme
pf4 <- plot[[4 + 7]] + ggtitle("") + f_theme + no_x_theme
pm5 <- plot[[5]] + ggtitle("") + m_theme
pf5 <- plot[[5 + 7]] + ggtitle("") + f_theme


combined3 <- 
  pm3 + plot_spacer() + pf3 +
  pm4 + plot_spacer() + pf4 +
  pm5 + plot_spacer() + pf5 +
  plot_layout(ncol = 3, widths = c(1, 0.01, 1), guides = "collect") &
  b_theme

combined3

ggsave(
  paste0(
    "../out/fig/new2/combined/manu/",
    " PH_Decomposition_Combined_Prepandemic.png"
  ),
  plot = combined3,
  width = 12,
  height = 18,
  dpi = 300,
  bg = "white"
)

# Selected pandemic intervals
library(grid)
# Define row period labels as grobs
label_2019_2021 <- wrap_elements(full = textGrob("2019–2021", rot = 90, gp = gpar(fontsize = 14)))
label_2021_2023 <- wrap_elements(full = textGrob("2021–2023", rot = 90, gp = gpar(fontsize = 14)))

pm6 <- plot[[6]] + ggtitle("Males") + m_theme + no_x_theme
pf6 <- plot[[6 + 7]] + ggtitle("Females") + f_theme + no_x_theme
pm7 <- plot[[7]] + ggtitle("") + m_theme
pf7 <- plot[[7 + 7]] + ggtitle("") + f_theme

combined4 <- 
  label_2019_2021 + pm6 + plot_spacer() + pf6 +
  label_2021_2023 + pm7 + plot_spacer() + pf7 +
  plot_layout(ncol = 4, widths = c(0.01, 1, 0.01, 1), guides = "collect") &
  b_theme

combined4

# Row 1: 2019–2021
row1 <- label_2019_2021 + pm6 + plot_spacer() + pf6 +
  plot_layout(ncol = 4, widths = c(0.1, 1, 0.01, 1))

# Row 2: 2021–2023
row2 <- label_2021_2023 + pm7 + plot_spacer() + pf7 +
  plot_layout(ncol = 4, widths = c(0.1, 1, 0.01, 1))

# Combine both rows into final layout
combined4 <- row1 / row2 + 
  plot_layout(heights = c(1, 1), guides = "collect") & 
  b_theme

ggsave(
  paste0(
    "../out/fig/new2/combined/manu/",
    " PH_Decomposition_Combined_Postpandemic.png"
  ),
  plot = combined4,
  width = 12,
  height = 30,
  dpi = 300,
  bg = "white"
)

