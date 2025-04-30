library(tidyverse)
library(haven)
library(kableExtra)
library(scales)

options(scipen = 100000, digits = 4)

################################################################################
# Data preparation

# Extract crvs data
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

# LE Decomposition (Cause)

source("fn_decomp.R")

lt    <- read_csv("../out/data/wpp_lt_1x1_rfunc.csv")
cod10 <- read_csv("../out/data/cod10.csv")

cod <- 
  cod10 %>% 
  pivot_wider(names_from = cod10, values_from = px10) %>% 
  select(-any_of(levels(cod$cod10)), all_of(levels(cod$cod10)))

# Years of interest
input_yrs <- c(1993, 2003, 2013, 2019, 2021, 2023)

# Tables and plots
plot <-
  map(input_yrs, function(yr) {
    message("Running for ", yr)
    s1 <- "Male"
    s2 <- "Female"
    s3 <- tolower(substr(s1, 1, 1))
    s4 <- tolower(substr(s2, 1, 1))
    
    lt1 <-
      lt %>%
      filter(loc == 608, year == yr, sex == s1) 
    
    lt2 <-
      lt %>%
      filter(loc == 608, year == yr, sex == s2) 
    
    ex_diff <- lt2$ex[1] - lt1$ex[1]
    ex_decomp1 <- arriaga2(lt1$mx, lt2$mx, s3, s4, breakdown = F)
    
    cause_prop1 <-
      cod %>% 
      filter(year == yr, sex == s1) %>% 
      select(c(4:15))
    
    cause_prop2 <-
      cod %>% 
      filter(year == yr, sex == s2) %>% 
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
    if (yr >= 2006) {
      cause_prop1 <- bind_rows(cause_prop1, cause_prop1[99, ], cause_prop1[99, ])
    }
    if (yr >= 2006) {
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
    sum(cause_fac2 - cause_fac1)
    
    cause_mat <- cause_fac2 - cause_fac1
    
    table <- matrix(round(
      c(lt2$ex[1],
        lt1$ex[1],
        sum(ex_decomp1),
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
      paste0("Life expectancy at birth for PH ", str_to_lower(s2), "s in ", yr),
      paste0("Life expectancy at birth for PH ", str_to_lower(s1), "s in ", yr),
      "Life expectancy difference",
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
    colnames(table) <- paste0("PH ", yr)
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
      s2, " e0 = ", format(round(lt2$ex[1], 1), nsmall = 1), "\n",
      s1, " e0 = ", format(round(lt1$ex[1], 1), nsmall = 1), "\n",
      "\u0394 = ", format(round(sum(ex_decomp1), 1), nsmall = 1)
    )
    
    # Breaks and limits
    y_breaks <- seq(-0.1, 0.8, 0.1)
    y_limits <- c(-0.1, 0.8)
    
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
          "Age- and cause-decomposition of the male-female life expectancy gap, Philippines ",
          yr),
        x = "Age Group",
        y = "Contribution (in years)",
        fill = "Cause"
      )
    
    ggsave(
      paste0("../out/fig/new/", yr, " PH_Male-Female_Decomposition.png"),
      plot = p,
      width = 12, height = 6, dpi = 300, bg = "white"
    )
    
    return(p)
  }
  )

################################################################################
# Plots for manuscript

# Age- and cause-decomposition of the change in life expectancy, Philippines, 1993–2019
p1 <- plot[[1]] +
  ggtitle("1993") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

p2 <- plot[[6]] +
  ggtitle("2023") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

combined <- p1 + plot_spacer() + p2 +
  plot_layout(ncol = 3, widths = c(1, 0.01, 1), guides = "collect") &
  theme(
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    legend.position = "right"
  )

combined

ggsave(
  paste0("../out/fig/new/combined/", " PH_MF_Decomposition_Combined.png"),
  plot = combined, 
  width = 12, height = 6, dpi = 300, bg = "white"
)

