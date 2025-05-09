#
# Kaya dust samples
# 05/2025
#

library(readxl);library(dplyr);library(purrr)
library(tidyr);library(dplyr)
library(stringr)

# notes
# Each excel file contains samples for each of our field days. 
# At the beginning of each ID it will say either SS, SN, MM, or LP which signifies 
# what plot the sample was taken from. 

# C01, C02, etc - clean samples
# M01, etc. - March 4th dust samples
# LM01, etc. - March 28th dust samples
# F01, etc. - Feb 4th dust samples
# L1 or L01 - Liter samples

# If the ID has a V at the end, that means it was taken vertically.
# 3D01, 3D02, etc. - samples taken with the dust cutter

# file paths
fls = list.files("data/dust/uvu-sundance-2025/", full.names = T)

# collection dates
dates <- as.Date(c("2025-04-04", "2025-03-21", "2025-03-28"))

# Read, add date column, and bind rows
# all_data <- map2_dfr(fls, dates, ~ read_excel(.x) %>% mutate(sample_collect = .y))
all_data <- map2_dfr(fls, dates, ~ read_excel(.x) %>%
                       rename_with(tolower) %>%
                       mutate(sample_collect = .y))
# head(all_data)
# dim(all_data)

# tidy up data based on notes
all_data <- all_data %>%
  mutate(
    # site from start of ID
    site = str_extract(id, "^(SS|SN|MM|LP)"),
    
    # layer based on text patterns
    layer = case_when(
      str_detect(id, "LM\\d{2}") ~ "March 27",
      str_detect(id, "M\\d{2}") ~ "March 4",
      str_detect(id, "F\\d{2}") ~ "Feb 4",
      str_detect(id, "C\\d{2}") ~ "Clean",
      TRUE ~ NA_character_
    ),
    
    # sample number (digits at the end of the match for each layer type)
    sample_number = str_extract(id, "\\d{2,}$"),
    
    # size
    size = case_when(
      str_detect(id, "L1|L01") ~ "Liter",
      str_detect(id, "3D\\d{2}") ~ "cutter",
      TRUE ~ NA_character_
    ),
    
    # V if ID ends with V
    orientation = if_else(str_detect(id, "V$"), "V", NA_character_)
  )

# # add depth
# all_data <- all_data %>%
#   # depth into lower and upper
#   separate(depth, into = c("lower_depth", "upper_depth"), sep = "-", convert = TRUE) %>%
#   mutate(
#     # thickness
#     thickness = as.numeric(upper_depth) - as.numeric(lower_depth)
#   )
all_data <- all_data %>%
  # Make a temp column to split without overwriting depth
  separate(depth, into = c("lower_depth", "upper_depth_temp"), sep = "-", remove = FALSE) %>%
  mutate(
    # Convert to numeric safely
    lower_depth = as.numeric(lower_depth),
    upper_depth = as.numeric(upper_depth_temp),
    # Calculate thickness, only if both depths are numeric
    thickness = ifelse(is.na(lower_depth) | is.na(upper_depth), NA, upper_depth - lower_depth),
    # Update NA layers based on depth column patterns
    layer = case_when(
      !is.na(layer) ~ layer,  # keep existing values
      str_detect(depth, "LM") ~ "March 27",
      str_detect(depth, "M")  ~ "March 4",
      str_detect(depth, "F")  ~ "Feb 4",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-upper_depth_temp)  # clean up temp column


# add elevation and plot names
all_data <- all_data %>%
  mutate(
    # elevation
    elevation = case_when(
      site == "SN" ~ 7852,
      site == "LP" ~ 6333,
      site == "MM" ~ 7225,
      site == "SS" ~ 8184,
      TRUE ~ NA_real_
    ),
    
    # layer names to dates
    layer_date = case_when(
      layer == "Feb 4"   ~ as.Date("2025-02-04"),
      layer == "March 4" ~ as.Date("2025-03-04"),
      layer == "March 27"~ as.Date("2025-03-27"),
      TRUE ~ as.Date(NA)
    )
  )

head(all_data)
glimpse(all_data)

# filter cct values below 0
all_data <- all_data %>% filter(dust_conc > 0 & dust_conc < 0.2)

# # # # # # # # # 
## PLOTS

library(ggplot2);library(scales) 

ggplot(all_data, aes(x = site, y = dust_conc, fill=as.factor(elevation))) +
  geom_boxplot() +
  labs(
    title = "Dust Concentration (ppm) by Site", x = "",
    y = "Dust Concentration (ppm)"
  ) +
  theme_minimal()


# Plot 1 all samples
ggplot(all_data, aes(x = site, y = dust_conc, fill = elevation)) +
  geom_boxplot(color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 3, size = 3, color = "firebrick") +
  scale_fill_gradient(
    low = "#cce5ff", high = "#003366",
    name = "Elevation (ft)",
    labels = comma  # adds comma formatting to legend
  ) +
  labs(
    # title = "Dust Concentration (ppm) by Site",
    x = "",
    y = "Dust concentration (ppm)"
  ) +
  theme_minimal(base_size = 14) +  # sets a general base font size
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# Add labels
# Summarize counts per site
count_data <- all_data %>%
  group_by(site) %>%
  summarise(n = n(), .groups = "drop")
# Add counts with geom_text()
f1 = ggplot(all_data, aes(x = site, y = dust_conc, fill = elevation)) +
  geom_boxplot(color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 3, size = 3, color = "firebrick") +
  geom_text(
    data = count_data,
    aes(x = site, y = max(all_data$dust_conc, na.rm = TRUE) + 0.01, label = paste0("n=", n)),
    inherit.aes = FALSE,
    size = 4,
    vjust = 0
  ) +
  scale_fill_gradient(
    # low = "#cce5ff", high = "#003366",
    high = "#cce5ff", low = "#003366",
    name = "Elevation (ft)",
    labels = comma
  ) +
  labs(
    x = "",
    y = "Dust concentration (ppm)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

svpth = "figures/dust-samples-25"
ggsave(file.path(svpth, "f1-dust-site-all.png"), plot = f1,
       width = 8, height = 5, units = "in", dpi = 300)

  # by date
ggplot(all_data, aes(x = site, y = dust_conc, fill = layer)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.6, color = "black") +
  stat_summary(
    fun = mean, geom = "point", shape = 21, size = 3,
    position = position_dodge(0.8), color = "black"
  ) +
  labs(
    title = "Dust Concentration (ppm) by Site and Layer",
    x = "",
    y = "Dust Concentration (ppm)",
    fill = "Layer"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# or
ggplot(all_data, aes(x = site, y = dust_conc, fill = site)) +
  geom_boxplot(color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  facet_wrap(~layer) +
  labs(
    x = "",
    y = "Dust Concentration (ppm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  )


# add text
count_data <- all_data %>%
  group_by(site, layer) %>%
  summarise(n = n(), .groups = "drop")

ggplot(all_data, aes(x = site, y = dust_conc, fill = layer)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.6, color = "black") +
  stat_summary(
    fun = mean, geom = "point", shape = 21, size = 3,
    position = position_dodge(0.8), color = "black"
  ) +
  geom_text(
    data = count_data,
    aes(x = site, y = 0.18, label = n, group = layer),
    position = position_dodge(0.8),
    size = 4,
    vjust = 1
  ) +
  labs(
    title = "Dust Concentration (ppm) by Site and Layer",
    x = "",
    y = "Dust Concentration (ppm)",
    fill = "Layer"
  ) +
  theme_minimal(base_size = 14)


View(all_data[which(is.na(all_data$layer)),1:9])






# # Make a copy to update
# updated_data <- all_data
# 
# # Get rows with missing layer
# missing_layer <- updated_data %>%
#   filter(is.na(layer))
# 
# # Function to find matching layer based on depth proximity
# find_matching_layer <- function(lower, upper, collect_date, all_data) {
#   matches <- all_data %>%
#     filter(!is.na(layer), sample_collect == collect_date) %>%
#     filter(
#       abs(lower_depth - lower) <= 6,
#       abs(upper_depth - upper) <= 6
#     )
#   
#   # Return the first matching layer, or NA if none found
#   if (nrow(matches) > 0) {
#     return(matches$layer[1])
#   } else {
#     return(NA_character_)
#   }
# }
# 
# # Apply function row-wise
# updated_data <- updated_data %>%
#   mutate(
#     layer = if_else(
#       is.na(layer),
#       pmap_chr(list(lower_depth, upper_depth, sample_collect), 
#                ~ find_matching_layer(..1, ..2, ..3, all_data)),
#       layer
#     )
#   )


updated_data <- all_data
updated_data$layer[is.na(updated_data$layer)] = "March 27"

# add text
count_data <- updated_data %>%
  group_by(site, layer) %>%
  summarise(n = n(), .groups = "drop")

f2 = ggplot(updated_data, aes(x = site, y = dust_conc, fill = layer)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.6, color = "black") +
  stat_summary(
    fun = mean, geom = "point", shape = 21, size = 3,
    position = position_dodge(0.8), color = "black"
  ) +
  scale_fill_manual(
    # values = c("white", "lightblue", "skyblue", "skyblue4"), 
    values = c("white", "#f4cccc", "#ea9999", "#cc0000"), 
    name = "Layer"
  ) +
  geom_text(
    data = count_data,
    aes(x = site, y = 0.18, label = n, group = layer),
    position = position_dodge(0.8),
    size = 3,
    fontface = "italic",
    vjust = 1
  ) +
  labs(
    # title = "Dust Concentration (ppm) by Site and Layer",
    x = "",
    y = "Dust Concentration (ppm)",
    fill = "Layer"
  ) +
  theme_bw(base_size = 14)


svpth = "figures/dust-samples-25"
ggsave(file.path(svpth, "f2-dust-site-layer.png"), plot = f2,
       width = 8, height = 5, units = "in", dpi = 300)



  # Count data for labeling (by site and layer)
count_data <- updated_data %>%
  group_by(site, layer) %>%
  summarise(n = n(), .groups = "drop")

f3 = ggplot(updated_data, aes(x = site, y = dust_conc, fill = site)) +
  geom_boxplot(color = "black") +
  stat_summary(
    fun = mean, geom = "point", shape = 3, size = 2, color = "firebrick"
  ) +
  scale_fill_manual(
    values = rev(c("white", "lightblue", "skyblue", "skyblue4")),  
    name = "Layer"
  ) +
  geom_text(
    data = count_data,
    aes(x = site, y = 0.18, label = n),
    inherit.aes = FALSE,
    size = 3,
    fontface = "italic"
  ) +
  facet_wrap(~layer) +
  labs(
    x = "",
    y = "Dust Concentration (ppm)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  )


svpth = "figures/dust-samples-25"
ggsave(file.path(svpth, "f3-dust-site-layer-facet.png"), plot = f3,
       width = 8, height = 5, units = "in", dpi = 300)



# # # # # # # # # #
# stats
library(broom)

# Fit models by layer
lm_results <- updated_data %>%
  filter(!is.na(layer)) %>%  # ensure no missing layers
  group_by(layer) %>%
  do({
    model = lm(dust_conc ~ elevation, data = .)
    tidy_model = tidy(model)       # coefficients
    glance_model = glance(model)   # model-level stats
    tibble(
      estimate = tidy_model$estimate[2],   # slope
      p_value = tidy_model$p.value[2],     # p-value for slope
      r_squared = glance_model$r.squared   # R²
    )
  }) %>%
  ungroup()


lm_results
# layer        estimate  p_value r_squared
# <chr>           <dbl>    <dbl>     <dbl>
# 1 Clean     0.000000687 0.787      0.00266
# 2 Feb 4    -0.0000289   0.000365   0.301  
# 3 March 27 -0.0000264   0.000375   0.263  
# 4 March 4  -0.0000222   0.0213     0.155  

# statement
# Dust concentration decreased significantly with elevation in the Feb 4, March 27, and March 4 layers (p < 0.05), with the strongest relationship observed in the Feb 4 layer (R² = 0.301); no significant relationship was found in the Clean layer (p = 0.787, R² = 0.003).

# TEST FOR NORMALITY
updated_data %>%
  group_by(layer) %>%
  summarise(p_value = shapiro.test(dust_conc)$p.value)

# layer        p_value
# <chr>          <dbl>
#   1 Clean    0.000000497
# 2 Feb 4    0.00384    
# 3 March 27 0.00707    
# 4 March 4  0.0202   

# p-values < 0.05, which suggests that the data are not normally distributed for each layer

# WILCOX - non-parametric
pairwise.wilcox.test(updated_data$dust_conc, updated_data$site,
                     p.adjust.method = "bonferroni")

#    LP      MM      SN     
# MM 0.23440 -       -      
# SN 0.00409 1.00000 -      
# SS 0.00085 1.00000 1.00000
# P value adjustment method: bonferroni 

# summit sites significantly different than LP

# Filter data for March 27 layer
march27_data <- updated_data %>% filter(layer == "March 27")

# Run pairwise Wilcoxon test between the March 27 layer and other layers
pairwise.wilcox.test(
  march27_data$dust_conc, 
  march27_data$site,
  p.adjust.method = "bonferroni"
)
# LP      MM    SN   
# MM 0.021   -     -    
# SN 8.9e-06 1.000 -    
# SS 0.054   1.000 0.177

# significant MM vs LP

# STATEMENT
# Dust concentrations at sites SN and SS were significantly higher than at site LP (Pairwise Wilcox test, p = 0.0041 and 0.0009). No significant differences were observed between sites MM, SN, and SS.





kruskal.test(dust_conc ~ layer, data = updated_data)
# Kruskal-Wallis chi-squared = 69.275, df = 3, p-value = 6.103e-15
# small p value means significant difference between groups & higher chi-squared is stronger difference


# # ANOVA
# anova_model <- aov(dust_conc ~ site, data = updated_data)
# summary(anova_model)
# 
# # Tukey post-hoc test
# TukeyHSD(anova_model)
# 
# # diff         lwr          upr     p adj
# # MM-LP -0.022846403 -0.04752533  0.001832526 0.0804071
# # SN-LP -0.033401271 -0.05793270 -0.008869840 0.0030137
# # SS-LP -0.036136380 -0.06113190 -0.011140857 0.0014083
# # SN-MM -0.010554868 -0.03419594  0.013086204 0.6525632
# # SS-MM -0.013289977 -0.03741228  0.010832323 0.4814133
# # SS-SN -0.002735109 -0.02670649  0.021236268 0.9908876
# # Dust concentrations at sites SN and SS were significantly higher than at site LP (Tukey HSD, p = 0.0030 and 0.0014, respectively)



# Type of measurements

library(dplyr)

# Identify the rows with size == "Liter" or orientation == "V"
liter_v_subset <- updated_data %>%
  filter(size == "Liter" | orientation == "V")

# Extract unique combinations of site and layer from those rows
target_combos <- liter_v_subset %>%
  distinct(site, layer, sample_collect)

# Get all rows from updated_data that match any of those site/layer combinations
final_subset <- updated_data %>%
  semi_join(target_combos, by = c("site", "layer", "sample_collect"))


final_subset

library(dplyr)
library(ggplot2)

# Step 1: Filter only V samples
v_samples <- updated_data %>%
  filter(orientation == "V" & !is.na(site) & !is.na(layer) & !is.na(sample_collect))

# Step 2: For each V sample, find matching non-Vs by site, layer, and sample_collect
v_comparisons <- v_samples %>%
  rowwise() %>%
  mutate(
    group_mean = mean(
      updated_data %>%
        filter(
          site == site,
          layer == layer,
          sample_collect == sample_collect,
          orientation != "V",
          !is.na(dust_conc)
        ) %>%
        pull(dust_conc),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# See the comparison table
v_comparisons %>% select(id, site, layer, sample_collect, dust_conc, group_mean)

ggplot(v_comparisons, aes(x = group_mean, y = dust_conc)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Comparison of 'V' Orientation Samples to Group Mean",
    x = "Group Mean (Non-V Samples)",
    y = "'V' Orientation Measurement"
  ) +
  theme_minimal()








# ORIENTATION

# Step 1: Create group identifier
grouped_data <- updated_data %>%
  filter(!is.na(site), !is.na(layer), !is.na(sample_collect)) %>%
  mutate(group_id = paste(site, layer, sample_collect, sep = "_"))

# Step 2: Get V-oriented measurements
v_data <- grouped_data %>%
  filter(orientation == "V") %>%
  select(group_id, dust_conc) %>%
  rename(v_dust_conc = dust_conc)

# Step 3: Get average of non-V measurements in same group
group_means <- grouped_data %>%
  filter(orientation != "V" | is.na(orientation)) %>%
  group_by(group_id) %>%
  summarise(group_mean = mean(dust_conc, na.rm = TRUE), .groups = "drop")

# Step 4: Join V data with group means
comparison_df <- left_join(v_data, group_means, by = "group_id") %>%
  filter(!is.na(group_mean), !is.na(v_dust_conc))

ggplot(comparison_df, aes(x = group_mean, y = v_dust_conc)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "'V' Orientation vs Group Mean (non-V)",
    x = "Group Mean (non-V samples)",
    y = "'V' Orientation Dust Concentration"
  ) +
  theme_minimal()



# 23 samples used to compare orientation of measurements

grouped_data$orientation[is.na(grouped_data$orientation)] = "H"

# Summary stats
grouped_data %>%
  filter(orientation %in% c("V", "H")) %>%
  mutate(measurement_type = ifelse(orientation == "V", "V", "H")) %>%
  group_by(measurement_type) %>%
  summarise(
    mean_dust = mean(dust_conc, na.rm = TRUE),
    sd_dust = sd(dust_conc, na.rm = TRUE),
    n = n()
  )

ggplot(grouped_data %>% mutate(measurement_type = ifelse(orientation == "V", "V", "Other")),
       aes(x = measurement_type, y = dust_conc)) +
  geom_boxplot() +
  labs(title = "Dust Concentration by Measurement Type", x = "Measurement Type", y = "Dust Conc") +
  theme_minimal()









# Add difference and avg columns
comparison_df <- comparison_df %>%
  mutate(
    diff = v_dust_conc - group_mean,
    avg = (v_dust_conc + group_mean) / 2
  )

# Paired t-test
t.test(comparison_df$v_dust_conc, comparison_df$group_mean, paired = TRUE)

# Bland-Altman plot
ggplot(comparison_df, aes(x = avg, y = diff)) +
  geom_point() +
  geom_hline(yintercept = mean(comparison_df$diff, na.rm = TRUE), color = "blue") +
  geom_hline(yintercept = mean(comparison_df$diff, na.rm = TRUE) + 1.96 * sd(comparison_df$diff, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(comparison_df$diff, na.rm = TRUE) - 1.96 * sd(comparison_df$diff, na.rm = TRUE), linetype = "dashed", color = "red") +
  labs(title = "Bland–Altman Plot", x = "Average (V & Non-V)", y = "Difference (V - Non-V)")

lm_model <- lm(dust_conc ~ orientation, data = grouped_data)
summary(lm_model)

# statement
# Vertical (V) measurements had a slightly higher mean dust concentration (0.065 mg/m³) compared to horizontal (H) measurements (0.063 mg/m³), though with less variability (SD: 0.029 vs. 0.044). A paired t-test comparing V measurements to the average of non-V measurements from the same sampling group found no statistically significant difference (mean difference = –0.015 mg/m³, 95% CI: –0.032 to 0.002, p = 0.088).






# Sample data structure
library(ggplot2)

# Plot upper and lower depth ranges
ggplot(updated_data, aes(x = sample_collect, group = interaction(layer, site))) +
  geom_line(aes(y = upper_depth, color = layer), size = 1) +
  geom_line(aes(y = lower_depth, color = layer), size = 1) +
  geom_ribbon(aes(ymin = lower_depth, ymax = upper_depth, fill = layer), alpha = 0.3) +
  labs(x = "Sampling Date", y = "Depth (m)", title = "Depth Range of Layers Over Sampling Dates") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("Layer 1" = "blue", "Layer 2" = "green", "Layer 3" = "red")) +
  scale_fill_manual(values = c("Layer 1" = "blue", "Layer 2" = "green", "Layer 3" = "red"))

as.factor(updated_data$sample_collect)



library(ggplot2)
library(dplyr)

# Ensure correct date formats (if needed)
your_data <- updated_data
# your_data$sample_collect <- as.Date(your_data$sample_collect)
# your_data$sample_collect <- as.factor(your_data$sample_collect)
your_data$sample_collect <- factor(format(as.Date(your_data$sample_collect), "%B %d"), levels=c("March 21", "March 28", "April 04")) 
your_data$layer <- factor(your_data$layer, levels=c("Feb 4", "March 4", "March 27")) 


f4 <- your_data %>% filter(layer!="Clean") %>% 
  # ggplot(aes(x = sample_collect, y = upper_depth, group = layer, fill = layer)) +
  ggplot(aes(x = layer, y = upper_depth, group = layer, fill = layer)) +
  geom_boxplot() +
  # scale_fill_manual(
  #   values = c("lightblue", "skyblue", "skyblue4"),
  #   # values = c("white", "#f4cccc", "#ea9999", "#cc0000"), 
  #   name = "Layer"
  # ) +
  facet_wrap(~ site) +
  labs(
    x = "Layer",
    y = "Upper Depth (cm)",
    # title = "Upper Depth by Sample Date and Site",
    # subtitle = "Grouped and Colored by Layer"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  )

svpth = "figures/dust-samples-25"
ggsave(file.path(svpth, "f4-upper-depth-layer.png"), plot = f4,
       width = 8, height = 5, units = "in", dpi = 300)




ggplot(your_data, aes(x = interaction(sample_collect, layer), y = upper_depth, fill = layer)) +
  geom_boxplot() +
  facet_wrap(~ site, scales = "free_x") +
  labs(
    x = "Sample Date + Layer",
    y = "Upper Depth (cm)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)
library(ggplot2)

# Summarize: average upper_depth by sample_collect and layer
summary_data <- your_data %>% filter(layer!="Clean") %>% 
  group_by(site, sample_collect, layer) %>%
  summarise(mean_upper_depth = mean(upper_depth, na.rm = TRUE), .groups = "drop")

# Plot: line of mean upper_depth over time, colored by layer, faceted by site
summary_data %>% filter(layer!="Clean") %>% 
  ggplot(aes(x = sample_collect, y = mean_upper_depth, color = layer, group = layer)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~ site) +
  scale_y_reverse() +  # Reverse depth axis (deeper is lower)
  labs(
    x = "Sample Collection Date",
    y = "Mean Upper Depth (cm)",
    title = "Mean Upper Depth Over Time by Layer",
    color = "Layer"
  ) +
  theme_minimal()
