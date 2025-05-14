#
#
#
#


# Step 1: Clean & calculate albedo
dfs2 <- dfs %>%
  mutate(
    solar = as.numeric(solar_radiation_set_1),
    outgoing = as.numeric(outgoing_radiation_sw_set_1),
    
    # Set 0s to NA
    solar = ifelse(solar == 0, NA, solar),
    outgoing = ifelse(outgoing == 0, NA, outgoing),
    
    # If solar is NA, outgoing must be NA
    outgoing = ifelse(is.na(solar), NA, outgoing),
    
    # Outgoing can't exceed solar
    outgoing = pmin(outgoing, solar, na.rm = TRUE),
    
    # Calculate albedo
    albedo = outgoing / solar,
    albedo = pmin(pmax(albedo, 0), 1)
  ) %>%
  # Step 2: Apply 6-hour rolling means
  mutate(
    solar_roll6 = rollmean(solar, k = 1, fill = NA, align = "right"),
    albedo_roll6 = rollmean(albedo, k = 1, fill = NA, align = "right")
  )

# dfs2 <- dfs2 %>% filter(Date_Time >= as.POSIXct("2025-03-01"), Date_Time <= as.POSIXct("2025-05-01"))




# Air temperature and snow depth
library(dplyr)
library(ggplot2)
library(lubridate)

# 1. How does air temperature affect snow depth over time?
  # Hypothesis: As temperature increases, snow depth should decrease (especially during melt periods).
daily_data <- dfs2 %>%
  mutate(date = as.Date(Date_Time)) %>%
  group_by(date) %>%
  summarise(
    mean_temp = mean(air_temp_set_1, na.rm = TRUE),
    mean_snow_depth = mean(snow_depth_set_1, na.rm = TRUE)
  )

# Plot
fg3 = ggplot(daily_data, aes(x = mean_temp, y = mean_snow_depth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Air temperature and snow depth",
    x = "Mean daily temperature (°C)",
    y = "Mean daily snow depth (mm)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "top",
    legend.text = element_text(size = 14)
  )

fg3

svpth = "figures/dust-samples-25"
# ggsave(file.path(svpth, "fg3-station-airT-sdepth.png"), plot = fg3,
#        width = 6, height = 5, units = "in", dpi = 300)

model_temp_snow <- lm(mean_snow_depth ~ mean_temp, data = daily_data)
summary(model_temp_snow)
cor.test(daily_data$mean_temp, daily_data$mean_snow_depth, use = "complete.obs")
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1096.868     51.216  21.416  < 2e-16 ***
#   mean_temp    -43.781      9.762  -4.485 3.35e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 349.4 on 60 degrees of freedom
# Multiple R-squared:  0.2511,	Adjusted R-squared:  0.2386 
# F-statistic: 20.12 on 1 and 60 DF,  p-value: 3.352e-05

# Pearson's product-moment correlation
# 
# data:  daily_data$mean_temp and daily_data$mean_snow_depth
# t = -4.485, df = 60, p-value = 3.352e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.6673302 -0.2872654
# sample estimates:
#        cor 
# -0.5010812 




# 2. Is there a correlation between solar radiation and snowmelt (declining snow depth)?
  # Hypothesis: Higher solar radiation leads to greater snowmelt.
# Summarize daily solar radiation and snow depth
daily_radiation <- dfs2 %>%
  mutate(date = as.Date(Date_Time)) %>%
  group_by(date) %>%
  summarise(
    total_solar = sum(solar_radiation_set_1, na.rm = TRUE),
    mean_snow_depth = mean(snow_depth_set_1, na.rm = TRUE)
  )

ggplot(daily_radiation, aes(x = total_solar, y = mean_snow_depth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Daily Solar Radiation vs Snow Depth",
    x = "Total Daily Solar Radiation",
    y = "Mean Daily Snow Depth (mm)"
  )

model_solar_snow <- lm(mean_snow_depth ~ total_solar, data = daily_radiation)
summary(model_solar_snow)
cor.test(daily_radiation$total_solar, daily_radiation$mean_snow_depth, use = "complete.obs")


# ALBEDO?
# Aggregate daily values of albedo and snow depth
daily_albedo <- dfs2 %>%
  mutate(date = as.Date(Date_Time)) %>%
  group_by(date) %>%
  summarise(
    mean_albedo = mean(albedo_loess, na.rm = TRUE),
    mean_snow_depth = mean(snow_depth_set_1, na.rm = TRUE)
  )

# Plot albedo vs snow depth
ggplot(daily_albedo, aes(x = mean_albedo, y =mean_snow_depth)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(
    title = "Mean Daily Snow Depth vs Albedo",
    y = "Mean Daily Snow Depth (mm)",
    x = "Mean Daily Albedo"
  )

# Linear regression model
model_albedo <- lm(mean_snow_depth ~ mean_albedo, data = daily_albedo)
summary(model_albedo)

# Correlation
cor.test(daily_albedo$mean_albedo, daily_albedo$mean_snow_depth,use = "complete.obs")



# Next Step: Calculate Net Solar Radiation and Reassess

# Calculate Net Solar Radiation at the hourly level
dfs2 <- dfs2 %>%
  mutate(net_solar = solar_radiation_set_1 * (1 - albedo_loess))

# Aggregate daily values
daily_net_solar <- dfs2 %>%
  mutate(date = as.Date(Date_Time)) %>%
  group_by(date) %>%
  summarise(
    total_net_solar = sum(net_solar, na.rm = TRUE),
    mean_snow_depth = mean(snow_depth_set_1, na.rm = TRUE)
  )

# Plot Net Solar vs Snow Depth
fg5 = ggplot(daily_net_solar, aes(x = total_net_solar, y = mean_snow_depth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = "Solar radiation and snow depth",
    x = "Total daily net solar radiation",
    y = "Mean daily snow depth (mm)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "top",
    legend.text = element_text(size = 14)
  )

svpth = "figures/dust-samples-25"
# ggsave(file.path(svpth, "fg5-station-solarRad-sdepth.png"), plot = fg5,
#        width = 6, height = 5, units = "in", dpi = 300)

# Linear model
model_net_solar <- lm(mean_snow_depth ~ total_net_solar, data = daily_net_solar)
summary(model_net_solar)

# Correlation test
cor.test(daily_net_solar$total_net_solar, daily_net_solar$mean_snow_depth, use = "complete.obs")







# 3. When does the snowpack peak, and how fast does it melt?
  # Hypothesis: Snow depth increases to a seasonal peak and then declines rapidly during spring melt.
fg4 = ggplot(daily_data, aes(x = date, y = mean_snow_depth)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Snow depth during melt season",
    x = "",
    y = "Mean daily snow depth (mm)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "top",
    legend.text = element_text(size = 14)
  )

svpth = "figures/dust-samples-25"
# ggsave(file.path(svpth, "fg4-station-meltout-depth.png"), plot = fg4,
#        width = 6, height = 5, units = "in", dpi = 300)



daily_data %>% # filter(date > as.Date("2025-03-08")) %>% 
  filter(mean_snow_depth == max(mean_snow_depth, na.rm = TRUE))

# Find peak date
peak_day <- daily_data %>% # filter(date > as.Date("2025-03-08")) %>% 
  filter(mean_snow_depth == max(mean_snow_depth, na.rm = TRUE)) %>%
  pull(date)

# Estimate melt rate post-peak
melt_data <- daily_data %>% # filter(date > as.Date("2025-03-08")) %>% 
  filter(date > peak_day) %>%
  mutate(days_since_peak = as.numeric(date - peak_day))

model_melt_rate <- lm(mean_snow_depth ~ days_since_peak, data = melt_data)
summary(model_melt_rate)




# 4. Do wind speed or humidity influence snowpack loss?
  # Hypothesis: Higher wind speed and lower humidity might accelerate snowmelt via sublimation and increased turbulent heat flux.
daily_weather <- dfs2 %>%
  mutate(date = as.Date(Date_Time)) %>%
  group_by(date) %>%
  summarise(
    mean_snow_depth = mean(snow_depth_set_1, na.rm = TRUE),
    mean_wind_speed = mean(wind_speed_set_1, na.rm = TRUE),
    mean_humidity = mean(relative_humidity_set_1, na.rm = TRUE)
  )

# Plot wind speed vs snow depth
ggplot(daily_weather, aes(x = mean_wind_speed, y = mean_snow_depth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "purple") +
  labs(
    title = "Wind Speed vs Snow Depth",
    x = "Mean Daily Wind Speed (m/s)",
    y = "Mean Daily Snow Depth (mm)"
  )

model_wind <- lm(mean_snow_depth ~ mean_wind_speed, data = daily_weather)
summary(model_wind)
cor.test(daily_weather$mean_wind_speed, daily_weather$mean_snow_depth, use = "complete.obs")

model_humidity <- lm(mean_snow_depth ~ mean_humidity, data = daily_weather)
summary(model_humidity)
cor.test(daily_weather$mean_humidity, daily_weather$mean_snow_depth, use = "complete.obs")








# 5. Can we detect snowmelt events after warm or sunny days?
daily_data <- daily_data %>%
  arrange(date) %>%
  mutate(
    snow_change = mean_snow_depth - lag(mean_snow_depth),
    melt_event = ifelse(snow_change < -10, TRUE, FALSE) # threshold for significant melt
  )

# View days with melt
daily_data %>%
  filter(melt_event == TRUE) %>%
  select(date, mean_temp, snow_change)


melt_data_log <- daily_data %>%
  mutate(melt_event = ifelse(snow_change < -10, 1, 0))

model_melt_temp <- glm(melt_event ~ mean_temp, family = "binomial", data = melt_data_log)
summary(model_melt_temp)


# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   0.7672     0.3863   1.986  0.04700 *  
#   mean_temp     0.4459     0.1231   3.624  0.00029 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 68.05  on 60  degrees of freedom
# Residual deviance: 43.05  on 59  degrees of freedom
# (1 observation deleted due to missingness)
# AIC: 47.05
# 
# Number of Fisher Scoring iterations: 6








#
#


#

#

### Estimating snow settling rates


daily_snow <- dfs2 %>%
  mutate(date = as.Date(Date_Time)) %>%
  group_by(date) %>%
  summarise(
    mean_snow_depth = mean(snow_depth_set_1, na.rm = TRUE),
    snowfall = max(snow_depth_set_1, na.rm = TRUE) - min(snow_depth_set_1, na.rm = TRUE)
  ) %>%
  mutate(snow_change = mean_snow_depth - lag(mean_snow_depth))

# Tag peaks and assign settling sequences
daily_snow <- daily_snow %>%
  mutate(peak = if_else(snow_change > 100, TRUE, FALSE)) %>%
  mutate(settling_period = cumsum(replace_na(peak, FALSE)))

# View settling period example
head(daily_snow, 10)

# Pick one settling period for simplicity (e.g., after a large snowfall)
settling_data <- daily_snow %>%
  filter(settling_period == 3) %>%
  mutate(days_since_peak = row_number() - 1)

# Fit exponential decay model: Snow depth = a * exp(-b * t)
nls_model <- nls(mean_snow_depth ~ a * exp(-b * days_since_peak),
                 data = settling_data,
                 start = list(a = max(settling_data$mean_snow_depth), b = 0.1))

summary(nls_model)

# Plot
ggplot(settling_data, aes(x = days_since_peak, y = mean_snow_depth)) +
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(settling_data$mean_snow_depth), b = 0.1)),
              color = "blue", se = FALSE) +
  labs(title = "Snow Settling After Storm", x = "Days Since Peak", y = "Snow Depth (mm)")



#
#
#
#
#
#
#
# ALBEDO DECAY

daily_albedo <- dfs2 %>%
  mutate(date = as.Date(Date_Time)) %>%
  group_by(date) %>%
  summarise(
    mean_snow_depth = mean(snow_depth_set_1, na.rm = TRUE),
    mean_albedo = mean(albedo_loess, na.rm = TRUE)
  ) %>%
  mutate(snow_change = mean_snow_depth - lag(mean_snow_depth),
         new_snow = if_else(snow_change > 10, TRUE, FALSE),
         last_snow_day = if_else(new_snow, date, NA)) %>%
  fill(last_snow_day, .direction = "down") %>%
  mutate(days_since_snow = as.numeric(date - last_snow_day))

# Remove NAs
albedo_decay_data <- daily_albedo %>%
  filter(!is.na(mean_albedo), !is.na(days_since_snow), days_since_snow >= 0)

# Fit decay model: Albedo = a * exp(-b * days_since_snow)
albedo_model <- nls(mean_albedo ~ a * exp(-b * days_since_snow),
                    data = albedo_decay_data,
                    start = list(a = 0.85, b = 0.1))

summary(albedo_model)

# Plot
fg6 = ggplot(albedo_decay_data, aes(x = days_since_snow, y = mean_albedo)) +
  geom_point(alpha = 0.6) +
  stat_smooth(method = "nls", formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = 0.85, b = 0.1)),
              color = "darkred", se = FALSE) +
  labs(title = "Albedo decay since last snowfall", 
       x = "Days since snow", y = "Snow albedo")  +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "top",
    legend.text = element_text(size = 14)
  )

fg6

svpth = "figures/dust-samples-25"
# ggsave(file.path(svpth, "fg6-albedo-decay.png"), plot = fg6,
#        width = 6, height = 5, units = "in", dpi = 300)











library(dplyr)
library(lubridate)
library(minpack.lm)  # for robust nonlinear regression

# Assume daily_albedo has columns: date, mean_albedo, days_since_snow

# 1. Add seasonal label
daily_albedo <- daily_albedo %>%
  mutate(
    month = month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "DJF",
      month %in% c(3, 4, 5)  ~ "MAM",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(season))

# 2. Define function to fit exponential decay: albedo = a * exp(-b * days_since_snow)
fit_decay_model <- function(data) {
  tryCatch({
    nlsLM(
      mean_albedo ~ a * exp(-b * days_since_snow),
      data = data,
      start = list(a = 0.7, b = 0.03),
      control = nls.lm.control(maxiter = 100)
    )
  }, error = function(e) NA)
}

# 3. Fit models by season
models_by_season <- daily_albedo %>%
  group_by(season) %>%
  group_map(~ {
    model <- fit_decay_model(.x)
    list(
      season = unique(.x$season),
      model = model,
      summary = if (!is.na(model)[[1]]) summary(model) else NULL
    )
  })

# 4. Print results
for (result in models_by_season) {
  cat("Season:", result$season, "\n")
  if (!is.null(result$summary)) {
    print(result$summary)
  } else {
    cat("Model failed to converge.\n")
  }
  cat("\n")
}

