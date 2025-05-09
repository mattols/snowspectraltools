#
# USSDD Station Data
# 05/2025
#

station_path = "../snowspectraltools/data/UUSSD-2025/UUSSD.2025-05-08.csv"

# read the units row
units <- read.csv(station_path, skip = 11, nrows = 1, header = FALSE, stringsAsFactors = FALSE)
colnames_ <- read.csv(station_path, skip = 10, nrows = 1, header = TRUE, stringsAsFactors = FALSE)
dfs <- read.csv(station_path, skip = 11, header = TRUE, stringsAsFactors = FALSE)
# column names 
colnames(dfs) <- colnames(colnames_)
head(dfs)

# Required packages
library(dplyr);library(lubridate);library(zoo);library(ggplot2)

# Step 1: Convert Date_Time to POSIXct
dfs$Date_Time <- ymd_hms(dfs$Date_Time, tz = "Etc/GMT+7")  # MST is UTC-7 (Etc/GMT+7 reverses the sign)

# Step 2: Filter starting March 1
data_filtered <- dfs %>%
  filter(Date_Time >= as.POSIXct("2025-03-01", tz = "Etc/GMT+7"))

# Step 3: Subset until snow depth first reaches 0
# Ensure snow depth is numeric
data_filtered$snow_depth_set_1 <- as.numeric(data_filtered$snow_depth_set_1)

# Find the index where snow depth first hits 0
zero_index <- which(data_filtered$snow_depth_set_1 <= 0)[1]

# Subset up to that index
if (!is.na(zero_index)) {
  data_final <- data_filtered[1:zero_index, ]
} else {
  stop("Snow depth never reaches zero.")
}

# Step 4: Calculate 24-hour rolling means (window = 24 since data is hourly)
win = 6
data_final <- data_final %>%
  mutate(
    solar_rad_24hr = rollmean(solar_radiation_set_1, k = win, fill = NA, align = "right"),
    air_temp_24hr = rollmean(air_temp_set_1, k = win, fill = NA, align = "right"),
    rh_24hr = rollmean(relative_humidity_set_1, k = win, fill = NA, align = "right")
  )

# Step 5: Plot
ggplot(data_final, aes(x = Date_Time)) +
  geom_line(aes(y = solar_rad_24hr, color = "Solar Radiation")) +
  geom_line(aes(y = air_temp_24hr, color = "Air Temp (°C)")) +
  geom_line(aes(y = rh_24hr, color = "Relative Humidity (%)")) +
  labs(
    title = "24-hour Rolling Means: Solar Radiation, Air Temp, Relative Humidity",
    x = "Date",
    y = "Value",
    color = "Variable"
  ) +
  theme_minimal()







# library(patchwork)  # For combining ggplots vertically
library(gridExtra)

# Convert Date_Time to POSIXct
dfs$Date_Time <- ymd_hms(dfs$Date_Time, tz = "Etc/GMT+7")

# Filter from March 1 and subset until snow depth reaches zero
data_filtered <- dfs %>%
  filter(Date_Time >= as.POSIXct("2025-03-01", tz = "Etc/GMT+7"))

data_filtered$snow_depth_set_1 <- as.numeric(data_filtered$snow_depth_set_1)
zero_index <- which(data_filtered$snow_depth_set_1 <= 0)[1]

if (!is.na(zero_index)) {
  data_final <- data_filtered[1:zero_index, ]
} else {
  stop("Snow depth never reaches zero.")
}

# 24-hour rolling means
win = 1
data_final <- data_final %>%
  mutate(
    solar_rad_24hr = rollmean(solar_radiation_set_1, k = win, fill = NA, align = "right"),
    air_temp_24hr = rollmean(air_temp_set_1, k = win, fill = NA, align = "right"),
    rh_24hr = rollmean(relative_humidity_set_1, k = win, fill = NA, align = "right")
  )

# Top plot: Temp & RH
p1 <- ggplot(data_final, aes(x = Date_Time)) +
  geom_hline(yintercept = 32, color = "grey40", linetype="longdash") +
  geom_line(aes(y = rh_24hr, color = "Relative Humidity (%)"), size = 0.8) +
  geom_line(aes(y = air_temp_24hr* 9/5 + 32, color = "Air Temp (°F)"), size = 1) +
  scale_color_manual(values = c("Air Temp (°F)" = "firebrick", "Relative Humidity (%)" = "forestgreen")) +
  labs(
    # title = "24-hour Rolling Mean: Temperature & Relative Humidity",
       x = NULL, y = NULL, color = "") +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ) +
  theme(legend.position = "top")

# Bottom plot: Solar Radiation & Snow Depth
# p2 <- ggplot(data_final, aes(x = Date_Time)) +
#   geom_bar(aes(y = snow_depth_set_1), stat = "identity", fill = "blue", alpha = 0.3) +
#   geom_line(aes(y = solar_rad_24hr, color = "Solar Radiation (W~m^{-2})"), size = 1) +
#   scale_fill_manual(values = c("Snow depth (cm)" = "blue")) +
#   scale_color_manual(values = c("Solar Radiation (W~m^{-2})" = "darkgoldenrod")) +
#   labs(
#     # title = "Solar Radiation (Line) and Snow Depth (Bars)",
#        x = NULL, y = NULL, color = "") +
#   theme_bw(base_size = 14) +
#   theme(
#     plot.title = element_text(size = 18, face = "bold"),
#     axis.title.y = element_text(size = 16),
#     axis.text = element_text(size = 14),
#     legend.position = "none"
#   ) +
#   theme(legend.position = "top")

# Bottom plot: Solar Radiation & Snow Depth with labeled legend
p2 <- ggplot(data_final, aes(x = Date_Time)) +
  geom_bar(aes(y = snow_depth_set_1, fill = "Snow depth (mm)"), stat = "identity", alpha = 0.3) +
  geom_line(aes(y = solar_rad_24hr, color = "Solar Radiation (W/m²)"), size = 1) +
  scale_fill_manual(values = c("Snow depth (mm)" = "blue")) +
  scale_color_manual(values = c("Solar Radiation (W/m²)" = "darkgoldenrod")) +
  labs(
    # title = "Solar Radiation and Snow Depth",
    x = NULL, y = NULL, color = "", fill = ""
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ) +
  theme(legend.position = "top")


# Arrange the two plots vertically
fg1 = grid.arrange(p1, p2, ncol = 1)


svpth = "figures/dust-samples-25"
ggsave(file.path(svpth, "fg1-station-timeseries-3.png"), plot = fg1,
       width = 10, height = 7, units = "in", dpi = 300)


# Create a long-format version of the data for easier plotting
library(tidyr)

# Convert to long format for ggplot
radiation_data <- data_final %>%
  select(Date_Time,
         # PAR_1 = photosynthetically_active_radiation_set_1,
         # PAR_2 = photosynthetically_active_radiation_set_2,
         Solar = solar_radiation_set_1,
         Outgoing_SW = outgoing_radiation_sw_set_1) %>%
  pivot_longer(-Date_Time, names_to = "Variable", values_to = "Value")

# Create the plot
ggplot(radiation_data, aes(x = Date_Time, y = Value, color = Variable)) +
  geom_line(size = 1) +
  scale_color_manual(values = c(
    # "PAR_1" = "forestgreen",
    # "PAR_2" = "darkgreen",
    "Solar" = "darkorange",
    "Outgoing_SW" = "purple"
  )) +
  labs(
    title = "Radiation Components Over Time",
    x = "Date",
    y = "Radiation (W/m² or µmol/m²/s)",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(legend.position = "top")



# The two-channel split in the model is at 700 nm (280 - 700 VIS, 700 - 2800 (N)IR) 
# and depending on how, when, and where you measure or model the spectral irradiance 
# this fraction can shift a little bit. When I modeled it for clear skies, rural atmosphere, 
# 30 degree solar zenith angle it was 54% vis/46% nir for Reynolds Creek (direct+diffuse). 
# So the equations would be: 
# (1) WeightBB_alb = %In_VIS*Alb_VIS+%In_NIR*Alb_NIR 
# (2) Incoming= Net_Solar / (1- WeightBB_alb) 
# // Example for Net = 224.65, Vis_alb = 0.93, NIR_alb =0.59  
# WeightBB_alb: 0.54*0.93+0.46*0.59 = 0.70 | Incoming: 224.65/(1-0.76) = 995.37

# vis
photosynthetically_active_radiation_set_2 # out
photosynthetically_active_radiation_set_1 # in 

# conversion factor x 0.22 to get to W//m^2

solar_radiation_set_1
outgoing_radiation_sw_set_1 

dfs$weight_alb = 0.54 * (dfs$photosynthetically_active_radiation_set_2 / dfs$photosynthetically_active_radiation_set_1) + 
  0.46 * (dfs$outgoing_radiation_sw_set_1 / dfs$solar_radiation_set_1)
alpha = 
dfs$alb = pmax(0, pmin(dfs$outgoing_radiation_sw_set_1 / dfs$solar_radiation_set_1, 1))

plot(dfs$alb, type="l")




######
library(dplyr)
library(ggplot2)
library(scales)

# Step 1: Clean data
dfs2 <- dfs %>%
  mutate(
    solar = as.numeric(solar_radiation_set_1),
    outgoing = as.numeric(outgoing_radiation_sw_set_1),
    
    # Set 0s to NA
    solar = ifelse(solar == 0, NA, solar),
    outgoing = ifelse(outgoing == 0, NA, outgoing),
    
    # If solar is NA, outgoing must be NA
    outgoing = ifelse(is.na(solar), NA, outgoing),
    
    # Outgoing cannot be more than incoming
    outgoing = pmin(outgoing, solar, na.rm = TRUE),
    
    # Step 2: Calculate albedo
    albedo = outgoing / solar,
    
    # Force between 0 and 1
    albedo = pmin(pmax(albedo, 0.25), 1)
  )

# Optional: Filter to relevant time period (if needed)
dfs2 <- dfs2 %>% filter(Date_Time >= as.POSIXct("2025-03-01"), Date_Time <= as.POSIXct("2025-05-01"))

# Step 3: Plot solar radiation with albedo on right axis
ggplot(dfs2, aes(x = Date_Time)) +
  geom_line(aes(y = solar), color = "darkorange", size = 1) +
  geom_line(aes(y = albedo * max(solar, na.rm = TRUE)), color = "blue", size = 1) +  # scaled albedo
  scale_y_continuous(
    name = "Solar Radiation (W/m²)",
    sec.axis = sec_axis(~ . / max(dfs2$solar, na.rm = TRUE), name = "Albedo", labels = scales::number_format(accuracy = 0.01))
  ) +
  labs(title = "Solar Radiation and Albedo Over Time", x = "Date-Time") +
  theme_minimal() +
  theme(legend.position = "none")



library(dplyr)
library(ggplot2)
library(zoo)
library(scales)

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

dfs2 <- dfs2 %>% filter(Date_Time >= as.POSIXct("2025-03-01"), Date_Time <= as.POSIXct("2025-05-01"))

# Step 3: Plot with dual axis
ggplot(dfs2, aes(x = Date_Time)) +
  geom_line(aes(y = solar_roll6), color = "darkorange", size = 1) +
  geom_line(aes(y = albedo_roll6 * max(solar_roll6, na.rm = TRUE)), color = "deepskyblue", size = 1) +
  scale_y_continuous(
    name = "Solar Radiation (W/m²)",
    sec.axis = sec_axis(~ . / max(dfs2$solar_roll6, na.rm = TRUE), name = "Albedo", labels = scales::number_format(accuracy = 0.01))
  ) +
  labs(title = "6-hour Rolling Mean: Solar Radiation and Albedo", x = "Date-Time") +
  theme_minimal() +
  theme(legend.position = "none")



# library(dplyr)
# library(ggplot2)
# library(zoo)
# library(scales)

dfs2 <- dfs %>%
  mutate(
    solar = as.numeric(solar_radiation_set_1),
    outgoing = as.numeric(outgoing_radiation_sw_set_1),
    
    # Replace 0s with NA
    solar = ifelse(solar == 0, NA, solar),
    outgoing = ifelse(outgoing == 0, NA, outgoing),
    
    # Outgoing depends on solar
    outgoing = ifelse(is.na(solar), NA, pmin(outgoing, solar, na.rm = TRUE)),
    
    # Albedo calc, clamped
    albedo = outgoing / solar,
    albedo = pmin(pmax(albedo, 0), 1),
    
    # Rolling mean with partial windows
    solar_roll6 = rollapply(solar, width = 6, FUN = max, fill = NA, align = "right", partial = TRUE),
    albedo_roll12 = rollapply(albedo, width = 4, FUN = median, fill = NA, align = "right", partial = TRUE),
    # albedo_roll12 = zoo::na.approx(albedo_roll12, maxgap = 12, na.rm = TRUE),
    # albedo_smooth = rollapply(albedo_roll12, width = 44, FUN = mean, fill = NA, align = "right", partial = TRUE)
    
  ) %>%
  mutate(
    albedo_roll12 = zoo::na.approx(albedo_roll12, maxgap = 12, na.rm = TRUE)
  )

# Now fit loess with correct data argument
dfs2$albedo_loess <- predict(
  loess(albedo_roll12 ~ as.numeric(Date_Time), data = dfs2, span = 0.038),
  newdata = dfs2
)

dfs2$albedo_loess = pmin(dfs2$albedo_loess, 1)
# dfs$albedo_loess <- predict(loess(albedo_roll12 ~ as.numeric(dfs2$Date_Time), span = 0.2), newdata = dfs2)

# fit <- smooth.spline(x = as.numeric(dfs$Date_Time), y = dfs$albedo_roll12, spar = 0.7)
# dfs$albedo_spline <- predict(fit, x = as.numeric(dfs$Date_Time))$y
# 
# dfs2 <- dfs2 %>% filter(Date_Time >= as.POSIXct("2025-03-01"), Date_Time <= as.POSIXct("2025-05-01"))
# 
dfs2 <- dfs2 %>% filter(Date_Time >= as.POSIXct("2025-03-01"), Date_Time <= as.POSIXct("2025-05-01"))

# dfs2$net_solar = dfs2$solar_radiation_set_1 * (1- dfs2$albedo_loess)

# Plot with scaled albedo
fg2 = ggplot(dfs2, aes(x = Date_Time)) +
  geom_bar(aes(y = snow_depth_set_1, fill = "Snow depth (mm)"), stat = "identity", alpha = 0.3) +
  geom_line(aes(y = solar_radiation_set_1), color = "darkorange", size = 1) +
  geom_line(aes(y = albedo_loess * max(snow_depth_set_1, na.rm = TRUE)), color = "black", size = 1) +
  scale_fill_manual(name=NULL, values = c("Snow depth (mm)" = "deepskyblue")) +
  scale_color_manual(values = c("Solar Radiation (W/m²)" = "darkorange")) +
  scale_y_continuous(
    name = "Solar radiation (W/m²)",
    sec.axis = sec_axis(~ . / max(dfs2$snow_depth_set_1, na.rm = TRUE), name = "Surface albedo", labels = scales::number_format(accuracy = 0.01))
  ) +
  labs(x = NULL) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y.left = element_text(size = 16, color = "darkorange"),
    axis.text.y.left = element_text(color = "darkorange"),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "top"
  ) 


svpth = "figures/dust-samples-25"
ggsave(file.path(svpth, "fg2-station-albedo.png"), plot = fg2,
       width = 8, height = 6, units = "in", dpi = 300)




# # # # # # # #

library(ggplot2)

ggplot(dfs2, aes(x = Date_Time)) +
  # Bar for snow depth
  geom_bar(aes(y = snow_depth_set_1, fill = "Snow depth (cm)"), stat = "identity", alpha = 0.3) +
  
  # Line for solar radiation
  geom_line(aes(y = solar_radiation_set_1, color = "Solar Radiation (W/m²)"), size = 1) +
  
  # Line for albedo
  geom_line(aes(y = albedo_loess * max(snow_depth_set_1, na.rm = TRUE), color = "Albedo"), size = 1) +
  
  # Manual color/fill scales
  scale_fill_manual(name = NULL, values = c("Snow depth (cm)" = "deepskyblue")) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Solar Radiation (W/m²)" = "darkorange",
      "Albedo" = "black"
    )
  ) +
  
  # Y-axes
  scale_y_continuous(
    name = "Solar Radiation (W/m²)",
    sec.axis = sec_axis(~ . / max(dfs2$snow_depth_set_1, na.rm = TRUE), name = "Surface Albedo", labels = scales::number_format(accuracy = 0.01))
  ) +
  
  labs(x = NULL) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "top",
    legend.text = element_text(size = 14)
  )



