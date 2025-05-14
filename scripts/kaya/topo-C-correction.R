#
# Topographic correction example
# Using the C method
#

library(terra)

# --- INPUTS ---
# Load your data (if not already loaded)
library(tidyr)
library(terra)
library(dplyr)
library(ggplot2)

# bring in dem
dem0 <- rast("~/src/gds-R/data_tmp/wasatch_dem/ASTGTM2_N40W112_dem.tif")
dem0

# define path
pth = "../snowspectraltools/data/dust/landsat_wasatch"
list.files(pth)

# read in May 1
lspath <- list.files(pth,
                     pattern = ".*04.*B[1-7].TIF", full.names = T)
lspath
may_1 <- rast(lspath)

# create an extent to crop
# ext <- ext(-111.85, -111.550, 40.37, 40.60)
ext <- ext(-111.85, -111.50, 40.35, 40.70)
extent <- vect(ext, crs = "EPSG:4326")
crop_ext <- project(extent, crs(may_1))  # project to WGS 84 UTM Zone 12

# crop the rasters
may <- crop(may_1, crop_ext)
dem_prj <- project(dem0, crs(may_albedo))
dem_res <- resample(dem_prj, may)
dem <- crop(dem_res, crop_ext)

dem_ft <- dem * 3.281

plot(may[[1]])

# MTL file
meta_path <- list.files(pth, pattern = "MTL.txt", full.names = TRUE)
meta_path <- meta_path[1]
meta <- readLines(meta_path)
meta_spec <- grep("LEVEL2_SURFACE_REFLECTANCE_PARAMETERS", meta)
meta_final <- meta[meta_spec[1]:meta_spec[2]]
grep("REF.*MULT", meta_final, value=T)[1]  # mult value (scale)
grep("REF.*ADD", meta_final, value=T)[1]   # add value (offset)

MULT <- 2.75e-05
ADD <- -0.2
SUN_AZIMUTH = 148.26562962                                                                              
SUN_ELEVATION = 44.88571482


may_sr <- (may * MULT) + ADD
may_sr <- crop(may_sr, crop_ext)


# Set solar angles (degrees) for the image
solar_zenith <- 90 - SUN_ELEVATION
solar_azimuth <- SUN_AZIMUTH

# --- COMPUTE TERRAIN PARAMETERS ---
# Slope and aspect from DEM
slope <- terrain(dem, v = "slope", unit = "radians")
aspect <- terrain(dem, v = "aspect", unit = "radians")

# Convert solar angles to radians
sz_rad <- solar_zenith * pi / 180
sa_rad <- solar_azimuth * pi / 180

# Compute Illumination (IL): cosine of incidence angle
# cos(i) = cos(θz)*cos(α) + sin(θz)*sin(α)*cos(φ - φs)
cos_i <- cos(sz_rad) * cos(slope) +
  sin(sz_rad) * sin(slope) * cos(sa_rad - aspect)

# Clip negative values to avoid issues
cos_i[cos_i < 0] <- 0.0001

# --- C-CORRECTION ---
# Initialize empty list for corrected bands
corrected_bands <- rast(may_sr, nlyr=nlyr(may_sr)) #list()

for (i in 1:nlyr(may_sr)) {
  print(paste("Running", i, "of", nlyr(may_sr)))
  band <- may_sr[[i]]
  
  # Mask invalid data
  valid <- !is.na(band) & !is.na(cos_i)
  
  # Extract values for regression
  refl_vals <- values(band, mat = FALSE, na.rm = FALSE)
  illum_vals <- values(cos_i, mat = FALSE, na.rm = FALSE)
  
  # Keep only valid pixels
  idx <- which(valid[])
  refl_vals <- refl_vals[idx]
  illum_vals <- illum_vals[idx]
  
  # Perform linear regression: ρ_T = b + m * IL
  lm_fit <- lm(refl_vals ~ illum_vals)
  b <- coef(lm_fit)[1]
  m <- coef(lm_fit)[2]
  
  # Avoid divide by zero
  if (is.na(m) || m == 0) {
    warning(paste("Band", i, ": Invalid slope, skipping C-correction."))
    corrected_bands[[i]] <- band
    next
  }
  
  # Compute C = b / m
  C <- b / m
  print(paste("C=",C))
  
  # Apply C-correction: ρ_H = ρ_T * (cosθz + C) / (IL + C)
  rho_H <- band * ((cos(sz_rad) + C) / (cos_i + C))
  
  # Set negative values to NA (optional)
  rho_H[rho_H < 0] <- NA
  
  corrected_bands[[i]] <- rho_H
}

# Stack corrected bands
corrected_stack <- corrected_bands
names(corrected_stack) <- names(may_sr)
corrected_stack <- clamp(corrected_stack, 0, 1)

# --- SAVE OUTPUT ---
# writeRaster(corrected_stack, "corrected_may_sr.tif", overwrite = TRUE)


# MASK and Create albedo - WANG 2016 Paper
# create the NDSI function 
NDSI <- function(image){
  ndsi_no_thresh <- (image[[3]] - image[[6]]) / (image[[3]] + image[[6]])
  print("Calculating NDSI...")
  ndsi <- ndsi_no_thresh > 0.4
  print("NDSI Complete !")
  return(ndsi)
}

ndsi_0 <- NDSI(may_sr)
ndsi_corr <- NDSI(corrected_stack)
par(mfrow = c(1,2))
plot(ndsi_0);plot(ndsi_corr)
plot(ndsi_0 - ndsi_corr)

may_mask <- mask(may_sr, ndsi_0, maskvalue = FALSE)
may_corr_mask <- mask(corrected_stack, ndsi_corr, maskvalue = FALSE)
par(mfrow = c(1,2))
plot(may_mask[[2]]);plot(may_corr_mask[[2]])

may_albedo <- ((1.2242 * may_mask[[2]]) + (-0.4318 * may_mask[[3]]) + (-0.3446 * may_mask[[4]]) + (0.3367 * may_mask[[5]]) + (0.1834 * may_mask[[6]]) + (0.2555 * may_mask[[7]])) - 0.0052
may_corr_albedo <- ((1.2242 * may_corr_mask[[2]]) + (-0.4318 * may_corr_mask[[3]]) + (-0.3446 * may_corr_mask[[4]]) + (0.3367 * may_corr_mask[[5]]) + (0.1834 * may_corr_mask[[6]]) + (0.2555 * may_corr_mask[[7]])) - 0.0052
par(mfrow = c(1,2))
plot(may_albedo, main = "04/12/2024 Surface Albedo (uncorrected)", mar=c(2,2,1,3))
plot(may_corr_albedo, main = "04/12/2024 Topo-C Correction", mar=c(2,2,1,3))



# Set up side-by-side plots with consistent layout
par(mfrow = c(1, 2), mar = c(4, 4, 3, 5))  # Margins: bottom, left, top, right

# Define a consistent color scale for both rasters
albedo_range <- range(values(may_albedo), values(may_corr_albedo), na.rm = TRUE)
col_pal <- terrain.colors(100)  # or try viridis::viridis(100) for better perceptual scale

# Plot uncorrected albedo
plot(may_albedo,
     main = "04/12/2024 Surface Albedo (Uncorrected)",
     col = col_pal,
     zlim = albedo_range,
     legend.args = list(text = "Albedo", side = 4, line = 2.5, cex = 0.9),
     axes = FALSE, box = FALSE)

# Add a simple axis
axis(1); axis(2)

# Plot corrected albedo
plot(may_corr_albedo,
     main = "04/12/2024 Topo-C Corrected Albedo",
     col = col_pal,
     zlim = albedo_range,
     legend.args = list(text = "Albedo", side = 4, line = 2.5, cex = 0.9),
     axes = FALSE, box = FALSE)

# Add axis again
axis(1); axis(2)



# Load necessary packages
library(raster)
library(rasterVis)
library(viridis)
library(gridExtra)

# Compute difference raster
diff_albedo <- may_corr_albedo - may_albedo

# Define common color range for the first two plots
zlim_albedo <- range(values(may_albedo), values(may_corr_albedo), na.rm = TRUE)

# Plot 1: Uncorrected albedo
p1 <- levelplot(may_albedo, margin = FALSE,
                main = "04/12/2024 Surface Albedo",
                col.regions = viridis(100),
                at = seq(zlim_albedo[1], zlim_albedo[2], length.out = 100),
                colorkey = list(space = "right", width = 1, labels = list(cex = 0.8)))

# Plot 2: Topo-C Corrected albedo
p2 <- levelplot(may_corr_albedo, margin = FALSE,
                main = "Topo-C Corrected Albedo",
                col.regions = viridis(100),
                at = seq(zlim_albedo[1], zlim_albedo[2], length.out = 100),
                colorkey = list(space = "right", width = 1, labels = list(cex = 0.8)))

# Plot 3: Difference (Corrected - Uncorrected)
diff_range <- range(values(diff_albedo), na.rm = TRUE)
p3 <- levelplot(diff_albedo, margin = FALSE,
                main = "Corrected - Uncorrected",
                col.regions = viridis(100, option = "B"),
                at = seq(diff_range[1], diff_range[2], length.out = 100),
                colorkey = list(space = "right", width = 1, labels = list(cex = 0.8)))

# Combine plots side-by-side
grid.arrange(p1, p2, p3, ncol = 3)

# Combine with arrangeGrob
p_combined <- arrangeGrob(p1, p2, p3, ncol = 3)

# Now you can save using ggsave
ggsave(filename = file.path(svpth, "fig-rast-compare.png"),
       plot = p_combined,
       width = 12, height = 5, units = "in", dpi = 300)

#
#
#
# NEW plot
library(rasterVis)
library(viridis)
library(gridExtra)

# Common settings
zlim_albedo <- range(values(may_albedo), values(may_corr_albedo), na.rm = TRUE)
axis_cex <- 1.2

# Plot 1: Uncorrected albedo
p1 <- levelplot(may_albedo, margin = FALSE,
                main = "04/12/2024 Surface Albedo",
                col.regions = viridis(100),
                at = seq(zlim_albedo[1], zlim_albedo[2], length.out = 100),
                colorkey = list(space = "right", width = 1, labels = list(cex = 0.9)),
                scales = list(y = list(draw = FALSE), x = list(cex = axis_cex)),
                par.settings = list(axis.text = list(cex = axis_cex)))

# Plot 2: Topo-C Corrected albedo (no colorbar, no y-axis)
p2 <- levelplot(may_corr_albedo, margin = FALSE,
                main = "Topo-C Corrected Albedo",
                col.regions = viridis(100),
                at = seq(zlim_albedo[1], zlim_albedo[2], length.out = 100),
                colorkey = FALSE,
                scales = list(y = list(draw = FALSE), x = list(cex = axis_cex)),
                par.settings = list(axis.text = list(cex = axis_cex)))

# Plot 3: Difference (Corrected - Uncorrected)
diff_range <- range(values(diff_albedo), na.rm = TRUE)
p3 <- levelplot(diff_albedo, margin = FALSE,
                main = "Corrected - Uncorrected",
                col.regions = viridis(100, option = "B"),
                at = seq(diff_range[1], diff_range[2], length.out = 100),
                colorkey = list(space = "right", width = 1, labels = list(cex = 0.9)),
                scales = list(y = list(draw = FALSE), x = list(cex = axis_cex)),
                par.settings = list(axis.text = list(cex = axis_cex)))

# Arrange with tighter spacing
p_combined <- arrangeGrob(p1, p2, p3,
                          ncol = 3,
                          widths = c(1.05, 1, 1.1))  # Adjust spacing between plots

# Save
ggsave(filename = file.path(svpth, "fig-rast-compare-2.png"),
       plot = p_combined,
       width = 12, height = 5, units = "in", dpi = 300)


#
#
#
#
library(rasterVis)
library(viridis)
library(gridExtra)

# Shared axis label size
axis_cex <- 1.2

# Define color ranges
zlim_albedo <- range(values(may_albedo), values(may_corr_albedo), na.rm = TRUE)
diff_range <- range(values(may_corr_albedo - may_albedo), na.rm = TRUE)

# Plot 1: Uncorrected albedo
p1 <- levelplot(may_albedo, margin = FALSE,
                main = "04/12/2024 Surface Albedo",
                col.regions = viridis(100),
                at = seq(zlim_albedo[1], zlim_albedo[2], length.out = 100),
                colorkey = list(space = "right", width = 1, labels = list(cex = 0.9)),
                scales = list(x = list(cex = axis_cex), y = list(cex = axis_cex)),
                par.settings = list(axis.text = list(cex = axis_cex)))

# Plot 2: Topo-C Corrected albedo (no colorbar, no y-axis)
p2 <- levelplot(may_corr_albedo, margin = FALSE,
                main = "Topo-C Corrected Albedo",
                col.regions = viridis(100),
                at = seq(zlim_albedo[1], zlim_albedo[2], length.out = 100),
                colorkey = FALSE,
                scales = list(x = list(cex = axis_cex), y = list(draw = FALSE)),
                par.settings = list(axis.text = list(cex = axis_cex)))

# Plot 3: Difference (Corrected - Uncorrected)
p3 <- levelplot(may_corr_albedo - may_albedo, margin = FALSE,
                main = "Corrected - Uncorrected",
                col.regions = viridis(100, option = "B"),
                at = seq(diff_range[1], diff_range[2], length.out = 100),
                colorkey = list(space = "right", width = 1, labels = list(cex = 0.9)),
                scales = list(x = list(cex = axis_cex), y = list(draw = FALSE)),
                par.settings = list(axis.text = list(cex = axis_cex)))

# Arrange without manual width override — this ensures equal size panels
p_combined <- arrangeGrob(p1, p2, p3, ncol = 3)

# Save the combined plot
ggsave(filename = file.path(svpth, "fig-rast-compare-3.png"),
       plot = p_combined,
       width = 12, height = 5, units = "in", dpi = 300)








#
#
# PLOTS
#

library(dplyr)
library(ggplot2)
library(tidyr)

# --- Inputs ---
# SpatRasters: may_albedo, may_corr_albedo, dem

# --- Bin elevation ---
elevation_bins <- 100  # meters
dem_binned <- floor(values(dem_ft) / elevation_bins) * elevation_bins

# Extract values from rasters
df <- data.frame(
  elev_bin = dem_binned,
  albedo_uncorrected = values(may_albedo),
  albedo_corrected   = values(may_corr_albedo)
)
names(df) = c("elev_bin", "albedo_uncorrected","albedo_corrected")
# Remove NA values
df <- df %>% filter(!is.na(elev_bin), !is.na(albedo_uncorrected), !is.na(albedo_corrected))

# --- Summarize mean and SD by elevation bin ---
summary_df <- df %>%
  pivot_longer(cols = starts_with("albedo"), names_to = "type", values_to = "albedo") %>%
  group_by(elev_bin, type) %>%
  summarise(
    mean_albedo = mean(albedo),
    sd_albedo = sd(albedo),
    .groups = "drop"
  )

# --- Plot ---
f02 = ggplot(summary_df, aes(x = elev_bin, y = mean_albedo, color = type, fill = type)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_albedo - sd_albedo, ymax = mean_albedo + sd_albedo), alpha = 0.2, color = NA) +
  labs(
    # title = "Albedo and Elevation",
    x = "Elevation (ft)",
    y = "Albedo",
    color = "",
    fill = ""
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "top"
  )

f02
svpth = "figures/dust-samples-25"
ggsave(file.path(svpth, "f0-alb-elv-2.png"), plot = f02,
       width = 8, height = 5, units = "in", dpi = 300)


#
#
#
#
# --- Compute Aspect ---
aspect <- terrain(dem, v = "aspect", unit = "degrees")

# Bin aspect into 10° bins (or adjust as needed)
# aspect_binned <- floor(values(aspect) / 10) * 10
aspect_binned <- floor(values(aspect) / 15) * 15

# Extract albedo and aspect values
dfa <- data.frame(
  aspect_bin = aspect_binned,
  albedo_uncorrected = values(may_albedo),
  albedo_corrected   = values(may_corr_albedo)
)

names(dfa) = c("aspect_bin", "albedo_uncorrected","albedo_corrected")

# Remove NA values
dfa <- dfa %>% filter(!is.na(aspect_bin), !is.na(albedo_uncorrected), !is.na(albedo_corrected))

# --- Summarize mean and SD by aspect bin ---
summary_dfa <- dfa %>%
  pivot_longer(cols = starts_with("albedo"), names_to = "type", values_to = "albedo") %>%
  group_by(aspect_bin, type) %>%
  summarise(
    mean_albedo = mean(albedo),
    sd_albedo = sd(albedo),
    .groups = "drop"
  )

# Wrap aspect back into 0–360 range
summary_dfa$aspect_bin <- summary_dfa$aspect_bin %% 360

# --- Polar Plot ---
ggplot(summary_dfa, aes(x = aspect_bin, y = mean_albedo, color = type, fill = type)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_albedo - sd_albedo, ymax = mean_albedo + sd_albedo), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(0, 360, by = 45), limits = c(0, 360)) +
  coord_polar(start = pi/2, direction = -1) +
  labs(
    title = "Albedo vs Aspect",
    x = "Aspect (degrees)",
    y = "Albedo",
    color = "Albedo Type",
    fill = "Albedo Type"
  ) +
  theme_minimal()

# fix direction
ggplot(summary_dfa, aes(x = aspect_bin, y = mean_albedo, color = type, fill = type)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_albedo - sd_albedo, ymax = mean_albedo + sd_albedo), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(0, 360, by = 45), limits = c(0, 360)) +
  coord_polar(start = pi / 2, direction = 1) +  # Start at top and move clockwise
  geom_point(size = 1.5, alpha = 0.6) +
  geom_text(
    data = summary_dfa %>%
      filter(type == "albedo_corrected") %>%
      slice(seq(1, n(), by = 2)),
    aes(label = round(mean_albedo, 2)),
    size = 3, vjust = -0.5, check_overlap = TRUE
  ) +
  labs(
    # title = "Albedo vs Aspect",
    x = "Aspect",
    y = "",
    color = "Albedo Type",
    fill = "Albedo Type"
  ) +
  theme_minimal()




# # ADD LABELS
# # THIS ONE
ggplot(summary_dfa, aes(x = aspect_bin, y = mean_albedo, color = type, fill = type)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_albedo - sd_albedo, ymax = mean_albedo + sd_albedo), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(0, 360, by = 45), limits = c(0, 360)) +
  coord_polar(start = pi/2, direction = -1) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_text(aes(label = round(mean_albedo, 2)),
            size = 3, vjust = -0.5, check_overlap = TRUE) +
  labs(
    # title = "Albedo vs Aspect",
    x = "Aspect (degrees)",
    y = "Albedo",
    color = "Albedo Type",
    fill = "Albedo Type"
  ) +
  theme_minimal()

f01 = ggplot(summary_dfa, aes(x = aspect_bin, y = mean_albedo, color = type, fill = type)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_albedo - sd_albedo, ymax = mean_albedo + sd_albedo), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(0, 360, by = 45), limits = c(0, 360)) +
  coord_polar(start = pi / 2, direction = 1) +  # Start at top and move clockwise
  geom_point(size = 1.5, alpha = 0.6) +
  geom_text(
    data = summary_dfa %>%
      filter(type == "albedo_corrected") %>%
      slice(seq(1, n(), by = 2)),
    aes(label = round(mean_albedo, 2)),
    size = 5, vjust = -0.5, check_overlap = TRUE
  ) +
  labs(
    title = "Snow Albedo",
    x = "Aspect",
    y = "",
    color = "",
    fill = ""
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16),
    # axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "top"
  )

f01

svpth = "figures/dust-samples-25"
ggsave(file.path(svpth, "f0-alb-aspect-2.png"), plot = f01,
       width = 8, height = 7, units = "in", dpi = 300)



# # Add TEXT
# ggplot(summary_df, aes(x = aspect_bin, y = mean_albedo, color = type, fill = type)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = mean_albedo - sd_albedo, ymax = mean_albedo + sd_albedo), alpha = 0.2, color = NA) +
#   scale_x_continuous(breaks = seq(0, 360, by = 45), limits = c(0, 360)) +
#   coord_polar(start = pi/2, direction = -1) +
#   geom_point(size = 1.5, alpha = 0.6) +
#   geom_text(
#     data = summary_df %>%
#       filter(type == "albedo_corrected") %>%
#       slice(seq(1, n(), by = 2)),  # keep every other row
#     aes(label = round(mean_albedo, 2)),
#     size = 3, vjust = -0.5, check_overlap = TRUE
#   ) +
#   labs(
#     title = "Albedo vs Aspect",
#     x = "Aspect (degrees)",
#     y = "Albedo",
#     color = "Albedo Type",
#     fill = "Albedo Type"
#   ) +
#   theme_minimal()




#
#
# ELEVATION and ASPECT

library(dplyr)
library(terra)

# Assuming 'dem_ft' is a raster object for the DEM in feet and 'summary_df' contains columns for 'aspect_bin' and 'mean_albedo'

# Step 1: Load the DEM and extract the values into the summary data frame
# Assuming the 'dem_ft' raster corresponds to the same grid as your 'summary_df'

# If 'dem_ft' is a raster, extract values of the DEM corresponding to the 'summary_df' locations
# We assume 'summary_df' has coordinates or an equivalent that matches the DEM grid
library(sf)
library(terra)

# Assuming 'summary_df' has columns for coordinates, for example, 'longitude' and 'latitude'

# Convert summary_df to a spatial points object (sf format)
summary_sf <- st_as_sf(summary_df, coords = c("longitude", "latitude"), crs = 4326)  # Adjust CRS as needed

# Step 1: Extract DEM values for each point (coordinates)
summary_sf <- summary_sf %>%
  mutate(dem_value = extract(dem_ft, summary_sf))  # Extract DEM values from the 'dem_ft' raster

# Extract the DEM values into the summary_df (assuming summary_df contains 'x' and 'y' coordinates)
summary_df2 <- summary_df %>%
  mutate(dem_value = extract(dem_ft, cbind(x, y)))  # Extract DEM value at each (x, y)

# Step 2: Create elevation bins (500 feet intervals) from DEM
summary_df2 <- summary_df2 %>%
  mutate(elevation_bin = cut(dem_value, breaks = seq(min(dem_value), max(dem_value), by = 500),
                             labels = paste(seq(min(dem_value), max(dem_value), by = 500)[-1], "to", 
                                            seq(min(dem_value), max(dem_value), by = 500)[-length(seq(min(dem_value), max(dem_value), by = 500))] + 500),
                             include.lowest = TRUE))

# Step 3: Filter for corrected albedo (if necessary)
corrected_albedo_df <- summary_df %>%
  filter(type == "albedo_corrected")

# Step 4: Summarize by aspect_bin and elevation_bin, calculating the mean corrected albedo
summary_df_elevation_aspect <- corrected_albedo_df %>%
  group_by(aspect_bin, elevation_bin) %>%
  summarize(mean_albedo = mean(mean_albedo, na.rm = TRUE),  # Compute mean albedo for each group
            sd_albedo = sd(mean_albedo, na.rm = TRUE)) %>%   # Optional: compute standard deviation for albedo
  ungroup()  # Remove the grouping after summarization

# View the new summary_df with elevation bins and mean corrected albedo by aspect
head(summary_df_elevation_aspect)








library(dplyr)
library(ggplot2)

# Assuming 'summary_df' contains 'aspect_bin', 'mean_albedo', 'elevation', and 'type' (with corrected albedo)

# Step 1: Create elevation bins (500-meter intervals)
summary_df <- summary_df %>%
  mutate(elevation_bin = cut(elevation, breaks = seq(min(elevation), max(elevation), by = 500),
                             labels = paste(seq(min(elevation), max(elevation), by = 500)[-1], "to", 
                                            seq(min(elevation), max(elevation), by = 500)[-length(seq(min(elevation), max(elevation), by = 500))] + 500)))

# Step 2: Filter for corrected albedo (if necessary, assuming 'corrected' albedo is stored in the 'type' column)
corrected_albedo_df <- summary_df %>%
  filter(type == "albedo_corrected")

# Step 3: Create a polar plot with separate lines for each elevation bin
ggplot(corrected_albedo_df, aes(x = aspect_bin, y = mean_albedo, group = elevation_bin, color = elevation_bin)) +
  geom_line(size = 1) +  
  geom_point(size = 2, alpha = 0.6) +  
  scale_x_continuous(breaks = seq(0, 360, by = 45), limits = c(0, 360)) +  # Aspect bins
  coord_polar(start = pi / 2, direction = -1) +  # 0° at top, clockwise
  labs(
    title = "Corrected Albedo vs Aspect by Elevation Bins",
    x = "Aspect (degrees)",
    y = "Corrected Albedo",
    color = "Elevation Bin"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

