#
# Kaya's code with edits
# Albedo - RF
# 


library(tidyr)
library(terra)
library(dplyr)
library(ggplot2)

# define path
pth = "../snowspectraltools/data/dust/landsat_wasatch"
list.files(pth)

# read in May 1
lspath <- list.files(pth,
                     pattern = ".*04.*B[1-7].TIF", full.names = T)
lspath
may_1 <- rast(lspath)

# create an extent to crop
ext <- ext(-111.85, -111.550, 40.37, 40.60)
extent <- vect(ext, crs = "EPSG:4326")
crop <- project(extent, crs(may_1))  # project to WGS 84 UTM Zone 12

# crop the rasters
may <- crop(may_1, crop)

plot(may[[1]])

MULT <- 2.75e-05
ADD <- -0.2

may_1_sr <- (may * MULT) + ADD


# let's work on 2024


# get working space ready
# setwd("C:/Users/kayaf/OneDrive/GEOG_3440/PROJECT")
# lspath <- list.files("C:/Users/kayaf/OneDrive/GEOG_3440/PROJECT/landsat_wasatch_ext/landsat_wasatch_ext/",
#                      pattern = "B[1-7].TIF", full.names = T)
# lspath
# # filter through the bands to split by month
# 
# may <- rast(lspath[22:28])



# create an extent to crop
ext <- ext(-111.75, -111.551, 40.34, 40.45)
extent <- vect(ext, crs = "EPSG:4326")
crop <- project(extent, crs(may))  # project to WGS 84 UTM Zone 12

# crop the rasters
may <- crop(may, crop)
plotRGB(may, 4,3,2, stretch = "linear")

# MULT and ADD factors to convert to surface reflectance
meta_path <- list.files(pth,pattern = "MTL.txt", full.names = TRUE)
meta_path <- meta_path[1]
meta <- readLines(meta_path)

meta_spec <- grep("LEVEL2_SURFACE_REFLECTANCE_PARAMETERS", meta)
meta_final <- meta[meta_spec[1]:meta_spec[2]]

grep("REF.*MULT", meta_final, value=T)[1]  # mult value (scale)
grep("REF.*ADD", meta_final, value=T)[1]   # add value (offset)
MULT <- 2.75e-05
ADD <- -0.2

# apply to scene
may_sr <- (may * MULT) + ADD

# create the NDSI function 
NDSI <- function(image){
  ndsi_no_thresh <- (image[[3]] - image[[6]]) / (image[[3]] + image[[6]])
  print("Calculating NDSI...")
  ndsi <- ndsi_no_thresh > 0.4
  print("NDSI Complete !")
  return(ndsi)
}

ndsi <- NDSI(may_sr)
plot(ndsi)
may_mask <- mask(may_sr, ndsi, maskvalue = FALSE)

may_albedo <- ((1.2242 * may_mask[[2]]) + (-0.4318 * may_mask[[3]]) + (-0.3446 * may_mask[[4]]) + (0.3367 * may_mask[[5]]) + (0.1834 * may_mask[[6]]) + (0.2555 * may_mask[[7]])) - 0.0052
par(mfrow = c(1,1))
plot(may_albedo, main = "04/12/2024 Albedo")
hist(may_albedo)

# bring in dem
dem <- rast("~/src/gds-R/data_tmp/wasatch_dem/ASTGTM2_N40W112_dem.tif")
dem
dem <- project(dem, crs(may_albedo))
dem <- crop(dem, crop)
timp <- dem * 3.281


aspect <- terrain(timp, v = "aspect", unit = "degrees", neighbors = 8)
aspect_fr <- resample(aspect, may_albedo)

# Reclassify aspect into 8 categories
reclass_matrix <- matrix(c(
  0, 22.5, 1,
  22.5, 67.5, 2,
  67.5, 112.5, 3,
  112.5, 157.5, 4,
  157.5, 202.5, 5,
  202.5, 247.5, 6,
  247.5, 292.5, 7,
  292.5, 337.5, 8,
  337.5, 360, 1  # wraparound to north
), ncol = 3, byrow = TRUE)

aspect_classes <- classify(aspect_fr, rcl = reclass_matrix)
stacked <- c(may_albedo, aspect_classes)
names(stacked) <- c("albedo", "aspect_class")



aspect_df <- as.data.frame(stacked, xy = FALSE, na.rm = TRUE)

aspect_labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
aspect_df
df_summary <- aspect_df %>%
  mutate(aspect_label = factor(aspect_labels[aspect_class], levels = aspect_labels)) %>%
  group_by(aspect_label) %>%
  summarize(mean_albedo = mean(albedo, na.rm = TRUE))


ggplot(df_summary, aes(x = aspect_label, y = mean_albedo, fill = mean_albedo)) +
  geom_col(width = 1, color = "white") +
  coord_polar(start = -pi/8) +
  scale_fill_gradientn(
    colours = c("#313695", "#74add1", "#ffffbf", "#f46d43", "#a50026"),
    limits = c(0, 1),
    name = "Albedo"
  ) +
  geom_text(aes(label = round(mean_albedo, 2), y = mean_albedo + 0.05),
            size = 4, color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_line(color = "gray90"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12),
    legend.position = "right"
  ) +
  ggtitle("Mean Snow Albedo by Aspect (May 2024)") +
  ylim(0, 0.8)  


# let's work on elevation

elev_breaks <- c(6200, 6800, 7400, 8000, 8600)
elev_labels <- c("Low", "Mid", "High", "Very High")

# Reclassify elevation raster into bands
elev_class <- classify(timp, 
                       rcl = matrix(c(
                         elev_breaks[1], elev_breaks[2], 1,
                         elev_breaks[2], elev_breaks[3], 2,
                         elev_breaks[3], elev_breaks[4], 3,
                         elev_breaks[4], elev_breaks[5], 4
                       ), ncol = 3, byrow = TRUE))

# Stack elevation class with albedo raster
elev_class <- resample(elev_class, may_albedo)
elev_stack <- c(may_albedo, elev_class)
names(elev_stack) <- c("albedo", "elev_class")

df <- as.data.frame(elev_stack, xy = FALSE, na.rm = TRUE) %>%
  filter(!is.na(albedo), !is.na(elev_class))

# Bin elevation into 100 ft bands
df <- df %>%
  mutate(elev_band = cut(elev_class, breaks = seq(6200, 11000, by = 200)))

# Calculate mean albedo per band
elev_summary <- df %>%
  group_by(elev_band) %>%
  summarise(
    mean_albedo = mean(albedo),
    elev_mid = mean(elev_class)
  )

samples <- data.frame(
  elevation = c(7852, 6333, 7225, 8184),
  label = c("SN", "LP", "MM", "SS")
)

ggplot(elev_summary, aes(x = elev_mid, y = mean_albedo)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 2) +
  
  # vertical dotted lines from each sample point
  geom_segment(data = samples,
               aes(x = elevation, xend = elevation, y = 0, yend = 0.35),
               color = "red", linetype = "dotted", linewidth = 0.7) +
  
  # Sample points and labels
  geom_point(data = samples, aes(x = elevation, y = 0), color = "red", size = 2) +
  geom_text(data = samples, aes(x = elevation, y = 0.01, label = label),
            color = "red", size = 3) +
  
  labs(title = "Albedo vs Elevation",
       x = "Elevation (ft)",
       y = "Mean Albedo") +
  theme_minimal()


