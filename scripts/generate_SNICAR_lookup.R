#
# Generate Lookup table from SNICAR
# re-run online SNICAR v3 with varying grainsize
# 04/2025 - Olson
#

# Load required libraries
library(httr);library(rvest);library(dplyr)
library(readr);library(tidyr)

# generate parameters
html <- read_html("http://snow.engin.umich.edu")
html
# html |> html_elements("p")
html %>% html_nodes('b')
names <- html %>% html_nodes('input') %>% html_attr('name') %>% na.omit()
values <- html %>% html_nodes('input') %>% html_attr('value') %>% as.integer() %>% na.omit()

## Define parameters
# parameters <- setNames(values, names) # contains duplicates
# manage duplicates
input_data <- data.frame(name = names, value = values, stringsAsFactors = FALSE)

# Remove duplicates based on 'name' and keep the first occurrence
input_data_unique <- input_data[!duplicated(input_data$name), ]

# Create a named list from the unique data
parameters <- setNames(input_data_unique$value, input_data_unique$name)
parameters <- as.list(parameters)

# manually set options
parameters$in_atm = 1
parameters$in_direct_beam = 1
parameters$in_dust_type = 2
parameters$in_ice_ri = 3
parameters$in_sno_shp = 3

# Create function to generate lookup table
generate_lookup_table <- function(parameters, size) {
  #
  # Clean lookup table only
  #
  # Make Request
  url <- "http://snow.engin.umich.edu"
  url2 <- paste0(url, "/process.php")
  
  response <- POST(url2, body = parameters)
  
  # Extract Output
  soup <- read_html(content(response, "text"))
  results <- html_attr(html_nodes(soup, "a[href]"), "href")[1]
  output <- paste0(url, "/", results)
  
  # Read the data with readr
  df <- read_delim(output, delim = "\t", col_names = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  # Rename columns
  colnames(df) <- c("lambda", as.character(size))
  
  return(df)
}

## Generate lookup table for varying grain size
df_ls <- list()
# Create sequence from 30 to 1500 by 10 (subsample as in Python code)
grain_list <- seq(30, 1500, 10)

for (i in seq_along(grain_list)) {
  grain_size <- grain_list[i]
  print(paste0("Generating spectra for ", grain_size, " nm ", "(", i, " of ", length(grain_list), ")..."))
  parameters$in_rds_snw <- grain_size
  newdf <- generate_lookup_table(parameters, grain_size)
  
  if (grain_size == 30) {
    df_ls[[i]] <- newdf
  } else {
    df_ls[[i]] <- newdf[, 2, drop = FALSE]
  }
}

## Create final table
# First extract the lambda column from the first dataframe
lambda_col <- df_ls[[1]]$lambda

# Initialize result dataframe with lambda column
grain_tbl <- data.frame(lambda = lambda_col)

# Add all other columns
for (i in seq_along(grain_list)) {
  grain_size <- grain_list[i]
  if (grain_size == 30) {
    grain_tbl[as.character(grain_size)] <- df_ls[[i]][, 2]
  } else {
    grain_tbl[as.character(grain_size)] <- df_ls[[i]][, 1]
  }
}

# View the first few rows of the result
head(grain_tbl, 5)

## Plot
df_lookup_long = grain_tbl %>%
  pivot_longer(cols=!lambda,
               names_to = "grain_size",
               values_to = "reflectance") %>% 
  mutate(lambda = lambda*1000) # convert from microns to nm
head(df_lookup_long)

# plots
df_lookup_long %>% filter(grain_size==30 | grain_size==1500) %>% 
  ggplot(aes(x=lambda, y=reflectance, colour=grain_size)) + 
  geom_line(aes(group=1)) + theme_classic()

df_lookup_long %>% filter(grain_size %in% seq(30,1500,30)) %>% 
  mutate(grain_size=as.factor(grain_size)) %>% 
  ggplot(aes(x=lambda, y=reflectance, group=grain_size)) + 
  geom_line(aes(colour=grain_size), show.legend = FALSE) + xlim(c(305,2200)) +
  theme_classic()
