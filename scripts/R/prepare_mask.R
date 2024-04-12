
# Header ------------------------------------------------------------------

# Purpose: Generate the mask layer for NYC
# Author : Hamidreza Zoraghein
# Date   : 12/21/2023




# Packages ----------------------------------------------------------------

if (!requireNamespace('tidyverse', quietly = T))   install.packages('tidyverse')
if (!requireNamespace('data.table', quietly = T))  install.packages('data.table')
if (!requireNamespace('terra', quietly = T))       install.packages('terra')


library(tidyverse)
library(data.table)
library(terra)




# Inputs and Paths --------------------------------------------------------

mask_inputs_path   <- file.path('inputs', 'mask')
inputs_path        <- file.path('inputs', 'initial_inputs') 
downscaling_inputs <- file.path('inputs', 'main_downscaling_inputs')
buffer_dis         <- 45e3




# Main Program ------------------------------------------------------------

city_boundary <- file.path(inputs_path, 'city_boundary.shp') %>%
  vect()


# Read the vectorized mask layer
base_mask_vect <- file.path(mask_inputs_path, 'base_mask.gpkg') %>%
  vect() 



# Create the template for the mask raster
base_mask_raster_temp      <- rast(ext(base_mask_vect), resolution = c(5, 5))
crs(base_mask_raster_temp) <- crs(base_mask_vect)



# Rasterize the vector mask based on the template
base_mask_raster <- rasterize(base_mask_vect, base_mask_raster_temp, background = 0)
base_mask_raster <- 1 - base_mask_raster
base_mask_raster <- base_mask_raster %>%
  mask(city_boundary)



# Create the final mask raster at 100m resolution
final_mask_raster <- base_mask_raster %>%
  aggregate(fact = 20, na.rm = T, fun = 'mean') 

  
final_mask_raster[is.na(final_mask_raster)] <- 0


# Save the final mask raster
final_mask_raster <- final_mask_raster %>%
  mask(city_boundary)


writeRaster(final_mask_raster, file.path(downscaling_inputs, 'mask_raster.tif'),
            overwrite = TRUE)








