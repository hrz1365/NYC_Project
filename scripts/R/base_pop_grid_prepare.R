
# Header ------------------------------------------------------------------

# Purpose: Generate historical population rasters for the main population downscaling model
# Author : Hamidreza Zoraghein
# Date   : 5/14/2023




# Packages ----------------------------------------------------------------

if (!requireNamespace('tidyverse', quietly = T))  install.packages('tidyverse')
if (!requireNamespace('raster', quietly = T))     install.packages('raster')
if (!requireNamespace('terra', quietly = T))      install.packages('terra')


library(tidyverse)
library(raster)
library(terra)




# Inputs and Paths --------------------------------------------------------

ini_inputs_path <- file.path('inputs', 'initial_inputs') 
main_inputs     <- file.path('inputs', 'main_downscaling_inputs')
parcels_inputs  <- file.path('outputs', 'parcel_downscaling')
cur_year        <- 2000 
buffer_dis      <- 45e3




# Main Program ------------------------------------------------------------

city_boundary <- file.path(ini_inputs_path, 'city_boundary.shp') %>%
  vect()


city_boundary_buffer <- city_boundary %>%
  buffer(buffer_dis)

# city_boundary_buffer <- buffer(city_boundary, buffer_dis)


# The GHS-Pop raster to fill boundary cells
# cur_init_raster <- file.path(ini_inputs_path, 
#                              str_c('GHSL_Pop_', cur_year, '.tif')) %>%
#   rast() %>%
#   project('epsg:5070') %>%
#   crop(city_boundary_buffer) 



# Read the mask raster for rasterizing the parcels 
mask_raster <- file.path(main_inputs, 'mask_raster.tif') %>%
  rast()



# Read vectorized parcels
parcels_vect <- file.path(parcels_inputs, str_c('parcels_pop_', cur_year, 
                                                '.shp')) %>%
  vect() 

area_column <- expanse(parcels_vect, unit = 'm')

parcels_vect[['pop_density']] <- parcels_vect[['totpop_e']] / area_column



# Rasterize parcels based on population density
cur_orig_raster <- parcels_vect %>%
  rasterize(mask_raster, field = 'pop_density', background = 0) %>%
  mask(city_boundary)

cur_orig_raster <- cur_orig_raster * 10000



# Align the parcel-based population grid with the GHSL-based grid
cur_orig_raster <- cur_orig_raster %>%
  extend(city_boundary_buffer, fill = NA)



# Replace common values in the GHSL-based grid with the parcel-based grid
# cur_init_raster[cur_orig_raster >= 0 & !is.na(cur_orig_raster)] <-
#   cur_orig_raster[cur_orig_raster >= 0 & !is.na(cur_orig_raster)]



# Replace negative values of the parcel-based grid (outside the boundary) with 0
# cur_init_raster[cur_orig_raster < 0] <- 0



# Write the final raster as one of the main inputs to downscaling
cur_orig_raster %>%
  writeRaster(file.path(main_inputs, str_c('pop_grid_', cur_year, '.tif')), overwrite = T)



# Align the mask raster with the resulting population grid
mask_raster <- mask_raster %>%
  extend(cur_orig_raster, fill = NA) %>%
  resample(cur_orig_raster)


writeRaster(mask_raster, file.path(main_inputs, 'mask_raster.tif'),
            overwrite = TRUE)
