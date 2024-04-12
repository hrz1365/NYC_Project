
# Header ------------------------------------------------------------------

# Purpose: Create the main inputs for the main population downscaling model
# Author : Hamidreza Zoraghein
# Date   : 6/29/2023




# Packages ----------------------------------------------------------------

if (!requireNamespace('terra'))  install.packages('terra')

library(terra)




# Inputs and Paths --------------------------------------------------------

cur_year                <- 2020 
initial_inputs_path     <- file.path('inputs', 'initial_inputs') 
parcel_downscaling_path <- file.path('outputs', 'parcel_downscaling')
main_downscaling_path   <- file.path('inputs', 'main_downscaling_inputs')




# Main Program ------------------------------------------------------------

# Read the parcels vector file
parcel_downscaled_path  <- file.path(parcel_downscaling_path, paste0('parcels_pop_', cur_year, '.shp'))
pop_vector              <- vect(parcel_downscaled_path)
pop_vector[['density']] <- pop_vector[['totpop_e']] / pop_vector[['area']] 


# Read the mask raster to use as a reference raster
mask_raster <- rast(file.path(main_downscaling_path, 'mask_raster.tif'))


# Rasterize parcels
pop_raster <- rasterize(pop_vector, mask_raster, field = 'density', fun = 'sum')
pop_raster <- pop_raster * 1e4

writeRaster(pop_raster, file.path(initial_inputs_path, paste0('nyc_pop_', cur_year, '.tif')),
            overwrite = T)
