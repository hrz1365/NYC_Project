
# Header ------------------------------------------------------------------

# Author:     Hamidreza Zoraghein
# Date:       2024-02-25
# Purpose:    Multi-state population projection for NYC.



# Packages and Options ----------------------------------------------------

if (!requireNamespace('tidyverse', quietly = T))  install.packages('tidyverse')
if (!requireNamespace('sf', quietly = T))         install.packages('sf')
if (!requireNamespace('raster', quietly = T))     install.packages('raster')
if (!requireNamespace('terra', quietly = T))      install.packages('terra')
if (!requireNamespace('data.table', quietly = T)) install.packages('data.table')


library(tidyverse)
library(sf)
library(raster)
library(terra)
library(data.table)

raster::rasterOptions(maxmemory = 8e10, memfrac = 0.8, tmptime = 1)
raster::removeTmpFiles()



# Inputs and Paths --------------------------------------------------------

projection_years    <- seq(2030, 2100, 10) 
chars               <- c('Asian' = 'PASO',  'Hispanic' = 'PHIS',
                         'Black' = 'PNHB',  'White' = 'PNHW')
scenario            <- 'SSP1' 
outputs_dir         <- file.path('outputs', 'decomposition')
chars_file          <- file.path(outputs_dir, str_c('race_', scenario, '.gpkg'))
char_output_dir     <- file.path(outputs_dir, 'race', scenario) 
pop_downscaling_dir <- file.path('outputs', 'population_downscaling', 
                                 'projection')  
boundary_file <- file.path('inputs', 'initial_inputs', 
                           'city_boundary_boroughs.gpkg')
pop_projection_dir <- file.path('outputs', 'pop_projection', scenario,
                                'population_projections.csv')


if (!dir.exists(char_output_dir)) {
  dir.create(char_output_dir)
} else {
  print('The folder already exists')
}



# Functions ---------------------------------------------------------------

read_dem_sf <- function(dem_file, year){
  
  chars_sf <- dem_file %>%
    read_sf() %>%
    dplyr::select(ends_with(as.character(year))) %>%
    mutate(row_sums = rowSums(.[, 1:4, drop=TRUE])) %>%
    mutate(across(starts_with('P'), ~ .x / row_sums)) 
  
  return(chars_sf)
}


create_dem_raster <- function(dem_file, characteristic, year, scenario,
                              pop_dir){
  
  characteristic <- str_c(characteristic, as.character(year))
    
  # Select the column corresponding to the characteristics of interest
  cur_chars_sf <- dem_file %>%
    dplyr::select(all_of(characteristic)) 
  
  
  # Read the corresponding population raster
  pop_dir    <- file.path(pop_dir, scenario) 
  
  boroughs   <- list.dirs(pop_dir, full.names = F, recursive = F) 
  
  pop_rastres_list <- vector(mode = 'list', length = length(boroughs))
  names(pop_rastres_list) <- boroughs
  
  
  for (borough in boroughs) {
    
    pop_raster <- file.path(pop_dir, borough, str_c('pop_grid_', scenario, 
                                                    '_', year, '.tif')) %>%
      raster()
    
    pop_rastres_list[[borough]] <- pop_raster
    
  }

  pop_raster <- terra::mosaic(pop_rastres_list[[1]], pop_rastres_list[[2]], 
                              pop_rastres_list[[3]], pop_rastres_list[[4]],
                              pop_rastres_list[[5]],
                              fun = 'sum', na.rm = T)
  
  
  # Rasterize the spatial points file according to the population raster
  dem_raster <- raster::rasterize(cur_chars_sf, pop_raster)[[2]]
  
  
  # Create the final demographic population raster
  dem_pop_raster <- dem_raster * pop_raster
  
  
  return(terra::rast(dem_pop_raster))
}



# Main Program ------------------------------------------------------------

for (year in projection_years){
  
  print(str_c('The current year is: ', year))
  chars_sf <- read_dem_sf(chars_file, year)
  
  for (char in chars){
    
    print(str_c('The current characteristic is: ', char))
    
    initial_dem_raster <- create_dem_raster(chars_sf, char, year, scenario,
                                            pop_downscaling_dir)
    
    raster_path <- file.path(char_output_dir, str_c(char, '_', year, '_', 
                                                    scenario, '.tif'))
    raster::writeRaster(initial_dem_raster, raster_path, overwrite = T)
  }
  
}


# Rescale the initial rasters to match aggregate values
list_initial_rasters <- list.files(char_output_dir, full.names = T)

population_bor_df <- pop_projection_dir %>%
  fread()

boroughs <- unique(population_bor_df$Region)

nyc_boundary <- vect(boundary_file)


for (cur_file in list_initial_rasters){
  
  char <- str_split_1(basename(cur_file), '_')[1]
  year <- as.numeric(str_split_1(basename(cur_file), '_')[2])
  
  boroughs_raster_list <- vector(mode = 'list', length = length(boroughs))
  names(boroughs_raster_list) <- boroughs
  
  for (borough in boroughs) {
    
    target_pop_df <- population_bor_df[Year == year & Region == borough &
                                         Race == names(chars)[chars == char],
                                       .(Population = sum(Population)), 
                                       by = c('Year', 'Region', 'Race')]
    
    if (borough == 'Staten_Island') borough = 'Staten Island'
    
    cur_boundary  <- terra::subset(nyc_boundary, 
                                   nyc_boundary$boro_name == borough)
    
    borough_raster <- cur_file %>%
      rast() %>%
      terra::crop(cur_boundary) %>%
      terra::mask(cur_boundary)
    
    raster_pop_value <- sum(values(borough_raster), na.rm = T)
    
    scale <- target_pop_df[['Population']] / raster_pop_value
    
    borough_raster <- borough_raster * scale
    
    if (borough == 'Staten Island') borough = 'Staten_Island'
    
    boroughs_raster_list[[borough]] <- borough_raster
  }
  
  
  pop_raster <- terra::mosaic(boroughs_raster_list[[1]], 
                              boroughs_raster_list[[2]], 
                              boroughs_raster_list[[3]], 
                              boroughs_raster_list[[4]],
                              boroughs_raster_list[[5]], fun = 'sum')
  
  raster_path <- file.path(char_output_dir, str_c(char, '_', year, '_', 
                                                  scenario, '.tif'))
  raster::writeRaster(pop_raster, raster_path, overwrite = T)
}
