
# Header ------------------------------------------------------------------

# Purpose: Identify grid cells that fall within the region boundary
# Author : Hamidreza Zoraghein
# Date   : 1/5/2024




# Packages ----------------------------------------------------------------

if (!requireNamespace('tidyverse', quietly = T))  install.packages('tidyverse')
if (!requireNamespace('terra', quietly = T))      install.packages('terra')


library(tidyverse)
library(terra)




# Inputs and Paths --------------------------------------------------------

borough          <- 'Queens' 
ini_inputs_path  <- file.path('inputs', 'initial_inputs') 
main_inputs_path <- file.path('inputs', 'main_downscaling_inputs', 
                              'calibration', borough)




# Main Program ------------------------------------------------------------

city_boundary <- file.path(ini_inputs_path, 'city_boundary_boroughs.gpkg') %>%
  vect() %>%
  subset(., .$boro_name == borough)


pop_grid <- file.path(main_inputs_path, 'pop_grid_2020.tif') %>%
  rast() 


values(pop_grid) <- 0:(ncell(pop_grid) - 1)


pop_points_within <- pop_grid %>%
  mask(city_boundary) %>%
  as.points() %>%
  values()


colnames(pop_points_within) <- 'index'
saveRDS(pop_points_within, file.path(main_inputs_path, 'within_indices.rds'))





pop_points <- pop_grid %>%
  as.points()


pop_coordinates <- geom(pop_points)

pop_coordinates <- pop_coordinates %>%
  as.data.frame() %>%
  mutate(point_id = 0:(nrow(pop_coordinates)-1)) %>%
  relocate(point_id) %>%
  dplyr::select(point_id, x, y)


pop_coordinates <- pop_coordinates %>%
  write_csv(file.path(main_inputs_path, 'coors.csv'))
