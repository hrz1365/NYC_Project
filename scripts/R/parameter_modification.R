
# Header ------------------------------------------------------------------

# Purpose: Change alpha and beta patameters according to the SSP scenario
# Author : Hamidreza Zoraghein
# Date   : 3/31/2024




# Packages ----------------------------------------------------------------

if (!requireNamespace('data.table', quietly = T))  install.packages('data.table')


library(data.table)




# Functions ---------------------------------------------------------------

derive_slope <- function(x1, y1, x2, y2){
  
  slope <- (y2 - y1) / (x2 - x1)
  
  return(slope)
}



linear_interpolate <- function(x, x1, y1, slope){
  
  y <- slope * (x - x1) + y1
  
  return(y)
}




# Inputs and Paths --------------------------------------------------------

borough          <- 'Staten_Island' 
cur_scenario     <- 'SSP1' 
target_alpha     <- 2
target_beta      <- 2 
workspace_path   <- file.path('inputs', 'main_downscaling_inputs', 
                              'projection', borough)




# Main Program ------------------------------------------------------------

base_parameters <- file.path(workspace_path, 'parameters_SSP2.csv') %>%
  fread()

alpha_slope <- derive_slope(base_parameters[1, year], base_parameters[1, alpha],
                            base_parameters[.N, year], target_alpha)
beta_slope  <- derive_slope(base_parameters[1, year], base_parameters[1, beta],
                            base_parameters[.N, year], target_beta)


base_parameters[, alpha := linear_interpolate(year, 
                                              base_parameters[1, year],
                                              base_parameters[1, alpha],
                                              alpha_slope)]
base_parameters[, beta := linear_interpolate(year, 
                                             base_parameters[1, year],
                                             base_parameters[1, beta],
                                             beta_slope)]

base_parameters[, `:=`(scenario = cur_scenario)]


base_parameters %>%
  fwrite(file.path(workspace_path, paste0('parameters_', cur_scenario,
                                          '.csv')))
