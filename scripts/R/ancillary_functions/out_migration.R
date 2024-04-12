
derive_int_out_migration <- function(inputs_path, cur_year, pop_df, cur_scenario){
  
  # A data frame to hold internal in-migration values for region pairs
  pairs_out_int_mig_df <- data.table()
  
  
  # A data frame to hold internal in-migration values across all regions
  tot_out_int_mig_df <- data.table()
  
  
  # Retrieve regions
  regions <- unique(pop_df[, Region])
  
  
  for (region in regions){
    
    # Read the csv file holding in migration rates to the current state
    cur_int_mig_rates_df <- file.path(inputs_path, region, 'internal_out_migration.csv') %>%
      fread() %>%
      .[, Internal.migration.rate := ifelse(is.na(Internal.migration.rate), 0,
                                            Internal.migration.rate)] %>%
      .[, Age := as.numeric(str_sub(Age, 1, 3))] %>%
      .[, County := str_replace_all(County, ' ', '_')] %>%
      .[, Race := ifelse(str_detect(Race, 'Asian'), 'Asian', Race)]
    
    
    # Make sure the current region is removed in the internal migration numbers 
    cur_int_mig_rates_df <- cur_int_mig_rates_df[County != region, ]
    
    
    
    # Retrieve the internal migration factor consistent with the scenario
    int_mig_scen_factor <- file.path(inputs_path, region, paste0(cur_scenario, ".csv")) %>%
      fread() %>%
      .[year == cur_year, int_mig] %>%
      as.numeric()
    
    
    
    # Retrieve population of origin regions
    cur_origin_pop_df <- pop_df[Region == region,]
    
    
    
    # Apply the scenario factor to the migration table
    cur_int_mig_rates_df[, Internal.migration.rate := Internal.migration.rate *
                           int_mig_scen_factor]
    
    
    
    # Multiplication gives us the in migration numbers from each contributing state
    cur_int_mig_values_df <- cur_int_mig_rates_df[cur_origin_pop_df, 
                                                  .(Origin = region, County, Race, Sex,
                                                  Age, Population, Internal.migration.rate,
                                                  int_out_mig = Population * Internal.migration.rate),
                                                  on = c('Race', 'Sex', 'Age')]
    
    
    # Sum the total in migration numbers across all contributing regions
    cur_tot_int_mig_values_df <- cur_int_mig_values_df[, .(int_out_mig = sum(int_out_mig)),
                                                       by = .(Origin, Race, Sex, Age)]
    
    
    pairs_out_int_mig_df <- rbind(pairs_out_int_mig_df, cur_int_mig_values_df) 
    tot_out_int_mig_df   <- rbind(tot_out_int_mig_df, cur_tot_int_mig_values_df)
  }
  
  
  output_list <- list(region_pair_outmigration = pairs_out_int_mig_df,
                      total_outmigration       = tot_out_int_mig_df)
  
  return(output_list)
}
