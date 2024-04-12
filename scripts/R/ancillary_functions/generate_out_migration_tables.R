
generate_int_out_migration <- function(inputs_path, regions){
  
  # A data frame to hold internal in-migration values for region pairs

  
  for (region in regions){
    
    dest_regions <- regions[regions != region]
    
    out_migration_df <- data.table()
    
    for (dest_region in dest_regions){
      
      # Read internal migration files for destination regions
      cur_int_mig_rates_df <- file.path(inputs_path, dest_region,
                                        'internal_migration.csv') %>%
        fread() %>%
        .[, Internal.migration.rate := ifelse(is.na(Internal.migration.rate), 0,
                                              Internal.migration.rate)] %>%
        .[, Age := as.numeric(str_sub(Age, 1, 3))] %>%
        .[, Origin := str_replace_all(Origin, ' ', '_')] %>%
        .[Origin == region, .(Origin, Destination = dest_region, Race, Sex, Age,
                              Internal.migration.rate)]
      
      
      out_migration_df <- rbind(out_migration_df, cur_int_mig_rates_df)
    }
    
    fwrite(out_migration_df, file.path(inputs_path, region,
                                       'internal_out_migration.csv'))
  }

}
