estimate_fertility <- function(regions, inputs_path, cur_scenario, start_year, 
                                end_year)
{

  total_fertility_df  <- NULL
  
  
  # Fertility estimates per regions
  for (region in regions){
    
    cat(paste("\nFertility for", region))    
    
    region_fertility_df <- NULL

    
    # Generate paths
    cur_inputs_path <- file.path(inputs_path, region)  # Input data directory



    # Scenario data
    scenario_df <- file.path(cur_inputs_path, paste0(cur_scenario, ".csv")) %>%
      fread()



    # Fertility data in the base year
    fertility_df <- file.path(cur_inputs_path, 'fertility.csv') %>%
      fread() %>%
      .[, Age := as.numeric(str_sub(Age, 1, 3))] %>%
      .[, Race := ifelse(str_detect(Race, 'Asian'), 'Asian', Race)]
      

    
    # Total fertility for projection period according to the scenario
    scenario_tfr_df <- scenario_df[year >= start_year, .(year, 
                                                   Asian    = as.numeric(f_A),
                                                   Black    = as.numeric(f_B),
                                                   Hispanic = as.numeric(f_H),
                                                   White    = as.numeric(f_W))]
    setnames(scenario_tfr_df, 'year', 'Year')
    
    scenario_tfr_df <- melt(scenario_tfr_df, id.vars = 'Year', variable.name = 'Race',
                            value.name = 'tfr')
    
    races <- as.character(unique(fertility_df[, Race]))
    
    

    # Loop over races and years for which fertility data is needed
    for(race in races){
      
      # Subset based on the current race
      cur_fertility_df <- fertility_df[str_detect(Race, race)]
        
      
      # Loop over region types (rural/urban)
      for(year in start_year:end_year){
        
        # Compute tfr based on empirical data
        empirical_tfr <- cur_fertility_df[, .(sum_tfr = sum(Fertility))][,sum_tfr]
        
        
        # TFR proportion for each each group
        cur_fertility_df[, tfr_proportion := Fertility / empirical_tfr]
        
        
        # TFR cumulative sum
        cur_fertility_df[, tfr_cum_sum := cumsum(tfr_proportion)]
        
        
        # TFR cumulative proportion
        cur_fertility_df[, tfr_cum_prop := tfr_cum_sum * empirical_tfr]
        
        
        # Compute empirical IQR (Inter Quartile Range)
        emp_quartile_range <- calculate_target_value(cur_fertility_df, "tfr_cum_sum", "Age", 0.75) - 
          calculate_target_value(cur_fertility_df, "tfr_cum_sum", "Age", 0.25)
        
        
        # Compute the empirical age with median cumulative fertility 
        emp_median_age <- calculate_target_value(cur_fertility_df, "tfr_cum_sum", "Age", 0.5) 
        
        
        # Extract scenario TFR
        scenario_tfr  <- scenario_tfr_df[Year == year & Race == race][,tfr]   # future year


        # Compute IQR of Future year (t)
        # Note: Formula is based on empirically derived relationship between TFR and IQR
        scenario_iqr <- 6.1 + 0.466*scenario_tfr + 0.136*scenario_tfr^2


        # Compute beta value
        beta <- emp_quartile_range / scenario_iqr


        # Compute Median Age of Future year (t)
        # Formula is based on empirically derived relationship between TFR and Median Age
        if(scenario_tfr <= 4.5){
          
          scenario_median_age <- 30.8 - 4.64*scenario_tfr + 1.05*scenario_tfr^2

        } else {
          scenario_median_age <- 31.18 # when tfr of future year is larger than 4.5 we fix the median age at 31.18 years
        }


        # Derive target cumulative proportion based on the scenario median age
        scen_cum_prop_median <- calculate_target_value(cur_fertility_df, 'Age', 'tfr_cum_prop',
                                                    scenario_median_age)



        # Compute alpha value
        # The smaller the alpha the later the fertility process; normal alpha rage -0.5 to 0.5 (Zeng et al., 2000)
        alpha <- -log(-log(0.5)) - beta*(-log(-log(scen_cum_prop_median / empirical_tfr)))



        # Compute YHxT of Future
        # YHxT is computed by using alpha and beta values together with the Standard (see Equation 4 in Zeng et al. (2000))
        cur_fertility_df[, str_c('yHxT', year) := alpha + beta*(-log(-log(round(tfr_cum_prop/empirical_tfr, 7))))]

        
        
        # Compute cumsum for Future
        cur_fertility_df[, str_c('tfr_cum_sum_', year) := exp(-exp(-get(str_c('yHxT', year))))]
        
        

        # Compute cumprop for Future
        cur_fertility_df[, str_c('tfr_cum_prop_', year) := scenario_tfr * get(str_c('tfr_cum_sum_',
                                                                                    year))]

        
        
        # Compute fertility rates for Future (fx=H(x)-H(x-1))
        cur_fertility_df[, str_c('rates_', year) := get(str_c('tfr_cum_prop_', year)) - 
                           shift(get(str_c('tfr_cum_prop_', year)), type = 'lag')]
        cur_fertility_df[, str_c('rates_', year) := ifelse(is.na(get(str_c('rates_', year))),
                                                       0, get(str_c('rates_', year)))]


        # Apply middle year value
        # This step is necessary only if the underlying fertility data comes from 5-year age categories
        # tests similarity between age 21 and 22 of standard
        is_5_year <- cur_fertility_df[Age == 21, tfr_proportion] == cur_fertility_df[Age == 22, tfr_proportion]


        if(is_5_year){

          # Add variable that reflects middle age for each five-year group

          # sequence of middle ages
          middle_ages <- seq(2, max(cur_fertility_df[, Age], na.rm = T), 5)

          # sequence of middle age years
          middle_ages <- rep(middle_ages, each = 5)
          

          cur_fertility_df[, mid_ages :=  c(middle_ages, 100)]

          
          
          # Obtain middle age fertility value
          middle_age_df <- cur_fertility_df[Age %in% unique(mid_ages), .SD, 
                                            .SDcols = c('Age', str_c('rates_', year))]

    
          cur_fertility_df[, str_c('rates_', year) := NULL]
          cur_fertility_df <- cur_fertility_df[middle_age_df, on = c('mid_ages' = 'Age')]
          


          # Adjust estimates to meet target tfr
          # Note: this step is needed because the middle age assignment distorts the tfr value
          new_tfr   <- cur_fertility_df[, sum(get(str_c('rates_', year)))]                
          tfr_scale <- scenario_tfr / new_tfr                                      
          cur_fertility_df[, str_c('rates_', year) := get(str_c('rates_', year)) * tfr_scale] 
        }
      }
      
      cur_fertility_df    <- cur_fertility_df[, .SD, .SDcols = c('Race', 'Age', str_subset(colnames(cur_fertility_df),'^rates'))]
      region_fertility_df <- rbind(region_fertility_df, cur_fertility_df)
    }
    
    region_fertility_df <- data.table(Region = region, region_fertility_df)
    
    
    total_fertility_df <- rbind(total_fertility_df, region_fertility_df)
  }

  return(total_fertility_df)
}
