

estimate_mortality <- function(regions, inputs_path, cur_scenario, start_year, 
                                end_year, un_mortality_30_path, un_mortality_100_path)
{


  # UN mortality dfs
  un_mortality_30 <- un_mortality_30_path %>%
    fread() %>%
    melt(., id.vars = 'Age', variable.name = 'Sex', value.name = 'lx_un_30') %>%
    .[, Sex := ifelse(str_detect(Sex, '[F-f]emale'), 'Female', 'Male')] 

  
  un_mortality_100 <- un_mortality_100_path %>%
    fread() %>%
    melt(., id.vars = 'Age', variable.name = 'Sex', value.name = 'lx_un_100') %>%
    .[, Sex := ifelse(str_detect(Sex, '[F-f]emale'), 'Female', 'Male')] 

  un_mortality <- un_mortality_30[un_mortality_100, on = c('Age', 'Sex')] 

  
  total_mortality_df <- NULL
  
  
  for (region in regions){
    
    cat(paste("\nMortality calculations for", region))
    
    # Generate paths
    cur_inputs_path <- file.path(inputs_path, region)
    
    
    # Scenario data
    scenario_df <- file.path(cur_inputs_path, paste0(cur_scenario, ".csv")) %>%
      fread()
    
    
    # Mortality data for the current region
    mortality_df <- file.path(cur_inputs_path, 'mortality.csv') %>%
      fread() %>%
      .[, Age := as.numeric(str_sub(Age, 1, 3))] %>%
      .[, survival.ratio := ifelse(survival.ratio > 1, survival.ratio / 10, survival.ratio)]
    
    
    # Join to UN data
    mortality_df <- mortality_df[un_mortality, on = c('Age', 'Sex')]
    
    
    
    # Life expectancy at age zero (e0) for projection period according to the scenario
    e0_df <- scenario_df[year >= start_year, .(year, 
                                         Asian_Male      = as.numeric(m_AM),
                                         Asian_Female    = as.numeric(m_AF),
                                         Black_Male      = as.numeric(m_BM),
                                         Black_Female    = as.numeric(m_BF),
                                         Hispanic_Male   = as.numeric(m_HM),
                                         Hispanic_Female = as.numeric(m_HF),
                                         White_Male      = as.numeric(m_WM),
                                         White_Female    = as.numeric(m_WF))] 
    
    e0_df <- melt(e0_df, id.vars = 'year', variable.name = 'condition',
                  value.name = 'e0')
    e0_df[, c('Race', 'Sex') := tstrsplit(condition, '_')] 
    
    
    
    total_life_table  <- mortality_df[, .(Race, Sex, Age, lx = 0)]
    total_life_table[, Race := ifelse(str_detect(Race, 'Asian'), 'Asian', Race)]
    total_life_table  <- rbindlist(rep(list(total_life_table), 
                                       times = end_year - start_year + 1))
    num_ages          <- length(unique(total_life_table[,Age]))
    races             <- unique(total_life_table[, Race])
    genders           <- unique(total_life_table[, Sex]) 
    
    year_column <- rep(seq(start_year, end_year), 
                       each = length(races) * length(genders) * num_ages) 
    total_life_table[, Year := year_column]
    
    
    
    
    # Loop over years, races, and genders
    for(cur_year in start_year:end_year){
      
      for (race in races){
        
        for (gender in genders){
          
          # e0 based on the scenario
          e0_scenario <- e0_df[year == cur_year & condition == paste0(race, '_', gender), e0]
          
          
          # empirical e0 based on the life table 
          cur_mortality_df <- mortality_df[str_detect(Race, race) & Sex == gender] 
          e0_empirical     <- calculate_e0(cur_mortality_df, e0_scenario, 'lx', gender)
          
          
          
          # Choose correct UN lx variable for interpolation
          if(e0_scenario >= e0_empirical) {
            
            cur_un_mortality_100 <- un_mortality_100[Sex == gender]
            e0_UN <- calculate_e0(cur_un_mortality_100, 100, 'lx_un_100', gender)
            cur_mortality_df[, lx_un := lx_un_100]
            
            
          } else if(e0_scenario < e0_empirical){
            
            cur_un_mortality_30 <- un_mortality_30[Sex == gender]
            e0_UN <- calculate_e0(cur_un_mortality_30, 30, 'lx_un_30', gender)
            cur_mortality_df[, lx_un := lx_un_30]
            
          }
          
          
          # Update the empirical e0 based on UN values
          updated_lx <- update_lx(cur_mortality_df, 'lx_un', e0_empirical,
                                  e0_UN, e0_scenario)
          
          cur_mortality_df[, lx_upd := updated_lx]
          
          new_e0 <- calculate_e0(cur_mortality_df, e0_scenario, 'lx_upd', gender)
          
          
          # Compute difference between e0 (user specified) and e0 prime (from data)
          e0_diff <- abs(new_e0 - e0_scenario)  
          te    <- 0.01
          
          
          # Perform iterative adjustment
          if(e0_diff > te){
            
            count  <- 1   # Set counter
            e0_scenario_copy <- e0_scenario # generate a copy of the e0t value that will be used/modified in the adjustment process
            
            
            # Perform while loop
            while(e0_diff > te){
              
              # Specify the size of the increment by which e0tAdj is changed
              if(e0_diff >= 0.7){
                si <- 0.5
              }else if(e0_diff >= 0.2 & e0_diff < 0.7){
                si <- 0.1
              }else if(e0_diff >= 0.05 & e0_diff < 0.2){
                si <- 0.05
              }else if(e0_diff < 0.05){
                si <- 0.01
              }
              
              
              # Adjust scaler value
              if(new_e0 > e0_scenario){         # When e0tp (generated data) is larger than e0t (scenario)
                e0_scenario_copy <- e0_scenario_copy - si
              } else if(new_e0 < e0_scenario){   # When e0tp (generated data) is smaller than e0t (scenario)
                e0_scenario_copy <- e0_scenario_copy + si
              }
              
              
              # Update the empirical e0 based on UN values
              updated_lx <- update_lx(cur_mortality_df, 'lx_un', e0_empirical,
                                      e0_UN, e0_scenario_copy)
              
              cur_mortality_df[, lx_upd := updated_lx]
              
              new_e0 <- calculate_e0(cur_mortality_df, e0_scenario, 'lx_upd', gender)
              
              
              
              e0_diff <- abs(new_e0 - e0_scenario)  # Compute difference between e0 (user specified) and e0 prime (from data)
              
              count <- count+1
            }
          }
          
          total_life_table[Race == race & Sex == gender & Year == cur_year, lx:= updated_lx]
          
        }
      }
      
    }
    
    
    # Complete other elements of the life table
    # Percentage of survivals
    total_life_table[, ax := ifelse(Age == 0, 0.33, 0.5), by = .(Race, Sex, Year)]
    
    
    # Deaths per age group (dx)
    # Note: dx = lx0 - lx1;
    total_life_table[, dx := lx - shift(lx, type = 'lead'), by = .(Race, Sex, Year)]
    
    
    # Exposure to death per person year (Lx)
    total_life_table[, Lx := shift(lx, type = 'lead') + dx * ax, by = .(Race, Sex, Year)]
    
    
    total_life_table[, mx := dx / Lx, by = .(Race, Sex, Year)]
    
    
    total_life_table <- total_life_table[e0_df, on = c('Year' = 'year', 'Sex', 'Race')]
    
    
    total_life_table[Age == 100, mx := calculate_max_mx(Sex, e0), by = .(Race, Sex, Year)]
    
    
    # Compute Lx value for oldest age category (Lxmax)
    # Note: Lxmax=lxmax/mxmax
    total_life_table[Age == 100, Lx := lx / mx, by = .(Race, Sex, Year)]
    
    
    # Cumulative person years of survival
    # Note: Cumulative Lx from oldest to youngest age group
    total_life_table[, Tx := rev(cumsum(rev(Lx))), by = .(Race, Sex, Year)]
    
    
    # Generate proportion of individuals that survived to the next age group (Sx)
    # Note: Sx=Lx1/Lx0; Smax-1=S99=T100+/T99; Smax=0
    total_life_table[, Sx := shift(Lx, type = 'lead') / Lx, by = .(Race, Sex, Year)]
    total_life_table[Age >= 99, Sx := shift(Tx, type = 'lead') / Tx, by = .(Race, Sex, Year)]
    total_life_table[Age == 100, Sx := 0]
    
    
    # Compute life expectancy by age (ex)
    # Note: ex=Tx/lx
    total_life_table[, ex := Tx / lx, by = .(Race, Sex, Year)]
    
    
    total_life_table <- data.table(Region = region, total_life_table)
    
    
    total_mortality_df <- rbind(total_mortality_df, total_life_table)
  }
  
  
  return(total_mortality_df)
}




