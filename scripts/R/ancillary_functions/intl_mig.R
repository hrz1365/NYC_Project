
# Derive domestic (rest of NYC) in and out migration to NYC burrows
derive_intl_mig <- function(cur_inputs_path, cur_year, pop_df,
                            int_mig, cur_scenario)
{

  # Scenario data (The Constant_rate scenario)
  scenario  <- file.path(cur_inputs_path, paste0(cur_scenario, ".csv")) %>%
    fread() %>%
    .[!is.na(year)]
  

  
  # International migration data (proportions per age group)
  intl_mig_df  <- file.path(cur_inputs_path, 'International_migration.csv') %>%
    fread() %>%
    .[, Age := as.numeric(str_sub(Age, 1, 3))] %>%
    .[, Race := ifelse(str_detect(Race, 'Asian'), 'Asian', Race)]
  
  
  # Annual total net international migration counts 
  # female net international migrants
  #Asian 
  asian_female_intl <- as.numeric(scenario[year == cur_year, "nim_AF"])
  # Black
  black_female_intl <- as.numeric(scenario[year == cur_year, "nim_BF"])
  #Hispanic
  hisp_female_intl  <- as.numeric(scenario[year == cur_year, "nim_HF"])
  # White
  white_female_intl <- as.numeric(scenario[year == cur_year, "nim_WF"])

  
  
  # male net international migrants
  #Asian
  asian_male_intl <- as.numeric(scenario[year == cur_year, "nim_AM"])
  # Black
  black_male_intl <- as.numeric(scenario[year == cur_year, "nim_BM"])
  #Hispanic
  hisp_male_intl  <- as.numeric(scenario[year == cur_year, "nim_HM"])
  # White
  white_male_intl <- as.numeric(scenario[year == cur_year, "nim_WM"])
  
  
  
  # Spread international migrant numbers according to profile
  if (int_mig == 1){
    # Female
    # Asian
    intl_mig_df[str_detect(Race, 'Asian') & Sex == 'Female',
                net_intl_mig := International.migration * asian_female_intl] 
    # Black
    intl_mig_df[str_detect(Race, 'Black') & Sex == 'Female',
                net_intl_mig := International.migration * black_female_intl]
    # Hispanic
    intl_mig_df[str_detect(Race, 'Hispanic') & Sex == 'Female',
                net_intl_mig := International.migration * hisp_female_intl]
    # White
    intl_mig_df[str_detect(Race, 'White') & Sex == 'Female',
                net_intl_mig := International.migration * white_female_intl]
    
    # Male
    # Asian
    intl_mig_df[str_detect(Race, 'Asian') & Sex == 'Male',
                net_intl_mig := International.migration * asian_male_intl] 
    # Black
    intl_mig_df[str_detect(Race, 'Black') & Sex == 'Male',
                net_intl_mig := International.migration * black_male_intl]
    # Hispanic
    intl_mig_df[str_detect(Race, 'Hispanic') & Sex == 'Male',
                net_intl_mig := International.migration * hisp_male_intl]
    # White
    intl_mig_df[str_detect(Race, 'White') & Sex == 'Male',
                net_intl_mig := International.migration * white_male_intl]
 
  } else {
    
    intl_mig_df[, net_intl_mig := 0] 
    
  }
  
  
  intl_mig_df[, Race := ifelse(str_detect(Race, 'Asian'), 'Asian', Race)]
  
  
  
  # Return a list of outputs
  return(intl_mig_df)
  
}
  