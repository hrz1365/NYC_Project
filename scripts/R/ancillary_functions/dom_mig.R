
# Derive domestic (rest of NYC) in and out migration to NYC burrows 
derive_dom_mig <- function(cur_inputs_path, cur_year, pop_df,
                           dom_mig, cur_scenario)
{
  
  # Scenario data frame
  scenario  <- file.path(cur_inputs_path, paste0(cur_scenario, ".csv")) %>%
    fread() %>%
    .[!is.na(year)]
  
  
  # Domestic in-migration data (proportions per age group)
  dom_in_mig_df  <- file.path(cur_inputs_path, 'domestic_in_migration.csv') %>%
    fread() %>%
    .[, Age := as.numeric(str_sub(Age, 1, 3))] %>%
    .[, Domestic.in.migration := ifelse(Domestic.in.migration > 1,
                                         0, Domestic.in.migration)]
  
  
  # Domestic out-migration data (proportions per age group)
  dom_out_mig_df  <- file.path(cur_inputs_path, 'domestic_out_migration.csv') %>%
    fread() %>%
    .[, Age := as.numeric(str_sub(Age, 1, 3))] %>%
    .[, Domestic.out.migration := ifelse(Domestic.out.migration > 1,
                                         0, Domestic.out.migration)]
  
  
  # Count of domestic in migrants by applying percentages to populations
  # female 
  # Asian 
  asian_female_dom_in <- as.numeric(scenario[year == cur_year, 'idm_AF']) *
    pop_df[str_detect(Race, 'Asian') & Sex == 'Female', sum(Population)] / 100
  
  # Black
  black_female_dom_in <- as.numeric(scenario[year == cur_year, 'idm_BF']) *
    pop_df[Race == 'Black' & Sex == 'Female', sum(Population)] / 100
  
  # Hispanic
  hisp_female_dom_in  <- as.numeric(scenario[year == cur_year, "idm_HF"]) *
    pop_df[Race == 'Hispanic' & Sex == 'Female', sum(Population)] / 100
  
  # White
  white_female_dom_in <- as.numeric(scenario[year == cur_year, 'idm_WF']) *
    pop_df[Race == 'White' & Sex == 'Female', sum(Population)] / 100
  

  # male 
  # Asian 
  asian_male_dom_in <- as.numeric(scenario[year == cur_year, 'idm_AM']) *
    pop_df[str_detect(Race, 'Asian') & Sex == 'Male', sum(Population)] / 100
  
  # Black
  black_male_dom_in <- as.numeric(scenario[year == cur_year, 'idm_BM']) *
    pop_df[Race == 'Black' & Sex == 'Male', sum(Population)] / 100
  
  # Hispanic
  hisp_male_dom_in  <- as.numeric(scenario[year == cur_year, "idm_HM"]) *
    pop_df[Race == 'Hispanic' & Sex == 'Male', sum(Population)] / 100
  
  # White
  white_male_dom_in <- as.numeric(scenario[year == cur_year, 'idm_WM']) *
    pop_df[Race == 'White' & Sex == 'Male', sum(Population)] / 100
  
  
  
  # Count of domestic out migrants by applying percentages to populations
  # female 
  # Asian 
  asian_female_dom_out <- as.numeric(scenario[year == cur_year, 'odm_AF']) *
    pop_df[str_detect(Race, 'Asian') & Sex == 'Female', sum(Population)] / 100
  
  # Black
  black_female_dom_out <- as.numeric(scenario[year == cur_year, 'odm_BF']) *
    pop_df[Race == 'Black' & Sex == 'Female', sum(Population)] / 100
  
  # Hispanic
  hisp_female_dom_out  <- as.numeric(scenario[year == cur_year, "odm_HF"]) *
    pop_df[Race == 'Hispanic' & Sex == 'Female', sum(Population)] / 100
  
  # White
  white_female_dom_out <- as.numeric(scenario[year == cur_year, 'odm_WF']) *
    pop_df[Race == 'White' & Sex == 'Female', sum(Population)] / 100
  
  
  # male 
  # Asian 
  asian_male_dom_out <- as.numeric(scenario[year == cur_year, 'odm_AM']) *
    pop_df[str_detect(Race, 'Asian') & Sex == 'Male', sum(Population)] / 100
  
  # Black
  black_male_dom_out <- as.numeric(scenario[year == cur_year, 'odm_BM']) *
    pop_df[Race == 'Black' & Sex == 'Male', sum(Population)] / 100
  
  # Hispanic
  hisp_male_dom_out  <- as.numeric(scenario[year == cur_year, "odm_HM"]) *
    pop_df[Race == 'Hispanic' & Sex == 'Male', sum(Population)] / 100
  
  # White
  white_male_dom_out <- as.numeric(scenario[year == cur_year, 'odm_WM']) *
    pop_df[Race == 'White' & Sex == 'Male', sum(Population)] / 100
  
  
  
  # Spread domestic migration according to the age and gender profiles
  if (dom_mig == 1){
    # Domestic in migration
    # Female
    # Asian
    dom_in_mig_df[str_detect(Race, 'Asian') & Sex == 'Female',
                  in_dom_mig := Domestic.in.migration * asian_female_dom_in] 
    
    # Black
    dom_in_mig_df[Race == 'Black' & Sex == 'Female',
                  in_dom_mig := Domestic.in.migration * black_female_dom_in]
   
     # Hispanic
    dom_in_mig_df[Race == 'Hispanic' & Sex == 'Female',
                  in_dom_mig := Domestic.in.migration * hisp_female_dom_in]
    
    # White
    dom_in_mig_df[Race == 'White' & Sex == 'Female',
                  in_dom_mig := Domestic.in.migration * white_female_dom_in]
    
    
    # Male
    # Asian
    dom_in_mig_df[str_detect(Race, 'Asian') & Sex == 'Male',
                  in_dom_mig := Domestic.in.migration * asian_male_dom_in] 
    
    # Black
    dom_in_mig_df[Race == 'Black' & Sex == 'Male',
                  in_dom_mig := Domestic.in.migration * black_male_dom_in]
    
    # Hispanic
    dom_in_mig_df[Race == 'Hispanic' & Sex == 'Male',
                  in_dom_mig := Domestic.in.migration * hisp_male_dom_in]
    
    # White
    dom_in_mig_df[Race == 'White' & Sex == 'Male',
                  in_dom_mig := Domestic.in.migration * white_male_dom_in]
    
    
    
    # Domestic out migration
    # Female
    # Asian
    dom_out_mig_df[str_detect(Race, 'Asian') & Sex == 'Female',
                  out_dom_mig := Domestic.out.migration * asian_female_dom_out] 
    
    # Black
    dom_out_mig_df[Race == 'Black' & Sex == 'Female',
                  out_dom_mig := Domestic.out.migration * black_female_dom_out]
    
    # Hispanic
    dom_out_mig_df[Race == 'Hispanic' & Sex == 'Female',
                  out_dom_mig := Domestic.out.migration * hisp_female_dom_out]
    
    # White
    dom_out_mig_df[Race == 'White' & Sex == 'Female',
                  out_dom_mig := Domestic.out.migration * white_female_dom_out]
    
    
    # Male
    # Asian
    dom_out_mig_df[str_detect(Race, 'Asian') & Sex == 'Male',
                  out_dom_mig := Domestic.out.migration * asian_male_dom_out] 
    
    # Black
    dom_out_mig_df[Race == 'Black' & Sex == 'Male',
                  out_dom_mig := Domestic.out.migration * black_male_dom_out]
    
    # Hispanic
    dom_out_mig_df[Race == 'Hispanic' & Sex == 'Male',
                  out_dom_mig := Domestic.out.migration * hisp_male_dom_out]
    
    # White
    dom_out_mig_df[Race == 'White' & Sex == 'Male',
                  out_dom_mig := Domestic.out.migration * white_male_dom_out]
  
  } else {
    
    dom_in_mig_df[, in_dom_mig := 0]
    dom_out_mig_df[, out_dom_mig := 0]
    
  }
  
  
  # Have domestic in, out and net migration in one table
  dom_mig_df <- copy(dom_in_mig_df)
  dom_mig_df <- dom_in_mig_df[dom_out_mig_df,
                              .(Race, Sex, Age, in_dom_mig, out_dom_mig, 
                                net_dom_mig = in_dom_mig - out_dom_mig),
                              on = c('Race', 'Sex', 'Age')]
  
  dom_mig_df[, Race := ifelse(str_detect(Race, 'Asian'), 'Asian', Race)]
  
  return(dom_mig_df)
  
}
