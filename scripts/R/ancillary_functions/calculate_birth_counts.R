
calculate_birth_counts <- function(pop_df, female_birth_rates_df, 
                                   male_birth_rates_df, race) {
  
  # Current female population
  cur_female_pop <- pop_df[str_detect(Race, race) & Sex == 'Female'][, Population]
  
  
  # Birth rates for females and males
  cur_female_birth_rates <- female_birth_rates_df[str_detect(Race, race)][, b2_col]
  cur_male_birth_rates   <- male_birth_rates_df[str_detect(Race, race)][, b2_col]
  
  
  # Calculation of male and female counts 
  female_birth_counts <- sum(cur_female_pop * cur_female_birth_rates)
  male_birth_counts   <- sum(cur_female_pop * cur_male_birth_rates)
  
  
  output_vector        <- c(female_birth_counts, male_birth_counts)
  names(output_vector) <- c('female_births', 'male_births')
  
  return(output_vector)
}


