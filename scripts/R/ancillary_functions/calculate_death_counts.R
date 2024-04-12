calculate_death_counts <- function(pop_df, survival_df, race) {
  
  # Current population
  cur_female_pop <- pop_df[str_detect(Race, race) & Sex == 'Female'][, Population]
  cur_male_pop   <- pop_df[str_detect(Race, race) & Sex == 'Male'][, Population]
  
  
  # Birth rates for females and males
  cur_female_fatality_rates <- 1 - survival_df[str_detect(Race, race) & 
                                                 Sex == 'Female'][, Sx]
  cur_male_fatality_rates   <- 1 - survival_df[str_detect(Race, race) &
                                                 Sex == 'Male'][, Sx]
  
  
  # Calculation of male and female counts 
  female_death_counts <- sum(cur_female_pop * cur_female_fatality_rates)
  male_death_counts   <- sum(cur_male_pop * cur_male_fatality_rates)
  
  
  output_vector        <- c(female_death_counts, male_death_counts)
  names(output_vector) <- c('female_deaths', 'male_deaths')
  
  return(output_vector)
}
