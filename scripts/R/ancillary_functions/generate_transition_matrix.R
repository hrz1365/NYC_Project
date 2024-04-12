
generate_transition_matrix <- function(cur_survival_df, female_birth_rates,
                                       male_birth_rates, race){
  
  
  # Generate transition matrix
  survival_values <- cur_survival_df[Age < 100 & Race == race, .(Sex, Sx)]
  
  
  female_survival <- cbind(diag(survival_values[Sex == 'Female', Sx]), 0)
  male_survival   <- cbind(diag(survival_values[Sex == 'Male', Sx]), 0)
  
  
  # Horizontal matrix pieces of Birth rates
  female_birth <- female_birth_rates[Race == race, b2_col]
  male_birth   <- male_birth_rates[Race == race, b2_col]
  
  
  
  
  # Generate quadrant pieces of transition matrix
  # female births + female survival
  mat_11 <- rbind(unname(female_birth), female_survival) 
  # no values
  mat_12 <- matrix(0, nrow = nrow(mat_11), ncol = ncol(mat_11))  
  # male births: the new born boys are now computed based on the population of reproductive females
  mat_21 <- rbind(unname(male_birth), matrix(0, nrow = (nrow(mat_11) - 1), 
                                             ncol = ncol(mat_11))) 
  # male survival
  mat_22 <- rbind(matrix(0, nrow = (nrow(mat_11) - nrow(male_survival)),
                         ncol = ncol(mat_11)), male_survival) 
  
  # Combine quadrant pieces to complete matrix
  transition_matrix <- rbind(cbind(mat_11, mat_12), cbind(mat_21, mat_22))
  
  
  return(transition_matrix)
}
