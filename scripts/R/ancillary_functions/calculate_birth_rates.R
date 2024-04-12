
calculate_birth_rates <- function(cur_fertility_df, cur_srb_df, cur_Lx_df, cur_lx_df,
                                  cur_survival_df, gender){


  # Calculate birth rates (Fertility * sex ratio)
  birth_rates_df <- cur_fertility_df[cur_srb_df, 
                                     on = 'Race']
  birth_rates_df[, birth_proportion := get(str_subset(names(.SD), 'rates')) * get(gender)]
  
  
  
  # Survival values
  cur_survival_df <- cur_survival_df[Sex == gender, ]
  
  
  previous_L0     <- cur_Lx_df[Age == 0 & Sex == gender, .(Race, Lx)]
  previous_l0     <- cur_lx_df[Age == 0 & Sex == gender, .(Race, lx)]
  l_proportion_df <- previous_L0[previous_l0, .(Race, Lx, lx, l_proportion = Lx / lx),
                                 on = 'Race']
  
  
  # pure births
  birth_rates_df[, avg_rate := 0.5 * (birth_proportion + shift(birth_proportion,
                                                               type = 'lead', 1))]
  total_birth_df <- cur_survival_df[birth_rates_df, on = c('Age', 'Race', 'Year' = 'year')]
  total_birth_df[, b1_col := avg_rate * Sx]
  

  
  # births after mortality adjustment
  total_birth_df[l_proportion_df, b2_col := l_proportion * b1_col, on = 'Race']
  
  
  
  # Replace NA with 0
  total_birth_df[is.na(total_birth_df)] <- 0
  


  return(total_birth_df)
}
