
update_lx <- function(cur_mortality_df, un_column, e0_empirical, e0_UN, 
                      e0_scenario){

  # Generate scaler consisting of y values
  scaler <- (e0_scenario - e0_empirical) / (e0_UN - e0_empirical)
  
  
  # Compute xi values
  updated_lx <- cur_mortality_df[, lx] + (cur_mortality_df[[un_column]] - cur_mortality_df[, lx]) * scaler

  return(updated_lx)
}


