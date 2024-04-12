
calculate_target_value <- function(fertility_df, ref_x, target_col, ref_value){
  
  
  cur_df <- copy(fertility_df)
  cur_df <- cur_df[, .SD, .SDcols = c(target_col, ref_x)]
  
  
  # Find two values of target x closest to the reference value
  cur_df[, diff := abs(cur_df[[ref_x]] - ref_value)]
  
  pick <- cur_df[order(diff)][[ref_x]][2:1]
  
  
  # Define distance measures
  pick_diff <- pick[2] - pick[1] 
  tot_diff  <- ref_value - pick[1] 
  
  
  # difference between respective target values
  target_diff <- cur_df[get(ref_x) == pick[2], .SD, .SDcols = target_col][[1]] - 
    cur_df[get(ref_x) == pick[1], .SD, .SDcols = target_col][[1]] 
  
  
  # Estimate the age corresponding to the percentile value through linear interpolation
  target_value <- (target_diff * tot_diff / pick_diff) + cur_df[get(ref_x) == pick[1], 
                                                                .SD, .SDcols = target_col][[1]]

  return(target_value)
}




