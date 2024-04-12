
update_population <- function(prev_pop_df, transition_matrix, race) {
  
  
  upd_pop       <- transition_matrix%*%as.matrix(prev_pop_df)
  
  upd_pop_df    <- data.table(Race = race, Sex = rep(c('Female', 'Male'), each = 101),
                              Age  = rep(0:100, times = 2), Population = upd_pop[, 1]) 
  
  return(upd_pop_df)
}
