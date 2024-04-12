

complete_fertility_df <- function(cur_inputs_path){
  
  
  missing_ages <- c(seq(0, 14), seq(50, 100))
  
  
  fertility_df <- file.path(cur_inputs_path, 'fertility.csv') %>%
    fread() %>%
    .[, Age := as.numeric(str_sub(Age, 1, 3))] 
  
  dt_to_add <- data.table(Race = c('Asian and other', 'Black', 'Hispanic', 'White'),
                          Age = rep(0, 4), Fertility = rep(0, 4))
  
  
  dt_to_add <- dt_to_add[, .(Age = rep(Age, 66), Fertility = rep(Fertility, 66)),
                         by = Race]
  
  dt_to_add[, Age := c(seq(0, 14), seq(50, 100)), by = Race]
  
  
  fertility_df <- rbind(fertility_df, dt_to_add)
  
  
  fertility_df[, .(Age = order(Age), Fertility), by = Race]
  
  
  fertility_df <- fertility_df[order(Race, Age)]
  
  
  fwrite(fertility_df, file.path(cur_inputs_path, 'fertility.csv'))
}






