
calculate_e0 <- function(mortality_df, cur_e0, lx_column, gender){
  
  
  cur_mortality_df <- copy(mortality_df)
  
  if (lx_column == 'lx') {
    
    NULL
  } else if (lx_column != 'lx' & 'lx' %in% colnames(cur_mortality_df)) {
    
    cur_mortality_df[, lx:=NULL]
    
  }
  
  setnames(cur_mortality_df, lx_column, 'lx')
  
  
  # Percentage of survivals
  cur_mortality_df[, ax := ifelse(Age == 0, 0.33, 0.5)]


  # Deaths per age group (dx)
  # Note: dx = lx0 - lx1;
  cur_mortality_df[, dx := lx - shift(lx, type = 'lead')]

  
  
  # Exposure to death per person year (Lx)
  cur_mortality_df[, Lx := shift(lx, type = 'lead') + dx * ax]
  
  

  # Mortality rate (mx); the percentage of individuals in a certain age group that died in the last year
  # Note: mx=dx/Lx
  cur_mortality_df[, mx := dx / Lx]
  
  
  
  # Compute mx for the last age category (e.g., 100+)
  last_mx <- calculate_max_mx(gender, cur_e0) 
  cur_mortality_df[Age == 100, mx := last_mx]

  
  
  # Compute Lx value for oldest age category (Lxmax)
  # Note: Lxmax=lxmax/mxmax
  cur_mortality_df[Age == 100, Lx := lx / mx]
  
  
  
  # Cumulative person years of survival
  # Note: Cumulative Lx from oldest to youngest age group
  cur_mortality_df[, Tx := rev(cumsum(rev(Lx)))]
  
  
  
  # Generate proportion of individuals that survived to the next age group (Sx)
  # Note: Sx=Lx1/Lx0; Smax-1=S99=T100+/T99; Smax=0
  cur_mortality_df[, Sx := shift(Lx, type = 'lead') / Lx]
  cur_mortality_df[Age >= 99, Sx := shift(Tx, type = 'lead') / Tx]
  cur_mortality_df[Age == 100, Sx := 0]

  
  
  # Compute life expectancy by age (ex)
  # Note: ex=Tx/lx
  cur_mortality_df[, ex := Tx / lx]

  

  # Obtain life expectancy at birth (e0)
  empirical_e0 <- cur_mortality_df[Age == 0, ex]

  return(empirical_e0)
}
