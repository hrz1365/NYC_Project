
calculate_max_mx <- function(gender, e0){
  
  if(str_detect(gender, '[F-f]emale')){
    
    if(e0 <= 70){
      
      mm <- -0.0000018*e0^3 + 0.0003520*e0^2 - 0.023099*e0 + 1.0650
      
    } else if(e0 > 70){
      
      mm <- 0.00000029*e0^3 - 0.0218*e0 + 2.0386
    }
    
    
  } else if(str_detect(gender, '[M-m]ale')){
    
    if(e0 <= 70){
      
      mm <- -0.0000021*e0^3 + 0.0003771*e0^2 - 0.023437*e0 + 1.11422
      
    } else if(e0 > 70){
      
      mm <- 0.00000037*e0^3 - 0.025077*e0 + 2.2893914
    }
  }
  
  return(mm)
}




