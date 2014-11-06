plot_conversion_prior <- function( expected_conversion_rate=NA, alpha0=NA, beta0=NA){
      
  if (!is.na(expected_conversion_rate) & expected_conversion_rate == 0)
    stop('expected_conversion_rate must be strictly positive!')
    
  if (sum(is.na(c(alpha0,beta0,expected_conversion_rate))) !=1)
    stop('Specify exactly two of {expected_conversion_rate, alpha0,beta0}')
  
  #compute beta0 if needed
  if (!is.na(expected_conversion_rate)) 
    beta0 <- 2 - alpha0 + (alpha0 - 1)/expected_conversion_rate
    
  curve(dbeta(x, shape1=alpha0, shape2=beta0))
    
}