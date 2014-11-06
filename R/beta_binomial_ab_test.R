beta_binomial_ab_test <- function(y,n, alpha0=1, beta0=1, tolerance=.001, nsim=1e5, plot.density=TRUE, conf.level=.1, expected_conversion_rate=NULL, groups=NULL){            

  
  # parameterize either in terms of expected_conversion_rate if it is provided
  if (!is.null(expected_conversion_rate)) beta0 <- 2 - alpha0 + (alpha0 - 1) / expected_conversion_rate    
    
  ##
  
  ngroups <- length(y)
  
  ci <- matrix(nrow=ngroups, ncol=2, dimnames=list(NULL, c('lower', 'upper')))
  samps <- matrix(nrow=ngroups, ncol=nsim) #monte carlo samples. Each row is an A/B/C.. group.
  
  #posterior parameters
  alpha <- alpha0 + y
  beta <- n - y + beta0
  
  #Sample the posterior P(B>A). 
  for (g in 1:ngroups){
    samps[g,] <- rbeta(nsim, alpha[g], beta[g])
    ci[g,] <- qbeta(c(conf.level/2, 1-conf.level/2), alpha[g], beta[g])         
  }
  
  posterior.mean <- rowMeans(samps)
  
  # compute matrix where each entry is 1 if a sample is the best across all groups, 0 otherwise, and take the row means.
  prob.winning <- rowMeans(samps == matrix(rep(apply(samps, FUN=max, MARGIN=2), ngroups), nrow=ngroups, byrow=TRUE) )  
  
  # plot posterior density
  if(plot.density){
    for (g in 1:ngroups)         
      curve(dbeta(x,alpha[g], beta[g]), from=0, to=.15, n=1e5, col='darkblue', lwd=2, xlab='retention', ylab='density',add=g>1)            
  }
  
  ##Stop the test early if the risk is below tolerance    
      
    #winner <- which.max(prob.winning) #which group has the highest posterior probability of winning
    #loss <- apply(samps, FUN=max, MARGIN=2) - samps[winner,] #difference between the MAP mean (mean for the group we think is the best) and the group that is actually the best  
    #risk <- mean(loss)
    
    # compute risk associated with each possible decision
    risk <- rep(NA_real_,ngroups)
    for (g in 1:ngroups){
      loss <- apply(samps, FUN=max, MARGIN=2) - samps[g,] #difference between the MAP mean and the group that is actually the best        
      risk[g]  <- mean(loss)            
    }
    
    return(list(y=y, 
                n=n, 
                risk=risk,
                winner=(1:ngroups)[risk < tolerance], 
                stop.test=min(risk)<tolerance, 
                tolerance=tolerance, 
                prob.winning=prob.winning, 
                posterior.mean=posterior.mean, 
                ci=ci, 
                conf.level=conf.level, 
                posterior_parameters=data.frame(alpha=alpha, beta=beta), 
                groups=groups)
           ) 
} 