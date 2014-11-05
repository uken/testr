lognormal_ab_test <- function(data, nsim=1e5, alpha0=1, beta0=25, m0=4, k0=1, s_sq0=1, v0=5, expected_revenue_converted_users=NULL, expected_conversion_rate=NULL, plot.density=FALSE, conf.level=.1, tolerance=.01, save.hist=TRUE){  
  #bayes.lognormal.test <- function(data, nsim=1e5, alpha0=1, beta0=1, m0=4, k0=1, s_sq0=1, v0=1, plot.density=FALSE, conf.level=.1, tolerance=.01){  
  
  # probability model:  
  # conversion - binomial likelihood, beta prior with shape parameters alpha0, beta0  
  # revenue - Normal likelihood model for log(revenue). Prior: 
  # sigma^2 - inverse gamma prior with shape=v0, scale=s_sq0
  # mu | sigma^2 - normal prior with mean=m0, variance=sigma^2/k0      
  
  if (!is.null(expected_conversion_rate)) beta0 <- 2 - alpha0 + (alpha0 - 1) / expected_conversion_rate #optionally parameterize the beta prior using monetization rate
  
  if (!is.null(expected_revenue_converted_users)) m0 <- log(expected_revenue_converted_users)- .5 * v0 * s_sq0 / (v0 + 2)  #optionally parameterize the normal prior using mean spender lt
  
  #create matrix where each row is a group (pad with NAs)
  groups <- unique(data[,1])
  ngroups <- length(groups)
  n <- as.numeric(table(data[,1]))
  max_n <- max(n)
  
  names(data)[1:2] <- c('ab_group', 'revenue')
  
  sample.mean <- aggregate(revenue ~ ab_group, data=data, FUN=mean)$revenue
  
  data_matrix <- matrix(nrow=ngroups, ncol=max_n)
  for (g in 1:ngroups){
    new_data <- data[data[,1]==groups[g],2]
    length(new_data) <- max_n #pad with NA if needed
    data_matrix[g,] <- new_data                
  }
  data <- data_matrix    
  
  ####
  nonzero.count <- rowSums(data>0, na.rm=TRUE)
  sample.sizes <- rowSums(!is.na(data)) #vector of sample sizes 
  
  #preallocation
  ngroups <- nrow(data)
  conv.samps <- spenders.rev.samps <- rev.samps <- matrix(nrow=ngroups, ncol=nsim)
  ci <- matrix(nrow=ngroups, ncol=2, dimnames=list(NULL, c('lower', 'upper')))
  
  for (g in 1:ngroups){      
    #compute nonzero data
    nonzeros <- data[g, ]
    nonzeros <- nonzeros[nonzeros>0 & !is.na(nonzeros)]
    
    #sample posteriors
    conv.samps[g,] <- rbeta(nsim, nonzero.count[g] + alpha0, sample.sizes[g] - nonzero.count[g] + beta0) #samples from the distribution of conversion rates
    spenders.rev.samps[g,] <- draw_log_normal_means(nonzeros, m0,k0,s_sq0, v0, nsim) #samples from the distribution of spenders' revenue    
    rev.samps[g,] <- conv.samps[g,] * spenders.rev.samps[g,]
    
    #compute credible intervals
    ci[g,] <- quantile(rev.samps[g,], probs=c(conf.level/2, 1-conf.level/2))            
  }
  
  posterior.mean <- rowMeans(rev.samps)
  prob.winning <- rowMeans(rev.samps == matrix(rep(apply(rev.samps, FUN=max, MARGIN=2), ngroups), nrow=ngroups, byrow=TRUE)) # [P(group 1 is best), P(group 2 is best) ...]
  
  #########################
  # visualization
  posterior.samples <- data.frame(mean.revenue=as.numeric(t(rev.samps)), group=as.factor(rep(1:ngroups, each=nsim)))
  if (plot.density){
    x <- ggplot(posterior.samples, aes(x=mean.revenue, y=..ncount.., fill=group)) +  geom_histogram(alpha=0.2, position="identity", binwidth=.005)
    print(x)
  }      
  
  if (save.hist){    
    hist.data <- data.frame(group=NA, bin=NA, density=NA)
    for (g in 1:ngroups){
      h <- hist(posterior.samples[posterior.samples[, 'group']==g, 'mean.revenue'], plot=FALSE)
      hist.data <- rbind(hist.data, data.frame(group=g, bin=h$mids, density=h$density))      
    }          
  }
  hist.data <- hist.data[-1,]
  
  # stopping rule  
  
  #winner <- which.max(prob.winning) #which group has the highest posterior probability of winning  
  #loss <- apply(rev.samps, FUN=max, MARGIN=2) - rev.samps[winner,] #difference between the MAP mean (mean for the group we think is the best) and the group that is actually the best  
  #risk <- mean(loss)
  
  risk <- rep(NA_real_,ngroups)
  for (g in 1:ngroups){
    loss <- apply(rev.samps, FUN=max, MARGIN=2) - rev.samps[g,] #difference between the MAP mean and the group that is actually the best        
    risk[g]  <- mean(loss)            
  }
  
  return(list(risk=risk,winner=(1:ngroups)[risk < tolerance], stop.test=min(risk)<tolerance, tolerance=tolerance, prob.winning=prob.winning, posterior.mean=posterior.mean, ci=ci, conf.level=conf.level, hist.data=hist.data, n=n, sample.mean=sample.mean)) #sample size and winner
  #return(list(risk=risk, stop.test=risk<tolerance, tolerance=tolerance, prob.winning=prob.winning, posterior.mean=posterior.mean, ci=ci, conf.level=conf.level)) #sample size and winner when test stops      
  
}
