draw_log_normal_means <- function(data,
                                  m0,
                                  k0,
                                  s_sq0,
                                  v0,
                                  nsim=10000) {

  log_data <- log(data) # log transform the data

  # get samples from the joint posterior P(mu, sigma^2 | data)
  mu_sig_sq_samples <- draw_mus_and_sigmas(log_data,m0,k0,s_sq0,v0,nsim) #

  # map back to data space by finding log-normal mean
  log_normal_mean_samples <- exp(mu_sig_sq_samples[[1]] + mu_sig_sq_samples[[2]]/2)

  return(log_normal_mean_samples)
}
