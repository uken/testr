# sample the posterior P(mu, sigma^2 | X) assuming inverse gamma prior on sigma^2 and normal prior on mu|sigma^2
# m0 - Guess about where the mean is.
# k0 - Certainty about m0.  Compare with number of data samples.
# s_sq0 - Number of degrees of freedom of variance.
# v0 - Scale of the sigma_squared parameter.  Compare with number of data samples.
draw_mus_and_sigmas <- function(data,
                                m0 = 1,
                                k0 = 1,
                                s_sq0 = 1,
                                v0 = 1,
                                n_samples = 10000) {
  #require(MCMCpack)
  N <- length(data)   # number of samples
  the_mean <- mean(data) # find the mean of the data
  SSD <- sum((data - the_mean)^2) # sum of squared differences between data and mean

  # combining the prior with the data - page 79 of Gelman et al.
  # to make sense of this note that inv-chi-sq(v,s^2) = inv-gamma(v/2,(v*s^2)/2)
  kN <- k0 + N
  mN <- (k0/kN)*m0 + (N/kN)*the_mean
  vN <- v0 + N
  vN_times_s_sqN <- v0*s_sq0 + SSD + (N*k0*(m0-the_mean)**2)/kN

  # 1) draw the variances from an inverse gamma # (params: alpha, beta)
  alpha <- vN/2
  beta <- vN_times_s_sqN/2
  # thanks to wikipedia, we know that:
  # if X ~ inv-gamma(a,1) then b*X ~ inv-gamma(a,b)

  sig_sq_samples <- beta * 1/rgamma(n = n_samples, shape = alpha, rate = 1)
  #sig_sq_samples <- beta * rinvgamma(n=n_samples,shape=alpha)

  mean_norm <- mN
  sd_norm <- sqrt(sig_sq_samples/kN)
  mu_samples <- rnorm(n=n_samples, mean=mean_norm,sd=sd_norm)

  return(list(mu_samples = mu_samples,
              sig_sq_samples = sig_sq_samples,
              mu_par = list(mN=mN, kN=kN),
              sig_sq_par = list(vN= vN, s_sqN = vN_times_s_sqN/vN)))
}
