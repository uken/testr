#' Sample the the joint posterior for the Normal model.
#'
#' Sample the join posterior P(mu, sigma^2 | X) assuming inverse gamma prior on sigma^2 and normal prior on mu|sigma^2.
#' Based on \href{http://engineering.richrelevance.com/bayesian-analysis-of-normal-distributions-with-python}{Python code} by Sergey Feldman.
#' @param data vector of observed data, assumed to be generated from a Log-Normal distribution
#' @param m0 mean parameter for prior on mu | sigma^2
#' @param k0 scaling parameter for the variance of the normal prior on mu|sigma^2
#' @param s_sq0 number of degrees of freedom of variance
#' @param v0 scale of the sigma_squared parameter. Compare with number of data samples.
#' @param nsim number of monte carlo samples

#' @seealso \code{\link{lognormal_ab_test}} to run an A/B test with a Lognormal model (i.e. for testing revenue)
#' @seealso \code{\link{plot_revenue_prior}} to plot all components of the prior distribution for the revenue model
#' @seealso \code{\link{draw_mus_and_sigmas}} workhorse function to sample parameters of a normal posterior
#' @references \url{http://engineering.richrelevance.com/bayesian-analysis-of-normal-distributions-with-python}
#' @references \url{https://en.wikipedia.org/wiki/Gamma_distribution#Scaling}
#' @references Gelman, Andrew, et al. Bayesian data analysis. Vol. 2. London: Chapman & Hall/CRC, 2014.

draw_mus_and_sigmas <- function(data, m0 = 1, k0 = 1, s_sq0 = 1, v0 = 1, nsim = 10000) {
  N <- length(data)                 # number of samples
  the_mean <- mean(data)            # find the mean of the data
  SSD <- sum((data - the_mean) ^ 2) # sum of squared differences between data and mean
  
  # combining the prior with the data - page 79 of Gelman et al.
  # note that inv-chi-sq(v,s^2) = inv-gamma(v/2,(v*s^2)/2)
  kN <- k0 + N
  mN <- (k0 / kN) * m0 + (N / kN) * the_mean
  vN <- v0 + N
  vN_times_s_sqN <- v0 * s_sq0 + SSD + (N * k0 * (m0 - the_mean) * 2) / kN
  
  # draw the variances from an inverse gamma # (params: alpha, beta)
  alpha <- vN / 2
  beta <- vN_times_s_sqN / 2
  # if X ~ inv-gamma(a,1) then b*X ~ inv-gamma(a,b) [https://en.wikipedia.org/wiki/Gamma_distribution#Scaling]
  sig_sq_samples <- beta * 1 / rgamma(n = nsim, shape = alpha, rate = 1)
  
  mean_norm <- mN
  sd_norm <- sqrt(sig_sq_samples / kN)
  mu_samples <- rnorm(n = nsim, mean = mean_norm, sd = sd_norm)
  
  return(
    list(
      mu_samples = mu_samples,
      sig_sq_samples = sig_sq_samples,
      mu_par = list(mN = mN, kN = kN),
      sig_sq_par = list(vN = vN, s_sqN = vN_times_s_sqN / vN)
    )
  )
}
