#' Sample mean parameter of the Log-normal posterior
#'
#' This is the main workhorse for the Log-normal A/B test. The idea to leverage the results we have for the Normal model
#' by log transforming the data (so that it's normally distributed), sampling the posterior for the Normal model, and
#' then mapping back to the original data space.
#'
#' Based on \href{http://engineering.richrelevance.com/bayesian-ab-testing-with-a-log-normal-model}{Python code} by Sergey Feldman.
#' @param data vector of observed data for one group, assumed to be generated from a Log-Normal distribution
#' @param m0 mean parameter for prior on mu | sigma^2
#' @param k0 scaling parameter for the variance of the normal prior on mu|sigma^2
#' @param s_sq0 number of degrees of freedom of variance
#' @param v0 scale of the sigma_squared parameter. Compare with number of data samples.
#' @param nsim number of monte carlo samples

#' @seealso \code{\link{lognormal_ab_test}} to run an A/B test with a Lognormal model (i.e. for testing revenue)
#' @seealso \code{\link{plot_revenue_prior}} to plot all components of the prior distribution for the revenue model
#' @seealso \code{\link{draw_mus_and_sigmas}} workhorse function to sample parameters of a normal posterior

#' @references \url{http://engineering.richrelevance.com/bayesian-ab-testing-with-a-log-normal-model/}
#' @references \url{https://en.wikipedia.org/wiki/Log-normal_distribution}
#' @references Gelman, Andrew, et al. Bayesian data analysis. Vol. 2. London: Chapman & Hall/CRC, 2014.

draw_log_normal_means <- function(data, m0, k0, s_sq0, v0, nsim = 10000) {
  # log transform the data so that it's normally distributed
  log_data <- log(data)
  
  # get samples from the joint posterior P(mu, sigma^2 | data)
  mu_sig_sq_samples <- draw_mus_and_sigmas(log_data, m0, k0, s_sq0, v0, nsim)
  
  # map each (mu, sigma^2) pair back to data space by finding the log-normal
  # mean https://en.wikipedia.org/wiki/Log-normal_distribution#Arithmetic_moments
  
  log_normal_mean_samples <- exp(mu_sig_sq_samples[[1]] + mu_sig_sq_samples[[2]] / 2)
  
  return(log_normal_mean_samples)
}
