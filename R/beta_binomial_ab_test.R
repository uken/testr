#' Run a Beta Binomial A/B Test
#'
#' Perform bayesian analysis of an experiment with a binary response variable.
#'   The experiment is modelled with a binomial likelihood and a beta prior.
#'   The beta is parameterized either with two shape parameters {alpha0, beta0}
#'   or with one shape parameter alpha0 and the \code{expected_conversion_rate}
#'   (mode of the beta prior).
#'
#' @param y vector of counts of successes (i.e. number of converted users
#'   across the groups)
#' @param n vector of counts of trials (i.e number of total users)
#' @param alpha0 first shape parameter for beta prior. Increasing alpha0
#'   reduces uncertainty about \code{expected_conversion_rate}.
#' @param beta0 second shape parameter for beta prior. Ignored if
#'   \code{expected_conversion_rate} is given.
#' @param tolerance smallest difference we care about
#' @param nsim number of monte carlo samples
#' @param conf.level specifies alpha for (1-alpha)*100\% credible intervals
#' @param expected_conversion_rate before seeing the data, what is is the most
#'   likely conversion rate (i.e. mode of the beta prior)? From 0 to 1.
#' @param groups A character vector of group labels. Defaults to "A", "B", ...
#'
#' @return object of class beta_binomial_ab_test
#'
#' @examples
#'   beta_binomial_ab_test(y = c(100,120), n = c(1000, 999),
#'                         expected_conversion_rate = 0.1, alpha0 = 0.1)
#' @seealso \code{\link{plot.beta_binomial_ab_test}} to plot the marginal
#'   posteriors
#' @seealso \code{\link{plot_conversion_prior}} to plot the beta prior given
#'   its parameters
#' @seealso \code{\link{lognormal_ab_test}} to run an A/B test with a
#'   Lognormal model (i.e. for testing revenue)
#' @references \url{http://engineering.richrelevance.com/bayesian-ab-tests} for
#'   derivation of the model
#' @references \url{http://ayakubovich.github.io/bayes.pdf} for a
#'   parameterization fo the model in terms of expected conversion rate
#' @export

beta_binomial_ab_test <- function(y, n, alpha0 = 1, beta0 = 1, tolerance = 0.001, nsim = 1e5,
                                  conf.level = 0.1, expected_conversion_rate = NULL,
                                  groups = LETTERS[1:length(y)]) {
  if (any(alpha0 <= 0, beta0 <= 0))
    stop("alpha0 and beta0 must be positive.")
  
  # parameterize either in terms of expected_conversion_rate if it is provided
  if (!is.null(expected_conversion_rate)) {
    if (expected_conversion_rate <= 0)
      stop("expected_conversion_rate must be positive.")
    beta0 <- 2 - alpha0 + (alpha0 - 1) / expected_conversion_rate
  }
  
  ngroups <- length(y)
  
  # Create container
  ci <- matrix(nrow = ngroups,
               ncol = 2,
               dimnames = list(NULL, c('lower', 'upper')))
  samps <- matrix(nrow = ngroups, ncol = nsim)
  
  # Calculate posterior parameters
  alpha <- alpha0 + y
  beta <- n - y + beta0
  
  # Sample the posterior to find P(B>A)
  for (g in 1:ngroups) {
    samps[g,] <- rbeta(nsim, alpha[g], beta[g])
    ci[g,] <- qbeta(c(conf.level / 2,
                      1 - conf.level / 2),
                    alpha[g],
                    beta[g])
  }
  
  # Compose output
  posterior.mean <- rowMeans(samps)
  
  # compute matrix where each entry is 1 if a sample is the best across all groups, 0 otherwise, and take the row means.
  prob.winning <- rowMeans(samps == matrix(rep(
    apply(samps,
          FUN = max,
          MARGIN = 2),
    ngroups
  ),
  nrow = ngroups,
  byrow = TRUE))
  
  # compute risk associated with each possible decision
  risk <- rep(NA_real_, ngroups)
  for (g in 1:ngroups) {
    loss <- apply(samps,
                  FUN = max,
                  MARGIN = 2) - samps[g,] #difference between the MAP mean and the group that is actually the best
    risk[g]  <- mean(loss)
  }
  
  return(structure(
    list(
      y = y,
      n = n,
      risk = risk,
      winner = groups[risk < tolerance],
      stop.test = min(risk) < tolerance,
      tolerance = tolerance,
      prob.winning = prob.winning,
      posterior.mean = posterior.mean,
      ci = ci,
      conf.level = conf.level,
      posterior_parameters = data.frame(alpha = alpha,
                                        beta = beta),
      groups = groups
    ),
    class = "beta_binomial_ab_test"
  ))
}
