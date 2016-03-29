#' Run a A/B Test with a Lognormal model (e.g. for testing revenue)
#'
#' Perform bayesian analysis of an experiment with the following probability model:
#' \enumerate{
#'   \item Conversion - binomial likelihood, beta prior with shape parameters
#'     alpha0, beta0
#'   \item Revenue amongst spenders - Normal likelihood model for log(revenue).
#' }
#' The joint prior P(mu, sigma^2) is specified as a product of two priors:
#' \enumerate{
#'   \item sigma^2 - inverse gamma prior with shape=v0, scale=s_sq0
#'   \item mu | sigma^2 - normal prior with mean=m0, variance=sigma^2/k0
#' }
#' Based on \href{http://engineering.richrelevance.com/bayesian-analysis-of-normal-distributions-with-python}{Python code} by Sergey Feldman.
#' 
#' @param data A data frame with two columns: a grouping variable and customer
#'   lifetime value.
#' @param alpha0 first shape parameter for beta prior. Increasing alpha0
#'   reduces uncertainty about expected_conversion_rate.
#' @param beta0 second shape parameter for beta prior. Ignored if
#'   expected_conversion_rate is given.
#' @param tolerance smallest difference we care about
#' @param nsim number of monte carlo samples
#' @param conf.level specifies alpha for (1-alpha)*100\% credible
#'   intervals
#' @param expected_conversion_rate before seeing the data, what is is the most
#'   likely conversion rate (i.e. mode of the beta prior)? From 0 to 1.
#' 
#' @return object of class lognormal_ab_test
#' 
#' @examples
#' # Simulate data from the zero inflated lognormal likelihood.
#' n <- 10000 # users in each group
#' conversion = 0.1 # conversion rate. In this example, it is common to all groups
#' A_data <- rbinom(n, 1, conversion) * rlnorm(n, meanlog = 0, sdlog = 0.2)
#' B_data <- rbinom(n, 1, conversion) * rlnorm(n, meanlog = 0.08, sdlog = 0.2)
#' C_data <- rbinom(n, 1, conversion) * rlnorm(n, meanlog = 0.15, sdlog = 0.2)
#' data <- data.frame(ab_group = rep(c('A', 'B', 'C'), each = n), ltv = c(A_data, B_data, C_data))
#' plot_revenue_prior(expected_conversion_rate = 0.12, alpha0 = 15,
#'                    expected_revenue_converted_users = 1.5,
#'                    v0 = 73, k0 = 100, s_sq0 = 1.2)
#' l  =  lognormal_ab_test(data, expected_conversion_rate = 0.65, alpha0 = 15,
#'                         expected_revenue_converted_users = 1.5,
#'                         v0 = 73, k0 = 100, s_sq0 = 1.2)
#' plot(l)
#' 
#' @seealso \code{\link{plot.lognormal_ab_test}} to plot the marginal posteriors
#' @seealso \code{\link{plot_revenue_prior}} to plot all components of the prior distribution
#' @seealso \code{\link{draw_mus_and_sigmas}} workhorse function to sample parameters of a normal posterior
#' @seealso \code{\link{draw_log_normal_means}} workhorse function to sample the mean parameter of a lognormal posterior
#' @seealso \code{\link{beta_binomial_ab_test}} to run a Beta-Binomial A/B test (i.e. for conversion rate)
#' @references \url{https://en.wikipedia.org/wiki/Log-normal_distribution}
#' @references \url{http://engineering.richrelevance.com/bayesian-ab-testing-with-a-log-normal-model/}
#' @references Gelman, Andrew, et al. Bayesian data analysis. Vol. 2. London: Chapman & Hall/CRC, 2014.
#' @references \url{http://ayakubovich.github.io/bayes.pdf} for a parameterization of the model in terms of expected conversion rate and revenue amongst spenders
#' @export

lognormal_ab_test <- function(data, nsim = 1e5, alpha0 = 1, beta0 = 25, m0 = 4, k0 = 1, s_sq0 = 1, v0 = 5,
                              expected_revenue_converted_users = NULL, expected_conversion_rate = NULL,
                              conf.level = 0.1, tolerance = 0.01, plot.density = FALSE, save.hist = TRUE) {
  # Probability model:
  # conversion - binomial likelihood, beta prior with shape parameters alpha0, beta0
  # revenue - Normal likelihood model for log(revenue). Prior:
  # sigma^2 - inverse gamma prior with shape=v0, scale=s_sq0
  # mu | sigma^2 - normal prior with mean=m0, variance=sigma^2/k0
  
  if (!is.null(expected_conversion_rate)) {
    beta0 <- 2 - alpha0 + (alpha0 - 1) / expected_conversion_rate
  }
  
  if (!is.null(expected_revenue_converted_users)) {
    m0 <- log(expected_revenue_converted_users) - 0.5 * v0 * s_sq0 / (v0 + 2)
  }
  
  #create matrix where each row is a group (pad with NAs)
  names(data)[1:2] <- c('ab_group', 'revenue')
  
  n <- as.numeric(table(data[, 1]))
  max_n <- max(n)
  groups <- sort(unique(as.character(data$ab_group)))
  ngroups <- length(groups)
  sample.mean <- aggregate(revenue ~ ab_group, data = data, FUN = mean)$revenue
  
  data_matrix <- matrix(nrow = ngroups, ncol = max_n)
  for (g in 1:ngroups) {
    new_data <- data[data[, 1] == groups[g], 2]
    length(new_data) <- max_n #pad with NA if needed
    data_matrix[g, ] <- new_data
  }
  data <- data_matrix
  
  ####
  nonzero.count <- rowSums(data > 0, na.rm = TRUE)
  sample.sizes <- rowSums(!is.na(data)) #vector of sample sizes
  
  #preallocation
  ngroups <- nrow(data)
  conv.samps <- spenders.rev.samps <- rev.samps <- matrix(nrow = ngroups, ncol = nsim)
  ci <- matrix(nrow = ngroups, ncol = 2, dimnames = list(NULL, c('lower', 'upper')))
  
  for (g in 1:ngroups) {
    #compute nonzero data
    nonzeros <- data[g,]
    nonzeros <- nonzeros[nonzeros > 0 & !is.na(nonzeros)]
    
    #sample posteriors
    conv.samps[g, ] <- rbeta(nsim, nonzero.count[g] + alpha0,
                             sample.sizes[g] - nonzero.count[g] + beta0) #samples from the distribution of conversion rates
    spenders.rev.samps[g, ] <- draw_log_normal_means(nonzeros, m0, k0, s_sq0, v0, nsim) #samples from the distribution of spenders' revenue
    rev.samps[g, ] <- conv.samps[g, ] * spenders.rev.samps[g, ]
    
    #compute credible intervals
    ci[g, ] <- quantile(rev.samps[g, ], probs = c(conf.level / 2, 1 - conf.level / 2))
  }
  
  posterior.mean <- rowMeans(rev.samps)
  prob.winning <- rowMeans(rev.samps == matrix(rep(apply(rev.samps, FUN = max, MARGIN = 2), ngroups),
                                               nrow = ngroups, byrow = TRUE)) # [P(group 1 is best), P(group 2 is best) ...]
  
  #########################
  # visualization
  posterior.samples <- data.frame(mean.revenue = as.numeric(t(rev.samps)),
                                  group = as.factor(rep(groups, each = nsim)))
  
  if (save.hist) {
    hist.data <- data.frame(group = NA, bin = NA, density = NA)
    for (g in groups) {
      h <- hist(posterior.samples[posterior.samples[, 'group'] == g, 'mean.revenue'], plot = FALSE)
      hist.data <- rbind(hist.data, data.frame(group = g, bin = h$mids, density = h$density))
    }
  }
  hist.data <- hist.data[-1, ]
  
  # stopping rule
  risk <- rep(NA_real_, ngroups)
  for (g in 1:ngroups) {
    loss <- apply(rev.samps, FUN = max, MARGIN = 2) - rev.samps[g, ] #difference between the MAP mean and the group that is actually the best
    risk[g]  <- mean(loss)
  }
  
  return(structure(
    list(
      risk = risk,
      winner = groups[risk < tolerance],
      stop.test = min(risk) < tolerance,
      tolerance = tolerance,
      prob.winning = prob.winning,
      posterior.mean = posterior.mean,
      ci = ci,
      conf.level = conf.level,
      hist.data = hist.data,
      n = n,
      sample.mean = sample.mean,
      groups = groups,
      nsim = nsim,
      rev.samps = rev.samps
    ),
    class = "lognormal_ab_test"
  ))
}
