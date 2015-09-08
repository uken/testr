#' Visualize the Beta prior distribution for a Beta-Binomial A/B test
#' 
#' @param alpha0 first shape parameter for beta prior. Increasing alpha0 reduces uncertainty about expected_conversion_rate.
#' @param beta0 second shape parameter for beta prior. Ignored if expected_conversion_rate is given.
#' @param expected_conversion_rate before seeing the data, what is is the most likely conversion rate (i.e. mode of the beta prior)? From 0 to 1.
#' @examples plot_conversion_prior(alpha0=9, beta0=19)
#' 
#' @seealso \code{\link{plot_revenue_prior}} to plot all components of the prior distribution for a Lognormal A/B test
#' @seealso \code{\link{plot.beta_binomial_ab_test}} to plot the results of a Beta-Binomial A/B test 
#' @seealso \code{\link{beta_binomial_ab_test}} to run a Beta-Binomial A/B test (i.e. for conversion rate)

plot_conversion_prior <- function(expected_conversion_rate = NA,
                                  alpha0 = NA,
                                  beta0 = NA,
                                  plot = T,
                                  n = 101,
                                  return.graph = F) {

  if (!is.na(expected_conversion_rate) & expected_conversion_rate == 0) {
    stop('expected_conversion_rate must be strictly positive!')
  }

  if (sum(is.na(c(alpha0,beta0,expected_conversion_rate))) !=1) {
    stop('Specify exactly two of {expected_conversion_rate, alpha0, beta0}')
  }

  #compute beta0 if needed
  if (!is.na(expected_conversion_rate)) {
    beta0 <- 2 - alpha0 + (alpha0 - 1)/expected_conversion_rate
  }

  if (plot==T){
    graph <- curve(dbeta(x, shape1= alpha0, shape2= beta0), n=n)

    if (return.graph)
      return(data.frame(x= graph$x, y= graph$y))

  } else {

    x <- seq(0, 1, length.out = n)
    y <- dbeta(x, shape1 = alpha0, shape2 = beta0)

    return(
      data.frame(x = x,
                 y = y))
  }
}
