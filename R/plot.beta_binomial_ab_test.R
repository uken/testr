#' Plot results from a beta_binomial_ab_test object
#' 
#' Plot posterior distributions for a Beta Binomial A/B test using ggplot2
#'  @param x object of class beta_binomial_ab_test
#'  @param limits vector with (min,max) x-axis values
#'  @param labels vector with group names
#'  @param title graph title
#'  
#'  @return object of class ggplot2
#'  @examples bb <- beta_binomial_ab_test(y=c(100,120), n=c(1000, 999), expected_conversion_rate=.1, alpha0=.1) 
#'  plot(bb)
#'  plot(bb, limits=c(0,.3), labels=c('A', 'B'), title='AB Test: Signups')
#'  @seealso \code{\link{beta_binomial_ab_test}} to run a Beta-Binomial A/B test (i.e. for conversion rate)
#'  @seealso \code{\link{plot_conversion_prior}} to plot the beta prior given its parameters

plot.beta_binomial_ab_test <- function(x,
                                       limits = c(0, 1),
                                       labels = NULL,
                                       title = "") {

  d <- expand.grid(groups = x$groups,
                   input = seq(0, 1, 0.001))
  d$output <- mapply(FUN = dbeta,
                     x = d$input,
                     shape1 = x$posterior_parameters$alpha,
                     shape2 = x$posterior_parameters$beta)


  print(
    ggplot2::ggplot(d,
                    ggplot2::aes(x = input,
                                 y = output,
                                 colour = factor(groups))) +
      ggplot2::geom_line() +
      ggplot2::xlab("Conversion Rate") +
      ggplot2::ylab("Density") +
      ggplot2::ggtitle(title) +
      ggplot2::scale_colour_discrete(name = "Variant(s)") +
      ggplot2::scale_x_continuous(labels = scales::percent_format()) +
      ggplot2::coord_cartesian(xlim = limits) +
      ggplot2::theme_bw(base_size = 15) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 15))
  )
}