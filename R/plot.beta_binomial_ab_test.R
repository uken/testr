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
#' plot(bb)
#' plot(bb, limits=c(0,.3), labels=c('A', 'B'), title='AB Test: Signups')


plot.beta_binomial_ab_test <- function(x,
                                       limits = c(0, 1),
                                       labels = NULL,
                                       title = "") {
  ngroups <- length(x$y)
  
  d <- expand.grid(group = seq(1, ngroups, 1),
                   input = seq(0, 1, 0.001))
  d$output <- mapply(FUN = dbeta,
                     x = d$input,
                     shape1 = x$posterior_parameters$alpha,
                     shape2 = x$posterior_parameters$beta)
  
  if(is.null(labels)) {
    labels <- 1:ngroups
  }
  
  print(
    ggplot2::ggplot(d,
                    ggplot2::aes(x = input,
                                 y = output,
                                 colour = factor(group))) +
      ggplot2::geom_line() +
      ggplot2::xlab("Conversion Rate") +
      ggplot2::ylab("Density") +
      ggplot2::ggtitle(title) +
      ggplot2::scale_colour_discrete(name = "Variant(s)",
                                     labels = labels) +
      ggplot2::scale_x_continuous(labels = scales::percent_format()) +
      ggplot2::coord_cartesian(xlim = limits) +
      ggplot2::theme_bw(base_size = 15) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 15))
  )
}