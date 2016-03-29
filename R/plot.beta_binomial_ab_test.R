#' Plot results from a beta_binomial_ab_test object
#'
#' Plot posterior distributions for a Beta Binomial A/B test using ggplot2
#'
#' @param x object of class beta_binomial_ab_test
#' @param limits vector with (min,max) x-axis values
#' @param labels vector with group names
#' @param title graph title
#' @param ... Additional arguments to pass on to \code{geom_line()}
#'
#' @return object of class ggplot2
#' @examples
#'  bb <- beta_binomial_ab_test(y = c(100, 120), n = c(1000, 999),
#'                              expected_conversion_rate = 0.1, alpha0 = 0.1)
#'  plot(bb)
#'  plot(bb, limits = c(0, 0.3), labels = c('A', 'B'), title = 'AB Test: Signups')
#'  g <- plot(bb)
#'  g + ggplot2::theme_bw()
#' @seealso \code{\link{beta_binomial_ab_test}} to run a Beta-Binomial A/B test
#'   (i.e. for conversion rate)
#' @seealso \code{\link{plot_conversion_prior}} to plot the beta prior given
#'   its parameters
#' @importFrom scales percent_format
#' @export

plot.beta_binomial_ab_test <- function(x, limits = c(0, 1), labels = NULL, title = NULL, ...) {
  d <- expand.grid(groups = x$groups, input = seq(limits[1], limits[2], 0.001))
  d$output <- mapply(FUN = dbeta, x = d$input,
                     shape1 = x$posterior_parameters$alpha,
                     shape2 = x$posterior_parameters$beta
  )
  if (is.null(labels)) {
    labels <- x$groups
  }
  gg <- ggplot2::ggplot(d, ggplot2::aes(x = input, y = output, colour = factor(groups))) +
    ggplot2::geom_line(...) +
    ggplot2::scale_color_brewer(name = ifelse(length(x$groups) > 1, "Variants", "Variant"),
                                labels = labels, type = "qual", palette = "Set1") +
    ggplot2::scale_x_continuous(labels = percent_format(), limits = limits) +
    ggplot2::labs(x = "Conversion Rate", y = "Density", title = title)
  return(gg)
}
